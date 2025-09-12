#!/usr/bin/env Rscript
# metar_fetch_awc.R
# -------------------------------------------------------------------
# Fetch decoded METARs via AviationWeather.gov for all Canadian airports
# (large/medium/small) and U.S. airports (large/medium only).
# Saves into:
#   • Data/METARs/all_metars.csv          (master, append-only)
#   • Data/METARs/all_metars_MM_YYYY.csv  (monthly, append-only)
#   • Data/METARs/metar_log.txt           (log file)
#
# IMPORTANT: To avoid breaking existing consumers of all_metars.csv,
# we KEEP all existing columns and their meanings/positions.
# New timezone-aware fields are appended AT THE END:
#   iana_tz, windows_tz, observed_local_iana, local_date_iana, local_hour_iana
# (observed_local remains as before — Toronto time from earlier workflow)
# -------------------------------------------------------------------

# ---------------- File Paths --------------------
DATA_DIR <- "Data/METARs"
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

MASTER_FILE <- file.path(DATA_DIR, "all_metars.csv")
LOG_FILE    <- file.path(DATA_DIR, "metar_log.txt")

# ---------------- Packages ----------------------
need <- c("httr","jsonlite","tibble","dplyr","lubridate","readr","stringr")
missing <- need[!(need %in% rownames(installed.packages()))]
if(length(missing)) install.packages(missing, repos="https://cloud.r-project.org", quiet=TRUE)
suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(tibble)
  library(dplyr); library(lubridate); library(readr); library(stringr)
})

# ---------------- Helpers -----------------------
`%||%` <- function(a,b) if(!is.null(a)) a else b
chunk_vec <- function(x, size) split(x, ceiling(seq_along(x)/size))

normalize_row <- function(r){
  obs_utc <- suppressWarnings(ymd_hms(r$obsTime, tz="UTC"))
  tibble(
    icao            = r$id,
    site            = r$site %||% NA_character_,
    observed_utc    = obs_utc,                                            # POSIXct (UTC)
    observed_local  = if(!is.na(obs_utc)) with_tz(obs_utc,"America/Toronto") else NA, # (legacy)
    flight_category = r$fltcat %||% NA_character_,
    temp_c          = suppressWarnings(as.numeric(r$temp)),
    dewpoint_c      = suppressWarnings(as.numeric(r$dewp)),
    wind_dir_deg    = suppressWarnings(as.numeric(r$wdir)),
    wind_kts        = suppressWarnings(as.numeric(r$wspd)),
    gust_kts        = suppressWarnings(as.numeric(r$wgst)),
    vis_sm          = suppressWarnings(as.numeric(r$visib)),
    altimeter_hpa   = suppressWarnings(as.numeric(r$altim)),
    raw_text        = r$rawOb %||% NA_character_,
    fetched_at_utc  = Sys.time(),
    fetched_at_local= with_tz(Sys.time(),"America/Toronto")
  )
}

extract_props_df <- function(parsed){
  # Case A: nested properties
  if (!is.null(parsed$features) && !is.null(parsed$features$properties)) {
    props <- parsed$features$properties
    if (is.data.frame(props)) return(props)
    if (is.list(props)) {
      rows <- lapply(seq_along(props), function(i) as.data.frame(props[[i]], stringsAsFactors = FALSE))
      return(bind_rows(rows))
    }
  }
  # Case B: flattened with properties.* cols
  if (is.data.frame(parsed$features)) {
    feats <- parsed$features
    prop_cols <- grep("^properties\\.", names(feats), value = TRUE)
    if (length(prop_cols) > 0) {
      props <- feats[, prop_cols, drop = FALSE]
      names(props) <- sub("^properties\\.", "", names(props))
      return(props)
    }
  }
  NULL
}

get_metars_chunk <- function(stns){
  url <- paste0("https://aviationweather.gov/api/data/metar?format=geojson&ids=",
                paste(stns, collapse=","))
  message("Fetching: ", url)
  res <- GET(url, user_agent("CFCRP/1.0 (contact: your-email@example.com)"))
  if (http_error(res)) {
    warning("Request failed with status ", status_code(res))
    return(NULL)
  }
  txt <- content(res, "text", encoding="UTF-8")
  parsed <- tryCatch(fromJSON(txt, flatten = TRUE), error=function(e) NULL)
  if (is.null(parsed) || is.null(parsed$features) || length(parsed$features) == 0) {
    message("No features for this chunk.")
    return(NULL)
  }
  props <- extract_props_df(parsed)
  if (is.null(props) || nrow(props) == 0) {
    message("No properties rows found.")
    return(NULL)
  }
  rows <- lapply(seq_len(nrow(props)), function(i) as.list(props[i, , drop=FALSE]))
  df <- bind_rows(lapply(rows, normalize_row))
  message("Chunk returned ", nrow(df), " rows.")
  df
}

fetch_all <- function(stns){
  chunks <- chunk_vec(stns, 100)  # safe chunk size
  dfs <- list()
  for (i in seq_along(chunks)) {
    df <- get_metars_chunk(chunks[[i]])
    if (!is.null(df) && nrow(df) > 0) dfs[[length(dfs)+1]] <- df
    Sys.sleep(1)  # be polite
  }
  if (length(dfs)) bind_rows(dfs) else NULL
}

# Align df columns to an existing CSV header (drop extras / add missing NA) for safe append
align_to_existing_header <- function(path, df){
  hdr <- tryCatch(names(suppressMessages(read_csv(path, n_max = 0, show_col_types = FALSE))),
                  error = function(e) NULL)
  if (is.null(hdr) || !length(hdr)) return(df)  # nothing to align
  # Add any missing columns that exist in header but not in df
  missing_in_df <- setdiff(hdr, names(df))
  if (length(missing_in_df)) {
    for (nm in missing_in_df) df[[nm]] <- NA
  }
  # Reorder and drop extras to match existing header exactly
  df[hdr]
}

write_csvs <- function(df, master){
  now_local <- with_tz(Sys.time(), "America/Toronto")
  month_file <- file.path(DATA_DIR,
                          sprintf("all_metars_%s.csv", format(now_local, "%m_%Y")))

  # If the master already exists, align to its header so we don't change schema mid-file
  out_master <- if (file.exists(master)) align_to_existing_header(master, df) else df

  if (file.exists(master)) {
    write.table(out_master, master, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  } else {
    write.csv(out_master, master, row.names=FALSE)
  }

  # Monthly file: if exists align; if new, write full (including new columns at end)
  out_month <- if (file.exists(month_file)) align_to_existing_header(month_file, df) else df

  if (file.exists(month_file)) {
    write.table(out_month, month_file, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  } else {
    write.csv(out_month, month_file, row.names=FALSE)
  }

  cat(sprintf("[%s] Added %d rows (master hdr cols: %s)\n",
              format(now_local, "%Y-%m-%d %H:%M:%S %Z"),
              nrow(df),
              if (file.exists(master)) length(names(out_master)) else length(names(df))),
      file = LOG_FILE, append = TRUE)
}

# ---------------- Build STATIONS dynamically --------------------
message("Fetching airport list from GitHub mirror of OurAirports ...")
airports <- read_csv(
  "https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv",
  show_col_types = FALSE
)

# Step 1: fetch ALL airports in CA/US (include iata_code for tz join)
stations_df <- airports %>%
  filter(iso_country %in% c("CA","US")) %>%
  filter(!is.na(ident), str_length(ident) == 4) %>%
  filter(grepl("^(C|K|P)", ident)) %>%
  distinct(ident, iso_country, type, iata_code, name, latitude_deg, longitude_deg)

# Step 2: filter OUT small_airport in US (keep small in Canada)
stations_df <- stations_df %>%
  filter(!(iso_country == "US" & type == "small_airport"))

STATIONS <- stations_df$ident

# ---------------- Time Zone Map (IATA -> IANA/Windows) ----------------
# Matt Johnson-Pint gist (timezones.csv)
tz_gist_csv <- "https://gist.githubusercontent.com/mattjohnsonpint/6d219c48697c550c2476/raw/timezones.csv"

norm_tz_cols <- function(df) {
  # Normalize possible header variants
  rename_map <- list(
    iata_code = intersect(c("iata_code","iata","IATA"), names(df))[1],
    iana_tz   = intersect(c("iana_tz","iana","ianaTimeZone","iana_timezone"), names(df))[1],
    windows_tz= intersect(c("windows_tz","windows","windowsTimeZone","windows_timezone"), names(df))[1]
  )
  for (nm in names(rename_map)) {
    src <- rename_map[[nm]]
    if (!is.null(src) && !is.na(src) && src != nm) {
      names(df)[names(df) == src] <- nm
    }
  }
  df
}

tz_map <- tryCatch(
  {
    tzdf <- suppressMessages(read_csv(tz_gist_csv, show_col_types = FALSE))
    norm_tz_cols(tzdf) %>%
      mutate(iata_code = na_if(str_trim(iata_code), "")) %>%
      select(iata_code, iana_tz, windows_tz)
  },
  error = function(e) {
    message("⚠️ Could not read timezones.csv gist: ", conditionMessage(e))
    tibble(iata_code = character(), iana_tz = character(), windows_tz = character())
  }
)

# Precompute ICAO -> tz mapping from OurAirports iata_code
icao_tz <- stations_df %>%
  select(icao = ident, iata_code) %>%
  left_join(tz_map, by = "iata_code")

# ---------------- Main --------------------------
ca_count <- sum(stations_df$iso_country == "CA")
us_count <- sum(stations_df$iso_country == "US")
message("Canadian airports included: ", ca_count)
message("U.S. airports included: ", us_count)
message("Total airports in STATIONS: ", length(STATIONS))

res <- fetch_all(STATIONS)

if (!is.null(res) && nrow(res) > 0) {
  # Attach time zone mapping
  res <- res %>%
    left_join(icao_tz, by = "icao")

  # Compute per-row local time (cannot store mixed tz in one POSIXct vector),
  # so we keep observed_local (legacy) and APPEND new fields at the end.
  local_list <- mapply(function(dt, tz){
    if (is.na(dt)) return(NA)
    if (is.na(tz) || !nzchar(tz)) return(with_tz(dt, "UTC"))
    tryCatch(with_tz(dt, tz), error = function(e) with_tz(dt, "UTC"))
  }, res$observed_utc, res$iana_tz, SIMPLIFY = FALSE)

  get_char <- function(x, fmt) {
    if (length(x) == 0 || is.na(x)) return(NA_character_)
    format(x, fmt)
  }

  res <- res %>%
    mutate(
      # --- NEW FIELDS (appended; do NOT replace legacy observed_local) ---
      observed_local_iana = vapply(local_list, function(x) get_char(x, "%Y-%m-%d %H:%M:%S"), character(1)),
      local_date_iana     = as.Date(vapply(local_list, function(x) get_char(x, "%Y-%m-%d"), character(1))),
      local_hour_iana     = as.integer(vapply(local_list, function(x) get_char(x, "%H"), character(1)))
    )

  # Ensure new columns are at the END (preserve original order at the front)
  base_cols <- c(
    "icao","site","observed_utc","observed_local","flight_category","temp_c","dewpoint_c",
    "wind_dir_deg","wind_kts","gust_kts","vis_sm","altimeter_hpa","raw_text",
    "fetched_at_utc","fetched_at_local"
  )
  # Old files may not have had iana/windows; keep them with new block at end
  tz_cols   <- c("iana_tz","windows_tz","observed_local_iana","local_date_iana","local_hour_iana")
  # If any missing (e.g., tz_map failed), create them so column set is consistent
  for (nm in tz_cols) if (!nm %in% names(res)) res[[nm]] <- NA

  keep_order <- c(intersect(base_cols, names(res)), setdiff(names(res), c(base_cols, tz_cols)), tz_cols)
  res <- res[, keep_order, drop = FALSE]

  write_csvs(res, MASTER_FILE)

  # quick peek
  print(
    res %>%
      select(icao, observed_utc, observed_local, iana_tz, observed_local_iana, local_hour_iana) %>%
      head(10)
  )
} else {
  message("⚠️ No METARs returned")
}

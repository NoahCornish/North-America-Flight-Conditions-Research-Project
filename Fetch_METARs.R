#!/usr/bin/env Rscript
# metar_fetch_awc.R
# -------------------------------------------------------------------
# Fetch decoded METARs via AviationWeather.gov for all Canadian airports
# (large/medium/small) and U.S. airports (large/medium only).
# Appends rows to:
#   • Data/METARs/all_metars.csv
#   • Data/METARs/all_metars_MM_YYYY.csv
#   • Data/METARs/metar_log.txt
#
# IMPORTANT
# - Keeps the EXACT same columns/order as your current all_metars.csv.
# - Fixes observed_local: it is now the true station-local wall time (ISO string)
#   computed per-airport using IANA time zones (OurAirports → IATA → timezones.csv).
# - No new columns are added to the CSVs.
# -------------------------------------------------------------------

# ---------------- Paths ----------------
DATA_DIR <- "Data/METARs"
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
MASTER_FILE <- file.path(DATA_DIR, "all_metars.csv")
LOG_FILE    <- file.path(DATA_DIR, "metar_log.txt")

# ---------------- Packages ----------------
need <- c("httr","jsonlite","tibble","dplyr","lubridate","readr","stringr")
missing <- need[!(need %in% rownames(installed.packages()))]
if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org", quiet=TRUE)
suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(tibble)
  library(dplyr); library(lubridate); library(readr); library(stringr)
})

# ---------------- Helpers ----------------
`%||%` <- function(a,b) if(!is.null(a)) a else b
chunk_vec <- function(x, size = 100L) split(x, ceiling(seq_along(x)/size))

normalize_row <- function(r){
  obs_utc <- suppressWarnings(ymd_hms(r$obsTime, tz="UTC"))
  tibble(
    icao            = r$id,
    site            = r$site %||% NA_character_,
    observed_utc    = obs_utc,                          # POSIXct (UTC)
    observed_local  = if (!is.na(obs_utc)) with_tz(obs_utc,"UTC") else NA,  # placeholder; replaced later with station-local (string)
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
  res <- GET(url, user_agent("CFCRP/1.0 (contact: youremail@example.com)"))
  if (http_error(res)) {
    warning("Request failed with status ", status_code(res))
    return(NULL)
  }
  txt <- content(res, "text", encoding="UTF-8")
  parsed <- tryCatch(fromJSON(txt, flatten = TRUE), error=function(e) NULL)
  if (is.null(parsed) || is.null(parsed$features) || length(parsed$features) == 0) return(NULL)
  props <- extract_props_df(parsed)
  if (is.null(props) || nrow(props) == 0) return(NULL)
  rows <- lapply(seq_len(nrow(props)), function(i) as.list(props[i, , drop=FALSE]))
  df <- bind_rows(lapply(rows, normalize_row))
  df
}

fetch_all <- function(stns){
  chunks <- chunk_vec(unique(stns), 150)
  dfs <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    df <- get_metars_chunk(chunks[[i]])
    if (!is.null(df) && nrow(df) > 0) dfs[[i]] <- df
    Sys.sleep(1)  # be polite
  }
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs)) bind_rows(dfs) else NULL
}

# Align output to an existing CSV header (safe append without schema drift)
align_to_existing_header <- function(path, df){
  hdr <- tryCatch(names(suppressMessages(read_csv(path, n_max = 0, show_col_types = FALSE))),
                  error = function(e) NULL)
  if (is.null(hdr) || !length(hdr)) return(df)
  miss <- setdiff(hdr, names(df))
  if (length(miss)) for (nm in miss) df[[nm]] <- NA
  df <- df[hdr]
  df
}

write_csvs <- function(df, master){
  now_utc <- with_tz(Sys.time(), "UTC")  # use UTC for stable filenames/logs at month boundaries
  month_file <- file.path(DATA_DIR, sprintf("all_metars_%s.csv", format(now_utc, "%m_%Y")))

  out_master <- if (file.exists(master)) align_to_existing_header(master, df) else df
  if (file.exists(master)) {
    write.table(out_master, master, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE, fileEncoding="UTF-8")
  } else {
    write.csv(out_master, file = master, row.names=FALSE, fileEncoding="UTF-8")
  }

  out_month <- if (file.exists(month_file)) align_to_existing_header(month_file, df) else df
  if (file.exists(month_file)) {
    write.table(out_month, month_file, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE, fileEncoding="UTF-8")
  } else {
    write.csv(out_month, file = month_file, row.names=FALSE, fileEncoding="UTF-8")
  }

  cat(sprintf("[%s] Added %d rows\n",
              format(now_utc, "%Y-%m-%d %H:%M:%S %Z"), nrow(df)),
      file = LOG_FILE, append = TRUE)
}

# ---------------- Build STATIONS (OurAirports) ----------------
message("Fetching airport list from OurAirports …")
airports <- suppressMessages(read_csv(
  "https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv",
  show_col_types = FALSE
))

stations_df <- airports %>%
  filter(iso_country %in% c("CA","US")) %>%
  filter(!is.na(ident), str_length(ident) == 4) %>%
  filter(grepl("^(C|K|P)", ident)) %>%
  distinct(ident, iso_country, type, iata_code)

# US: drop small_airport; keep small in CA
#stations_df <- stations_df %>%
#  filter(!(iso_country == "US" & type == "small_airport"))

STATIONS <- stations_df$ident
message("Canadian airports included: ", sum(stations_df$iso_country == "CA"))
message("U.S. airports included: ", sum(stations_df$iso_country == "US"))
message("Total airports in STATIONS: ", length(STATIONS))

# ---------------- Time Zone Map (IATA -> IANA) ----------------
tz_gist_csv <- "https://gist.githubusercontent.com/mattjohnsonpint/6d219c48697c550c2476/raw/timezones.csv"

norm_tz_cols <- function(df){
  # Normalize potential header variants from the gist
  if (!"iata_code" %in% names(df)) {
    cand <- intersect(c("iata","IATA"), names(df)); if (length(cand)) names(df)[names(df)==cand[1]] <- "iata_code"
  }
  if (!"iana_tz" %in% names(df)) {
    cand <- grep("iana", names(df), ignore.case = TRUE, value = TRUE)
    if (length(cand)) names(df)[names(df)==cand[1]] <- "iana_tz"
  }
  df
}

tz_map <- tryCatch({
  tzdf <- suppressMessages(read_csv(tz_gist_csv, show_col_types = FALSE))
  tzdf <- norm_tz_cols(tzdf)
  tzdf %>%
    mutate(iata_code = na_if(str_trim(iata_code), "")) %>%
    select(iata_code, iana_tz) %>%
    distinct()
}, error = function(e){
  message("⚠️ Could not read timezones.csv gist: ", conditionMessage(e))
  tibble(iata_code = character(), iana_tz = character())
})

icao_tz <- stations_df %>%
  select(icao = ident, iata_code) %>%
  left_join(tz_map, by = "iata_code")

# ---------------- Main ----------------
res <- fetch_all(STATIONS)

if (!is.null(res) && nrow(res) > 0) {

  # Join IANA tz per ICAO
  res <- res %>% left_join(icao_tz, by = "icao")

  # Compute per-row local wall time and **store as ISO string** in observed_local
  local_list <- mapply(function(dt, tz){
    if (is.na(dt)) return(NA)
    if (is.na(tz) || !nzchar(tz)) return(with_tz(dt, "UTC"))  # fallback: UTC
    tryCatch(with_tz(dt, tz), error = function(e) with_tz(dt, "UTC"))
  }, res$observed_utc, res$iana_tz, SIMPLIFY = FALSE)

  res$observed_local <- vapply(local_list, function(x){
    if (length(x) == 0 || is.na(x)) return(NA_character_)
    format(x, "%Y-%m-%d %H:%M:%S")
  }, character(1))

  # DROP helper tz column(s) so schema matches existing CSV exactly
  res <- res %>%
    select(
      icao, site, observed_utc, observed_local,
      flight_category, temp_c, dewpoint_c,
      wind_dir_deg, wind_kts, gust_kts, vis_sm, altimeter_hpa,
      raw_text, fetched_at_utc, fetched_at_local
    )

  write_csvs(res, MASTER_FILE)

  # quick peek
  print(
    res %>%
      filter(icao %in% c("KMSP","KJFK","KLAX","CYYZ","CYVR")) %>%
      select(icao, observed_utc, observed_local) %>%
      head(10)
  )

} else {
  message("⚠️ No METARs returned")
}

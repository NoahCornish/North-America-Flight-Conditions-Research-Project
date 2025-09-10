#!/usr/bin/env Rscript
# metar_fetch_awc.R
# -------------------------------------------------------------------
# Fetch decoded METARs via AviationWeather.gov for *all Canadian & US
# large + medium airports that issue METARs*. Save into:
#   • Data/METARs/all_metars.csv          (master, append-only)
#   • Data/METARs/all_metars_MM_YYYY.csv  (monthly, append-only)
#   • Data/METARs/metar_log.txt           (log file)
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
    observed_utc    = obs_utc,
    observed_local  = if(!is.na(obs_utc)) with_tz(obs_utc,"America/Toronto") else NA,
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
  chunks <- chunk_vec(stns, 200)  # safe chunk size
  dfs <- list()
  for (i in seq_along(chunks)) {
    df <- get_metars_chunk(chunks[[i]])
    if (!is.null(df) && nrow(df) > 0) dfs[[length(dfs)+1]] <- df
    Sys.sleep(1)  # be polite
  }
  if (length(dfs)) bind_rows(dfs) else NULL
}

write_csvs <- function(df, master){
  now_local <- with_tz(Sys.time(), "America/Toronto")
  month_file <- file.path(DATA_DIR,
                          sprintf("all_metars_%s.csv", format(now_local, "%m_%Y")))
  
  if (file.exists(master)) {
    write.table(df, master, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  } else {
    write.csv(df, master, row.names=FALSE)
  }
  if (file.exists(month_file)) {
    write.table(df, month_file, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  } else {
    write.csv(df, month_file, row.names=FALSE)
  }
  
  cat(sprintf("[%s] Added %d rows\n",
              format(now_local, "%Y-%m-%d %H:%M:%S %Z"),
              nrow(df)),
      file = LOG_FILE, append = TRUE)
}

# ---------------- Build STATIONS dynamically --------------------
message("Fetching airport list from GitHub mirror of OurAirports ...")
airports <- read_csv(
  "https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv",
  show_col_types = FALSE
)

stations_df <- airports %>%
  filter(iso_country %in% c("CA","US")) %>%
  filter(
    # Large & medium everywhere
    (type %in% c("large_airport","medium_airport")) |
      # Small only in Canada
      (iso_country == "CA" & type == "small_airport")
  ) %>%
  filter(!is.na(ident), str_length(ident) == 4) %>%
  filter(grepl("^(C|K|P)", ident)) %>%
  distinct(ident)


STATIONS <- stations_df$ident
message("Total airports in STATIONS: ", length(STATIONS))

# ---------------- Main --------------------------
res <- fetch_all(STATIONS)

if (!is.null(res) && nrow(res) > 0) {
  write_csvs(res, MASTER_FILE)
  print(head(res, 10))
} else {
  message("⚠️ No METARs returned")
}

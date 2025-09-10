#!/usr/bin/env Rscript

# Deduplicate METAR rows by ICAO timeline and enrich with lat/lon.
# Keep a row if raw_text changed OR any of these changed:
# flight_category, temp_c, dewpoint_c, wind_dir_deg, wind_kts, gust_kts, vis_sm, altimeter_hpa
# Special handling:
# - wind_dir_deg: if NA → "VAR"
# - gust_kts: if NA → 0
# Adds site_lat / site_lon from OurAirports.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(optparse)
})

# ------------------- CLI options -------------------
opt_list <- list(
  make_option(c("-i","--input"), type="character", default=NULL,
              help="Path or URL to master CSV (defaults to Data/METARs/all_metars.csv)")
)
opt <- parse_args(OptionParser(option_list = opt_list))

DATA_DIR   <- "Data/METARs"
MASTER_CSV <- if (!is.null(opt$input)) opt$input else file.path(DATA_DIR, "all_metars.csv")
CLEAN_CSV  <- file.path(DATA_DIR, "clean_metars.csv")
LOG_FILE   <- file.path(DATA_DIR, "clean_log.txt")

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

message("Reading master: ", MASTER_CSV)

# ------------------- Read -------------------
raw_df <- suppressMessages(read_csv(MASTER_CSV, guess_max = 500000))

if (nrow(raw_df) == 0) {
  message("No rows found in master. Exiting.")
  quit(status = 0)
}

# ------------------- Normalize columns -------------------
norm_name <- function(df, candidates, default = NULL) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[1] else default
}

col_icao   <- norm_name(raw_df, c("icao","station_id","id","icaoId"), "icao")
col_obs    <- norm_name(raw_df, c("observed_utc","obsTime","observation_time"), "observed_utc")
col_raw    <- norm_name(raw_df, c("raw_text","rawOb","raw"), "raw_text")

needed <- c(col_icao, col_obs, col_raw,
            "flight_category","temp_c","dewpoint_c","wind_dir_deg",
            "wind_kts","gust_kts","vis_sm","altimeter_hpa")
missing <- setdiff(needed, names(raw_df))
if (length(missing)) {
  stop("Missing required columns in input: ", paste(missing, collapse = ", "))
}

df <- raw_df %>%
  rename(
    icao        = !!col_icao,
    observed_utc= !!col_obs,
    raw_text    = !!col_raw
  ) %>%
  mutate(
    observed_utc = suppressWarnings(ymd_hms(observed_utc, tz = "UTC")),
    temp_c        = suppressWarnings(as.numeric(temp_c)),
    dewpoint_c    = suppressWarnings(as.numeric(dewpoint_c)),
    wind_dir_deg  = suppressWarnings(as.numeric(wind_dir_deg)),
    wind_kts      = suppressWarnings(as.numeric(wind_kts)),
    gust_kts      = suppressWarnings(as.numeric(gust_kts)),
    vis_sm        = suppressWarnings(as.numeric(vis_sm)),
    altimeter_hpa = suppressWarnings(as.numeric(altimeter_hpa))
  ) %>%
  # Handle NA replacements
  mutate(
    wind_dir_deg = ifelse(is.na(wind_dir_deg), "VAR", as.character(wind_dir_deg)),
    gust_kts     = ifelse(is.na(gust_kts), 0, gust_kts)
  ) %>%
  filter(!is.na(icao), !is.na(observed_utc)) %>%
  arrange(icao, observed_utc)

# ------------------- Change detection -------------------
differs <- function(curr, prev) {
  if (length(prev) == 0) return(rep(TRUE, length(curr)))
  (is.na(curr) != is.na(prev)) | (curr != prev)
}

clean_df <- df %>%
  group_by(icao) %>%
  arrange(observed_utc, .by_group = TRUE) %>%
  mutate(
    changed_raw    = differs(raw_text,        lag(raw_text)),
    changed_cat    = differs(flight_category, lag(flight_category)),
    changed_temp   = differs(temp_c,          lag(temp_c)),
    changed_dew    = differs(dewpoint_c,      lag(dewpoint_c)),
    changed_wdir   = differs(wind_dir_deg,    lag(wind_dir_deg)),
    changed_wspd   = differs(wind_kts,        lag(wind_kts)),
    changed_gust   = differs(gust_kts,        lag(gust_kts)),
    changed_vis    = differs(vis_sm,          lag(vis_sm)),
    changed_alt    = differs(altimeter_hpa,   lag(altimeter_hpa)),
    keep_row = if_else(
      row_number() == 1L |
        changed_raw |
        changed_cat | changed_temp | changed_dew |
        changed_wdir | changed_wspd | changed_gust |
        changed_vis | changed_alt,
      TRUE, FALSE
    )
  ) %>%
  ungroup() %>%
  filter(keep_row) %>%
  select(-starts_with("changed_"), -keep_row)

# ------------------- Enrich with lat/lon -------------------
message("Fetching OurAirports data for coordinates...")
airports <- read_csv(
  "https://raw.githubusercontent.com/davidmegginson/ourairports-data/main/airports.csv",
  show_col_types = FALSE
) %>%
  select(ident, name, latitude_deg, longitude_deg, iso_country, iso_region)

clean_df <- clean_df %>%
  left_join(airports, by = c("icao" = "ident")) %>%
  rename(
    site_name = name,
    site_lat  = latitude_deg,
    site_lon  = longitude_deg,
    country   = iso_country,
    region    = iso_region
  )

# ------------------- Write outputs -------------------
now_local <- with_tz(Sys.time(), "America/Toronto")
month_out <- file.path(DATA_DIR, sprintf("clean_metars_%s.csv", format(now_local, "%m_%Y")))

message("Writing: ", CLEAN_CSV)
write_csv(clean_df, CLEAN_CSV)

message("Writing: ", month_out)
write_csv(clean_df, month_out)

added <- nrow(clean_df)
orig  <- nrow(df)
dupes <- orig - added

cat(sprintf("[%s] Cleaned METARs — input: %d rows, kept: %d rows, dropped: %d rows, enriched: %d rows\n",
            format(now_local, "%Y-%m-%d %H:%M:%S %Z"), orig, added, dupes, added),
    file = LOG_FILE, append = TRUE)

message("Done. Kept ", added, " / ", orig, " rows (", dupes, " dropped). Enriched with lat/lon.")

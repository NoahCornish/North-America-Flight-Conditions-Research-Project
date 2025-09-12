#!/usr/bin/env Rscript
# Clean_METARs.R
# -------------------------------------------------------------------
# Deduplicate METAR rows by ICAO timeline and enrich with lat/lon.
# Recomputes observed_local per row from observed_utc using station IANA TZ
# (OurAirports -> IATA -> Matt Johnson-Pint timezones.csv), fixing US rows
# that previously showed Eastern time. No new columns added—observed_local
# is overwritten in-place as a character "YYYY-MM-DD HH:MM:SS".
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(optparse)
})

# -------- CLI --------
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

# -------- Read master --------
raw_df <- suppressMessages(read_csv(MASTER_CSV, guess_max = 750000, show_col_types = FALSE))
if (nrow(raw_df) == 0) {
  message("No rows found in master. Exiting.")
  quit(status = 0)
}

# -------- Normalize column names we need --------
norm_name <- function(df, candidates, default = NULL) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[1] else default
}
col_icao <- norm_name(raw_df, c("icao","station_id","id","icaoId"), "icao")
col_obs  <- norm_name(raw_df, c("observed_utc","obsTime","observation_time"), "observed_utc")
col_raw  <- norm_name(raw_df, c("raw_text","rawOb","raw"), "raw_text")
col_loc  <- norm_name(raw_df, c("observed_local","local_time","local"), "observed_local")  # will be overwritten

needed <- c(col_icao, col_obs, col_raw, col_loc,
            "flight_category","temp_c","dewpoint_c","wind_dir_deg",
            "wind_kts","gust_kts","vis_sm","altimeter_hpa",
            "site","fetched_at_utc","fetched_at_local")

missing <- setdiff(needed, names(raw_df))
if (length(missing)) {
  stop("Missing required columns in input: ", paste(missing, collapse = ", "))
}

df <- raw_df %>%
  rename(
    icao         = !!col_icao,
    observed_utc = !!col_obs,
    raw_text     = !!col_raw,
    observed_local = !!col_loc
  ) %>%
  mutate(
    observed_utc   = suppressWarnings(ymd_hms(observed_utc, tz = "UTC")),
    temp_c         = suppressWarnings(as.numeric(temp_c)),
    dewpoint_c     = suppressWarnings(as.numeric(dewpoint_c)),
    wind_dir_deg   = suppressWarnings(as.numeric(wind_dir_deg)),
    wind_kts       = suppressWarnings(as.numeric(wind_kts)),
    gust_kts       = suppressWarnings(as.numeric(gust_kts)),
    vis_sm         = suppressWarnings(as.numeric(vis_sm)),
    altimeter_hpa  = suppressWarnings(as.numeric(altimeter_hpa))
  ) %>%
  filter(!is.na(icao), !is.na(observed_utc)) %>%
  arrange(icao, observed_utc)

# -------- Recompute observed_local from IANA tz (fixes US) --------
airports_url <- "https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv"
tz_url       <- "https://gist.githubusercontent.com/mattjohnsonpint/6d219c48697c550c2476/raw/timezones.csv"

air <- suppressMessages(read_csv(airports_url, show_col_types = FALSE)) %>%
  select(ident, iata_code)

tzdf <- suppressMessages(read_csv(tz_url, show_col_types = FALSE))
# normalize header variants
if (!"iata_code" %in% names(tzdf)) {
  cand <- intersect(c("iata","IATA"), names(tzdf)); if (length(cand)) names(tzdf)[names(tzdf)==cand[1]] <- "iata_code"
}
if (!"iana_tz" %in% names(tzdf)) {
  cand <- grep("iana", names(tzdf), ignore.case = TRUE, value = TRUE)
  if (length(cand)) names(tzdf)[names(tzdf)==cand[1]] <- "iana_tz"
}
tzmap <- tzdf %>% select(iata_code, iana_tz) %>% mutate(iata_code = na_if(str_trim(iata_code), ""))

df <- df %>%
  left_join(air, by = c("icao" = "ident")) %>%
  left_join(tzmap, by = "iata_code")

# compute station-local as ISO string per row; fallback UTC when tz missing/bad
local_list <- mapply(function(dt, tz){
  if (is.na(dt)) return(NA)
  if (is.na(tz) || !nzchar(tz)) return(with_tz(dt, "UTC"))
  tryCatch(with_tz(dt, tz), error = function(e) with_tz(dt, "UTC"))
}, df$observed_utc, df$iana_tz, SIMPLIFY = FALSE)

df$observed_local <- vapply(local_list, function(x){
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  format(x, "%Y-%m-%d %H:%M:%S")
}, character(1))

# -------- Deduplicate by change (your original rules) --------
differs <- function(curr, prev) {
  if (length(prev) == 0) return(rep(TRUE, length(curr)))
  (is.na(curr) != is.na(prev)) | (curr != prev)
}

clean_df <- df %>%
  group_by(icao) %>%
  arrange(observed_utc, .by_group = TRUE) %>%
  mutate(
    changed_raw  = differs(raw_text,        lag(raw_text)),
    changed_cat  = differs(flight_category, lag(flight_category)),
    changed_temp = differs(temp_c,          lag(temp_c)),
    changed_dew  = differs(dewpoint_c,      lag(dewpoint_c)),
    changed_wdir = differs(wind_dir_deg,    lag(wind_dir_deg)),
    changed_wspd = differs(wind_kts,        lag(wind_kts)),
    changed_gust = differs(gust_kts,        lag(gust_kts)),
    changed_vis  = differs(vis_sm,          lag(vis_sm)),
    changed_alt  = differs(altimeter_hpa,   lag(altimeter_hpa)),
    keep_row = if_else(
      row_number() == 1L |
        changed_raw | changed_cat | changed_temp | changed_dew |
        changed_wdir | changed_wspd | changed_gust |
        changed_vis | changed_alt,
      TRUE, FALSE
    )
  ) %>%
  ungroup() %>%
  filter(keep_row) %>%
  select(-starts_with("changed_"), -keep_row, -iata_code, -iana_tz)

# -------- Handle NA replacements (as before) --------
clean_df <- clean_df %>%
  mutate(
    wind_dir_deg = ifelse(is.na(wind_dir_deg), "VAR", as.character(wind_dir_deg)),
    gust_kts     = ifelse(is.na(gust_kts), 0, gust_kts)
  )

# -------- Enrich with lat/lon (OurAirports) --------
airports_full <- suppressMessages(read_csv(airports_url, show_col_types = FALSE)) %>%
  select(ident, name, latitude_deg, longitude_deg, iso_country, iso_region)

clean_df <- clean_df %>%
  left_join(airports_full, by = c("icao" = "ident")) %>%
  rename(
    site_name = name,
    site_lat  = latitude_deg,
    site_lon  = longitude_deg,
    country   = iso_country,
    region    = iso_region
  )

# -------- Write outputs --------
now_local <- with_tz(Sys.time(), "America/Toronto")
month_out <- file.path(DATA_DIR, sprintf("clean_metars_%s.csv", format(now_local, "%m_%Y")))

# keep original column order up to observed_local, then the rest
front_cols <- c("icao","site","observed_utc","observed_local","flight_category",
                "temp_c","dewpoint_c","wind_dir_deg","wind_kts","gust_kts",
                "vis_sm","altimeter_hpa","raw_text","fetched_at_utc","fetched_at_local")
other_cols <- setdiff(names(clean_df), front_cols)
ordered <- clean_df %>% select(all_of(front_cols), all_of(other_cols))

message("Writing: ", CLEAN_CSV)
write_csv(ordered, CLEAN_CSV)

message("Writing: ", month_out)
write_csv(ordered, month_out)

added <- nrow(ordered)
orig  <- nrow(df)

cat(sprintf("[%s] Cleaned METARs — input: %d rows, kept: %d rows (recomputed observed_local per IANA tz)\n",
            format(now_local, "%Y-%m-%d %H:%M:%S %Z"), orig, added),
    file = LOG_FILE, append = TRUE)

message("Done. Kept ", added, " / ", orig, " rows. Local times fixed for US stations.")

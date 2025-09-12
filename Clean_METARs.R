#!/usr/bin/env Rscript
# Clean_METARs.R
# -------------------------------------------------------------------
# Deduplicate METAR rows by ICAO timeline and enrich with lat/lon.
# Recomputes observed_local per row from observed_utc using station IANA TZ
# (OurAirports -> IATA -> Matt Johnson-Pint timezones.csv).
# Writes:
#   • Data/METARs/clean_metars.csv (deduplicated, local times fixed)
#   • Data/METARs/clean_metars_MM_YYYY.csv (monthly snapshot)
#   • Data/METARs/airports_missing_flight_category.csv (running list)
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(optparse)
})

# ---------------- CLI Options ----------------
opt_list <- list(
  make_option(c("-i","--input"), type="character", default=NULL,
              help="Path or URL to master CSV (defaults to Data/METARs/all_metars.csv)")
)
opt <- parse_args(OptionParser(option_list = opt_list))

DATA_DIR   <- "Data/METARs"
MASTER_CSV <- if (!is.null(opt$input)) opt$input else file.path(DATA_DIR, "all_metars.csv")
CLEAN_CSV  <- file.path(DATA_DIR, "clean_metars.csv")
LOG_FILE   <- file.path(DATA_DIR, "clean_log.txt")
GREY_FILE  <- file.path(DATA_DIR, "airports_missing_flight_category.csv")

if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

message("Reading master: ", MASTER_CSV)

# ---------------- Read master ----------------
raw_df <- suppressMessages(read_csv(MASTER_CSV, guess_max = 750000, show_col_types = FALSE))
if (nrow(raw_df) == 0) {
  message("No rows found in master. Exiting.")
  quit(status = 0)
}

# ---------------- Normalize Columns ----------------
df <- raw_df %>%
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

# ---------------- Recompute observed_local ----------------
airports_url <- "https://raw.githubusercontent.com/davidmegginson/ourairports-data/refs/heads/main/airports.csv"
tz_url       <- "https://gist.githubusercontent.com/mattjohnsonpint/6d219c48697c550c2476/raw/timezones.csv"

air <- suppressMessages(read_csv(airports_url, show_col_types = FALSE)) %>%
  select(ident, iata_code)

tzdf <- suppressMessages(read_csv(tz_url, show_col_types = FALSE))
if (!"iata_code" %in% names(tzdf)) {
  cand <- intersect(c("iata","IATA"), names(tzdf))
  if (length(cand)) names(tzdf)[names(tzdf)==cand[1]] <- "iata_code"
}
if (!"iana_tz" %in% names(tzdf)) {
  cand <- grep("iana", names(tzdf), ignore.case = TRUE, value = TRUE)
  if (length(cand)) names(tzdf)[names(tzdf)==cand[1]] <- "iana_tz"
}
tzmap <- tzdf %>% select(iata_code, iana_tz) %>% mutate(iata_code = na_if(str_trim(iata_code), ""))

df <- df %>%
  left_join(air, by = c("icao" = "ident")) %>%
  left_join(tzmap, by = "iata_code")

local_list <- mapply(function(dt, tz){
  if (is.na(dt)) return(NA)
  if (is.na(tz) || !nzchar(tz)) return(with_tz(dt, "UTC"))
  tryCatch(with_tz(dt, tz), error = function(e) with_tz(dt, "UTC"))
}, df$observed_utc, df$iana_tz, SIMPLIFY = FALSE)

df$observed_local <- vapply(local_list, function(x){
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  format(x, "%Y-%m-%d %H:%M:%S")
}, character(1))

# ---------------- Save Grey-Airport List ----------------
grey_airports <- df %>%
  filter(is.na(flight_category) | flight_category == "") %>%
  distinct(icao) %>%
  arrange(icao)

if (file.exists(GREY_FILE)) {
  existing <- suppressMessages(read_csv(GREY_FILE, show_col_types = FALSE))
  grey_airports <- bind_rows(existing, grey_airports) %>% distinct(icao) %>% arrange(icao)
}
write_csv(grey_airports, GREY_FILE)
message("Grey-airport list written: ", GREY_FILE, " (", nrow(grey_airports), " ICAOs)")

# ---------------- Deduplicate ----------------
differs <- function(curr, prev) (is.na(curr) != is.na(prev)) | (curr != prev)

clean_df <- df %>%
  group_by(icao) %>%
  arrange(observed_utc, .by_group = TRUE) %>%
  mutate(
    keep_row = row_number() == 1L |
      differs(raw_text, lag(raw_text)) |
      differs(flight_category, lag(flight_category)) |
      differs(temp_c, lag(temp_c)) |
      differs(dewpoint_c, lag(dewpoint_c)) |
      differs(wind_dir_deg, lag(wind_dir_deg)) |
      differs(wind_kts, lag(wind_kts)) |
      differs(gust_kts, lag(gust_kts)) |
      differs(vis_sm, lag(vis_sm)) |
      differs(altimeter_hpa, lag(altimeter_hpa))
  ) %>%
  ungroup() %>%
  filter(keep_row) %>%
  select(-iata_code, -iana_tz, -keep_row)

# ---------------- Write Outputs ----------------
now_local <- with_tz(Sys.time(), "America/Toronto")
month_out <- file.path(DATA_DIR, sprintf("clean_metars_%s.csv", format(now_local, "%m_%Y")))

write_csv(clean_df, CLEAN_CSV)
write_csv(clean_df, month_out)

cat(sprintf("[%s] Cleaned METARs — input: %d rows, kept: %d rows (grey airports: %d)\n",
            format(now_local, "%Y-%m-%d %H:%M:%S %Z"), nrow(df), nrow(clean_df), nrow(grey_airports)),
    file = LOG_FILE, append = TRUE)

message("Done. Clean METAR file written with ", nrow(clean_df), " rows.")

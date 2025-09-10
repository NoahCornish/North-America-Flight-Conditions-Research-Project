#!/usr/bin/env Rscript
# -------------------------------------------------------------------
# METAR_Summaries.R
# Compute % time in VFR/MVFR/IFR/LIFR:
#   • per airport (ICAO, site, region, country)
#   • per region (province/state)
#   • per country ("Canada", "United States")
#   • overall (Global, all data combined)
#
# Input : Data/METARs/clean_metars.csv
# Output: Data/Summaries/airport_flight_share.csv
#         Data/Summaries/region_flight_share.csv
#         Data/Summaries/country_flight_share.csv
#         Data/Summaries/global_flight_share.csv
#         Data/Summaries/parse_diagnostics.csv
#         Data/Summaries/analysis_log.txt
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(here)   # ensures paths are repo-root anchored
})

# ---------------- Settings ----------------
DATA_DIR      <- here("Data")
INPUT_CSV     <- here("Data", "METARs", "clean_metars.csv")
OUT_DIR       <- here("Data", "Summaries")
LOG_FILE      <- file.path(OUT_DIR, "analysis_log.txt")
MAX_GAP_HOURS <- 3  # cap time between reports to avoid outage skew
ALL_CATS      <- c("VFR","MVFR","IFR","LIFR")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_msg <- function(...) {
  cat(paste0("[", format(with_tz(Sys.time(),"UTC"), "%Y-%m-%d %H:%M:%S %Z"), "] ",
             paste0(..., collapse = ""), "\n"),
      file = LOG_FILE, append = TRUE)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Canonical region codes
CA_PROV <- c("AB","BC","MB","NB","NL","NS","NT","NU","ON","PE","QC","SK","YT")
US_STATE <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC","PR","VI","GU","MP"
)

# ---------------- Helpers ----------------
norm_category <- function(x) {
  x <- toupper(trimws(x %||% ""))
  x[!x %in% ALL_CATS] <- NA_character_
  x
}

derive_country <- function(icao) {
  if (is.na(icao) || !nzchar(icao)) return(NA_character_)
  p <- substr(toupper(icao), 1, 1)
  if (p == "C") return("Canada")
  if (p %in% c("K","P")) return("United States")
  NA_character_
}

derive_region <- function(site, country_guess = NA_character_) {
  if (is.na(site) || !nzchar(site)) return(NA_character_)
  toks <- rev(trimws(str_split(site, ",\\s*")[[1]]))
  allowed <- if (country_guess == "Canada") CA_PROV
  else if (country_guess == "United States") US_STATE
  else c(CA_PROV, US_STATE)
  for (tk in toks) {
    if (nchar(tk) == 2 && toupper(tk) %in% allowed) return(toupper(tk))
  }
  NA_character_
}

# ---------------- Load ----------------
if (!file.exists(INPUT_CSV)) stop("Input not found: ", INPUT_CSV)
log_msg("Reading ", INPUT_CSV)

df <- suppressMessages(read_csv(INPUT_CSV, show_col_types = FALSE))

need <- c("icao", "observed_utc", "flight_category", "site")
missing <- setdiff(need, names(df))
if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))

df <- df %>%
  mutate(
    observed_utc    = suppressWarnings(ymd_hms(observed_utc, tz = "UTC")),
    flight_category = norm_category(flight_category),
    country         = map_chr(icao, derive_country),
    region          = map2_chr(site, country, derive_region)
  ) %>%
  filter(!is.na(icao), !is.na(observed_utc)) %>%
  arrange(icao, observed_utc)

# ---------------- Diagnostics ----------------
diag <- df %>%
  transmute(icao, site, observed_utc,
            flight_category, country, region,
            country_ok = !is.na(country) & country %in% c("Canada","United States"),
            region_ok  = case_when(
              country == "Canada" ~ region %in% CA_PROV,
              country == "United States" ~ region %in% US_STATE,
              TRUE ~ FALSE
            ))
write_csv(
  diag %>% filter(!country_ok | !region_ok) %>%
    distinct(icao, site, country, region, country_ok, region_ok),
  file.path(OUT_DIR, "parse_diagnostics.csv")
)

# Stable site name per ICAO
site_lookup <- df %>%
  group_by(icao) %>%
  summarise(site = last(na.omit(site)), .groups = "drop")

# ---------------- Compute durations ----------------
df_dur <- df %>%
  group_by(icao) %>%
  arrange(observed_utc, .by_group = TRUE) %>%
  mutate(minutes = as.numeric(difftime(lead(observed_utc), observed_utc, units = "mins")),
         minutes = ifelse(is.na(minutes), 30, minutes),       # fallback for single obs
         minutes = pmin(pmax(minutes, 0), MAX_GAP_HOURS * 60) # cap
  ) %>%
  ungroup() %>%
  filter(!is.na(flight_category), minutes > 0)

# ---------------- Summaries ----------------

# Airport
airport_share <- df_dur %>%
  group_by(icao, country, region, flight_category) %>%
  summarise(minutes = sum(minutes), .groups = "drop_last") %>%
  mutate(total = sum(minutes)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100*minutes/total, 2), 0)) %>%
  left_join(site_lookup, by = "icao")

airport_share <- df %>%
  distinct(icao, site, country, region) %>%
  crossing(flight_category = ALL_CATS) %>%
  left_join(airport_share, by = c("icao","site","country","region","flight_category")) %>%
  mutate(across(c(minutes,total,pct), ~replace_na(.,0))) %>%
  arrange(icao, desc(pct), flight_category)

# Region
region_share <- df_dur %>%
  filter(country %in% c("Canada","United States"), !is.na(region)) %>%
  group_by(country, region, flight_category) %>%
  summarise(minutes = sum(minutes), .groups = "drop_last") %>%
  mutate(total = sum(minutes)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100*minutes/total, 2), 0)) %>%
  complete(country, region, flight_category = ALL_CATS, fill=list(minutes=0,total=0,pct=0)) %>%
  arrange(country, region, desc(pct), flight_category)

# Country
country_share <- df_dur %>%
  filter(country %in% c("Canada","United States")) %>%
  group_by(country, flight_category) %>%
  summarise(minutes = sum(minutes), .groups = "drop_last") %>%
  mutate(total = sum(minutes)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100*minutes/total, 2), 0)) %>%
  complete(country, flight_category = ALL_CATS, fill=list(minutes=0,total=0,pct=0)) %>%
  arrange(country, desc(pct), flight_category)

# Global
global_share <- df_dur %>%
  group_by(flight_category) %>%
  summarise(minutes = sum(minutes), .groups = "drop_last") %>%
  mutate(total = sum(minutes)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100*minutes/total, 2), 0)) %>%
  complete(flight_category = ALL_CATS, fill=list(minutes=0,total=0,pct=0)) %>%
  arrange(desc(pct), flight_category)

# ---------------- Write outputs ----------------
write_csv(airport_share, file.path(OUT_DIR, "airport_flight_share.csv"))
write_csv(region_share,  file.path(OUT_DIR, "region_flight_share.csv"))
write_csv(country_share, file.path(OUT_DIR, "country_flight_share.csv"))
write_csv(global_share,  file.path(OUT_DIR, "global_flight_share.csv"))

log_msg("Outputs written to ", normalizePath(OUT_DIR))

cat("✅ Done.\n",
    "• ", file.path(OUT_DIR,"airport_flight_share.csv"), "\n",
    "• ", file.path(OUT_DIR,"region_flight_share.csv"), "\n",
    "• ", file.path(OUT_DIR,"country_flight_share.csv"), "\n",
    "• ", file.path(OUT_DIR,"global_flight_share.csv"), "\n",
    "• ", file.path(OUT_DIR,"parse_diagnostics.csv"), "\n", sep = "")

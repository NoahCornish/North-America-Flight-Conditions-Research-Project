#!/usr/bin/env Rscript
# -------------------------------------------------------------------
# analyze_flight_share.R
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
})

# ---------------- Settings ----------------
DATA_DIR       <- "Data"
INPUT_CSV      <- file.path(DATA_DIR, "METARs", "clean_metars.csv")
OUT_DIR        <- file.path(DATA_DIR, "Summaries")
LOG_FILE       <- file.path(OUT_DIR, "analysis_log.txt")
MAX_GAP_HOURS  <- 3  # cap time between reports to avoid outage skew
ALL_CATS       <- c("VFR","MVFR","IFR","LIFR")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_msg <- function(...) {
  cat(paste0("[", format(with_tz(Sys.time(),"America/Toronto"),
                         "%Y-%m-%d %H:%M:%S %Z"), "] ",
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

# Country from ICAO prefix only
derive_country <- function(icao) {
  if (is.na(icao) || !nzchar(icao)) return(NA_character_)
  p <- toupper(substr(icao, 1, 1))
  if (p == "C") return("Canada")
  if (p %in% c("K","P")) return("United States")
  NA_character_
}

# Region from trailing tokens in `site`
derive_region <- function(site, country_guess = NA_character_) {
  if (is.na(site) || !nzchar(site)) return(NA_character_)
  toks <- str_split(site, ",\\s*")[[1]]
  toks <- rev(trimws(toks))
  allowed <- if (!is.na(country_guess) && country_guess == "Canada") CA_PROV
  else if (!is.na(country_guess) && country_guess == "United States") US_STATE
  else c(CA_PROV, US_STATE)
  for (tk in toks) {
    if (nchar(tk) == 2) {
      up <- toupper(tk)
      if (up %in% allowed) return(up)
    }
  }
  NA_character_
}

# Duration until next report, capped
dur_lead_capped <- function(ts, max_hours = 3) {
  d <- as.numeric(difftime(lead(ts), ts, units = "mins"))
  cap <- max_hours * 60
  ifelse(is.na(d), 0, pmin(pmax(d, 0), cap))
}

# Pad WITHIN groups
pad_within <- function(.data, keys, cats = ALL_CATS) {
  .data %>%
    group_by(across(all_of(keys))) %>%
    complete(flight_category = cats,
             fill = list(minutes = 0, total = 0, pct = 0)) %>%
    ungroup()
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
    flight_category = norm_category(flight_category)
  ) %>%
  filter(!is.na(icao), !is.na(observed_utc)) %>%
  arrange(icao, observed_utc)

# ---------------- Parse country/region ----------------
country_vec <- purrr::map_chr(df$icao, derive_country)
region_vec  <- purrr::map2_chr(df$site, country_vec, derive_region)

df <- df %>%
  mutate(
    country = country_vec,
    region  = region_vec
  )

# Diagnostics
diag <- df %>%
  transmute(icao, site, observed_utc,
            flight_category, country, region,
            country_ok = !is.na(country) & country %in% c("Canada","United States"),
            region_ok  = case_when(
              country == "Canada" ~ !is.na(region) & region %in% CA_PROV,
              country == "United States" ~ !is.na(region) & region %in% US_STATE,
              TRUE ~ FALSE
            ))
write_csv(
  diag %>% filter(!country_ok | !region_ok) %>%
    distinct(icao, site, country, region, country_ok, region_ok),
  file.path(OUT_DIR, "parse_diagnostics.csv")
)
log_msg("Diagnostics written to parse_diagnostics.csv")

# Stable site name per ICAO
site_lookup <- df %>%
  group_by(icao) %>%
  summarise(site = last(na.omit(site)), .groups = "drop")

# ---------------- Compute time shares ----------------
df_dur <- df %>%
  group_by(icao) %>%
  arrange(observed_utc, .by_group = TRUE) %>%
  mutate(minutes = as.numeric(difftime(lead(observed_utc), observed_utc, units = "mins"))) %>%
  # Cap at MAX_GAP_HOURS
  mutate(minutes = pmin(pmax(minutes, 0), MAX_GAP_HOURS * 60, na.rm = TRUE)) %>%
  # Fallback: if all minutes are NA (only 1 row), assign 30 min
  mutate(minutes = ifelse(is.na(minutes), 30, minutes)) %>%
  ungroup() %>%
  filter(!is.na(flight_category), minutes > 0)


# ---------- Airport ----------
airport_share <- df_dur %>%
  group_by(icao, country, region, flight_category) %>%
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total = sum(minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100 * minutes / total, 2), 0)) %>%
  left_join(site_lookup, by = "icao") %>%
  select(icao, site, country, region, flight_category, minutes, total, pct)

# Ensure *all airports × categories* exist
all_airports <- df %>%
  distinct(icao, site, country, region)

airport_share <- all_airports %>%
  crossing(flight_category = ALL_CATS) %>%
  left_join(airport_share, by = c("icao","site","country","region","flight_category")) %>%
  mutate(
    minutes = replace_na(minutes, 0),
    total   = replace_na(total, 0),
    pct     = replace_na(pct, 0)
  ) %>%
  arrange(icao, desc(pct), flight_category)

# ---------- Region ----------
region_share <- df_dur %>%
  filter(country %in% c("Canada","United States"), !is.na(region)) %>%
  mutate(region = toupper(region)) %>%
  filter((country == "Canada" & region %in% CA_PROV) |
           (country == "United States" & region %in% US_STATE)) %>%
  group_by(country, region, flight_category) %>%
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total = sum(minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100 * minutes / total, 2), 0)) %>%
  select(country, region, flight_category, minutes, total, pct)

region_share <- pad_within(region_share, c("country","region")) %>%
  arrange(country, region, desc(pct), flight_category)

# ---------- Country ----------
country_share <- df_dur %>%
  filter(country %in% c("Canada","United States")) %>%
  group_by(country, flight_category) %>%
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total = sum(minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100 * minutes / total, 2), 0)) %>%
  select(country, flight_category, minutes, total, pct)

country_share <- pad_within(country_share, c("country")) %>%
  arrange(country, desc(pct), flight_category)

# ---------- Global ----------
global_share <- df_dur %>%
  filter(!is.na(flight_category)) %>%
  group_by(flight_category) %>%
  summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total = sum(minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total > 0, round(100 * minutes / total, 2), 0)) %>%
  select(flight_category, minutes, total, pct)

global_share <- pad_within(global_share, character(0)) %>%
  arrange(desc(pct), flight_category)

# ---------------- Write outputs ----------------
write_csv(airport_share, file.path(OUT_DIR, "airport_flight_share.csv"))
write_csv(region_share,  file.path(OUT_DIR, "region_flight_share.csv"))
write_csv(country_share, file.path(OUT_DIR, "country_flight_share.csv"))
write_csv(global_share,  file.path(OUT_DIR, "global_flight_share.csv"))

log_msg("Wrote airport_flight_share.csv (", nrow(airport_share), " rows)")
log_msg("Wrote region_flight_share.csv (",  nrow(region_share),  " rows)")
log_msg("Wrote country_flight_share.csv (", nrow(country_share), " rows)")
log_msg("Wrote global_flight_share.csv (",  nrow(global_share),  " rows)")

cat("✅ Done.\n",
    "• Data/Summaries/airport_flight_share.csv\n",
    "• Data/Summaries/region_flight_share.csv\n",
    "• Data/Summaries/country_flight_share.csv\n",
    "• Data/Summaries/global_flight_share.csv\n",
    "• Data/Summaries/parse_diagnostics.csv\n", sep = "")

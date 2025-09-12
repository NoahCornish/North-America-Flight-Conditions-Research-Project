# 🚀 v0.1.1 — Major Stability & Data Integrity Update

This release is a significant improvement for the **North America Flight Conditions Research Project (NAFCRP)**.  
It focuses on **data accuracy, reliability, and long-term maintainability** — with several new features and fixes to support users who rely on this project for real-time analysis.

---

## ✨ Key Improvements

### 🕒 Accurate Local Times for All Stations
- Fixed **U.S. local time bug** where stations previously displayed Eastern Time universally.
- Local times are now computed per station using:
  - **OurAirports → IATA codes → Matt Johnson-Pint’s `timezones.csv` mapping**
  - Automatic UTC fallback if station time zone data is missing or invalid.
- Ensures the dashboard accurately reflects **true local observation times**, critical for hourly analysis.

### 🗂️ Continuous Grey-Station Tracking
- Introduced **`airports_missing_flight_category.csv`**, a running log of stations that:
  - Reported a METAR but **did not include a flight category** (VFR/MVFR/IFR/LIFR).
  - These appear as **grey circles** on the map.
- This allows us to monitor problematic stations and potentially exclude them if they consistently fail.

### 🧹 Enhanced Cleaning & Deduplication
- Retained the original deduplication logic, but made it more robust:
  - Keeps rows where at least one significant weather element changed.
  - Reduces noise while preserving legitimate observations.
- Recomputes `observed_local` per row with improved error handling.

---

## 🔄 Workflow & Automation

- The **Fetch → Clean → Summarize workflow** now runs every **4 minutes** — the fastest possible cadence without overlap.
- Each run:
  - Appends to `all_metars.csv` (raw master log)
  - Rebuilds `clean_metars.csv` (deduplicated production dataset)
  - Updates `airports_missing_flight_category.csv`
  - Writes monthly snapshots for archival/debugging
- Safe `git pull --rebase` included before pushing to avoid merge conflicts.

---

## 🗺️ Map & Front-End Updates

- Verified **map.html** remains fully compatible with the updated schema.
- Grey stations are now clearly displayed with a neutral `#999` marker color.
- Confirmed that **~2,500 airports** across Canada and the U.S. are correctly visualized.

---

## 🛡️ Stability & Data Integrity

- Column order remains locked to prevent downstream breakage.
- Cleaning log now reports input row count and retained rows for better monitoring.
- Fail-safe fallbacks keep pipeline operational even if metadata fetch fails.

---

## 🧪 Known Issues

- **Rapid File Growth:**
  - `all_metars.csv` grows ~**0.5 MB every 4 minutes**, reaching ~50 MB in **8–12 hours**.
  - `clean_metars.csv` will also eventually approach GitHub’s **100 MB hard limit**.
  - **Current workaround:** The master CSV is manually deleted once or twice per day to keep the repo healthy.
- **Planned Improvements:**
  - Automatic nightly compaction (move old rows into monthly archives).
  - Exploring external data storage (Git LFS or cloud-hosted archives).
  - Considering on-demand API fetching to eliminate excessive log growth.

---

## 📣 Call for Feedback

This project is **still in active development**, and user feedback is incredibly valuable.  
If you notice any issues, missing data, or inaccuracies, please open an [Issue](../../issues) here on GitHub.  

---

## 📊 Summary

This version is about **accuracy and infrastructure hardening** — the dashboard should now show correct times everywhere, track grey stations for investigation, and provide more reliable data for end users.  
We’re working toward long-term scalability so the dataset remains sustainable as more data is collected.


### 🚀 v0.1.1 Highlights

- ✅ **Local Time Fix:** U.S. station times now show *true station-local wall time* (no more Eastern Time everywhere!)
- 🟡 **Grey Station Tracking:** METARs with missing flight categories are now logged in `airports_missing_flight_category.csv` for review.
- 🧹 **Better Data Cleaning:** Improved deduplication and error handling to ensure cleaner, more reliable data.
- 🔄 **Faster Updates:** Workflow now runs every **4 minutes** with safe commits and automatic monthly snapshots.

⚠️ **Known Issue:** `all_metars.csv` grows quickly (~50 MB in 8–12 hours). Manual deletion is currently required once or twice per day — long-term fixes are being explored.


---

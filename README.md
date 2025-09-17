<h1 align="center">
North America Flight Conditions Research Project (NAFCRP)  

**Developer / Maintainer:** Noah Cornish  
**Release:** v0.1.0 (Pre-Release)  
**License:** MIT  
<h1/>
<p align="center">
  <img src="https://img.shields.io/badge/status-active-brightgreen"/>
  <img src="https://img.shields.io/badge/version-0.1.0-blue"/>
  <img src="https://img.shields.io/github/last-commit/NoahCornish/North-America-Flight-Conditions-Research-Project"/>
  <img src="https://img.shields.io/github/actions/workflow/status/NoahCornish/North-America-Flight-Conditions-Research-Project/fetch_metars.yml?label=METAR%20Fetch"/>
  <img src="https://img.shields.io/badge/data%20updates-every%204%20min-ff69b4"/>
  <img src="https://img.shields.io/badge/airports-tracked%202500+-informational"/>
  <img src="https://img.shields.io/badge/made%20with-R%20%26%20GitHub%20Actions-lightgrey"/>
</p>

## 📖 Overview  

The **North America Flight Conditions Research Project (NAFCRP)** collects, cleans, and archives **METAR aviation weather reports** from Canada, the United States, and surrounding regions.  

The repository provides:  
- **Automated ingestion** of METARs every 4 minutes  
- **Deduplication and cleaning** to retain only meaningful changes  
- **Rolling dataset** (`clean_metars.csv`) with ~50 hours of recent data per airport  
- **Monthly snapshots** and **per-airport archives** for long-term analysis  
- **Public-facing website** displaying current conditions  

## 📂 Key Files  

- `Data/METARs/all_metars.csv` — Master dataset (continuous growth)  
- `Data/METARs/clean_metars.csv` — Last ~50h per ICAO  
- `Data/METARs/clean_metars_MM_YYYY.csv` — Monthly snapshots  
- `Data/METARs/Individual_Airport_All_Time/` — Historical airport archives  

## 📡 Data Sources  

- **METAR observations** —[AvationWeather](https://aviationweather.gov/data/api/)
- **Airport metadata** — [OurAirports](https://ourairports.com/data/)  
- **Timezone mappings** — [Matt Johnson-Pint](https://github.com/mattjohnsonpint)  

## 🔮 Roadmap & Versioning  

This project follows **semantic versioning (SemVer)**:  

- 🟥 **MAJOR (X.0.0):** Fundamental or breaking changes  
- 🟦 **MINOR (0.X.0):** Feature additions or improvements  
- 🟩 **PATCH (0.0.X):** Bug fixes or small adjustments  

### Roadmap (General Direction)  

🚀 **Short-term (days/weeks):** Incremental improvements to archiving, cleaning, and visualization  
🌐 **Mid-term (months):** Repository optimization and workflow scaling  
🛠 **Long-term (far-fetched):** Possible expansion to airports outside North America  

## 📣 Contact  

- **Bug reports / technical issues** → Open a [GitHub Issue](../../issues)  
- **General inquiries / dataset access** → Email the maintainer  

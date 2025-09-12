# 🌎 North America Flight Conditions Research Project (NAFCRP)

![Status](https://img.shields.io/badge/status-active-brightgreen)
![Version](https://img.shields.io/badge/version-1.0.0-blue)
![GitHub last commit](https://img.shields.io/github/last-commit/NoahCornish/North-America-Flight-Conditions-Research-Project)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/NoahCornish/North-America-Flight-Conditions-Research-Project/fetch_metars.yml?label=METAR%20Fetch)
![Data Pipeline](https://img.shields.io/badge/data%20updates-every%204%20min-ff69b4)
![Airports](https://img.shields.io/badge/airports-tracked%202500+-informational)
![Made%20With](https://img.shields.io/badge/made%20with-R%20%26%20GitHub%20Actions-lightgrey)
![Maintainer](https://img.shields.io/badge/maintainer-Noah%20Cornish-yellow)


---

## 📖 Overview
This project continuously collects and cleans METAR (aviation weather) reports for **nearly 2,500 airports** across Canada, the United States, and select other regions.  
It powers a public-facing website that visualizes **real-time flight conditions** (VFR, MVFR, IFR, LIFR) and enables **per-airport historical analysis**.

---

## 🔑 Key Features
- **Automated Data Collection:** Every 4 minutes via GitHub Actions.
- **Deduplication:** Ensures each observation is unique and time-ordered.
- **True Local Timestamps:** Uses IANA timezones per airport for accurate local time conversion.
- **Grey Circle Detection:** Highlights airports missing flight category information.
- **Per-Airport Historical Files:** Organized by country/region for easy offline research.
- **Education-Friendly:** Designed with classroom integration in mind.

---

## 🧑‍🔬 About the Author
This project is created and maintained by **Noah Cornish**,  
a high school science and geography teacher in Moosonee, Ontario, Canada.

- 🎓 **Education:**  
  - B.Ed (Intermediate/Senior) — Nipissing University  
  - Hons. B.A. in Environmental Geography — Nipissing University (Specialist in Geomatics & Environmental Management)  
- 🧑‍🏫 **Teaching Focus:** Integrating real-world data into science and geography education.  
- ✈️ **Passion:** Aviation, weather, and open data accessibility.

---

## 📊 Data Pipeline
1. **Fetch:** Pulls latest METARs via API.
2. **Clean:** Deduplicates, validates, and recomputes `observed_local` times.
3. **Summarize:** Produces aggregate statistics and visualization-ready data.
4. **Archive:** Appends unique rows to local historical storage.

---

## 🌐 Live Website
Explore the live visualization here:  
**[🌍 Live Flight Conditions Map](https://noahcornish.github.io/North-America-Flight-Conditions-Research-Project/)**

---

## 📈 Example Use Cases
- Aviation weather tracking (real-time).
- Climatology research using historical METAR data.
- Classroom demonstrations of weather patterns.
- Training datasets for ML models on flight visibility prediction.

---

## 📝 Contributing
Contributions are welcome — whether it's bug reports, feature requests, or pull requests.  
Open a new issue under [GitHub Issues](../../issues) to get started.

---

## ⚠️ Known Limitations
- Repository size grows quickly due to frequent updates — daily cleanup and future archiving workflows are planned.
- Data availability depends on METAR API uptime and GitHub Actions execution.

---

## 📜 License
This project is open-source and available under the MIT License.  
See [LICENSE](LICENSE) for details.

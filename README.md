# BCVI Stroke Risk Prediction Calculator

Reproducible research compendium for blunt cerebrovascular injury (BCVI) stroke risk prediction model and calculator. This repository contains both the **interactive web calculator** and the **complete analysis code** from Wagner et al. (2026, manuscript in review).

**🌐 Live Calculator:** [https://grady-bcvi-calc.shinyapps.io/calculator/](https://grady-bcvi-calc.shinyapps.io/calculator/)

---

## 📋 Repository Structure

```
grady-bcvi-calculator/
├── Calculator/             # Shiny web application
│   ├── R/, data/, www/     # Code, models, assets
│   └── renv.lock           # Exact package versions
├── Publication/            # Complete analysis pipeline
│   ├── scripts/            # 10 analysis scripts (00-09)
│   ├── R/
│   │   ├── modeling_pipelines/  # Training scripts for ML models
│   │   └── utilities/           # Utility/Helper functions
│   ├── Outputs/            # Tables, figures, models
│   ├── Figures.prism       # GraphPad Prism file for publication figures
│   ├── config.yaml         # Computer-specific paths
│   └── renv.lock           # Exact package versions
├── run_all.R               # Master pipeline script
├── Dockerfile.*            # Docker images
└── docker-compose.yml      # Container orchestration
```

---

## 🚀 Quick Start

### Option 1: Build the Calculator Locally

```bash
cd Calculator
R -e "install.packages('renv'); renv::restore()"
R -e "shiny::runApp()"
```

### Option 2: Run Full Analysis Pipeline and Build the Calculator

```bash
# From root directory
Rscript run_all.R
```

**Requirements:**
- R ≥ 4.5.1
- Data files (see [Data Availability](#-data-availability))
- Update data paths in `Publication/config.yaml` for your system

**What it does:**
1. Restores R packages (via renv)
2. Runs all 10 analysis scripts (generates tables/figures)
3. Builds and launches calculator locally

### Option 3: Use Docker (Recommended for Reproducibility)

Pre-built images are available on [Docker Hub](https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator) for immediate use, or build locally:

```bash
# Build images locally
docker compose build

# Run calculator
docker compose up calculator
# Access at http://localhost:3838

# Or run analysis environment
docker compose up publication
```

**Pull pre-built images from Docker Hub:**
```bash
docker pull jdpreston30/grady-bcvi-calculator:calculator
docker pull jdpreston30/grady-bcvi-calculator:publication
```

---

## 📊 Analysis Scripts

The `Publication/scripts/` folder contains 10 scripts that generate all manuscript tables and figures:

| Script | Description | Output |
|--------|-------------|--------|
| `00_setup.r` | Load packages, detect data paths | - |
| `01_import_and_preprocess.r` | Data cleaning and preprocessing | - |
| `02_descriptive_statistics.r` | Cohort descriptive statistics | T1, ST1, ST2 |
| `03_trad_linear_modeling.r` | Traditional logistic regression | - |
| `04_modeling_and_performance.r` | Train ML models (runs pipelines in `R/modeling_pipelines/`) | ST3, T2 |
| `05_ROC_construction.r` | Generate ROC curves | Figures |
| `06_risk_simulations.r` | Stroke risk simulations and predictions | Figures |
| `07_asa_timing.r` | ASA timing stratified by stroke occurrence | T3 |
| `08_equations.r` | Extract final model equations | - |
| `09_stratification.r` | PPV/NPV stratification analysis | - |

**Tables:** T1-T3 (manuscript), ST1-ST3 (supplementary) → `Publication/Outputs/Tables/`  
**Figures:** Data exported to CSVs by R scripts, final publication figures created in GraphPad Prism (`Figures.prism`)  
**Models:** → `Publication/Outputs/Models/`

---

## 📦 Dependencies

Both projects use **renv** for exact version locking. See `DESCRIPTION` and `renv.lock` files in each subfolder.

**Key packages:**
- **Calculator:** shiny, dplyr, ggplot2, patchwork, ggprism
- **Publication:** All calculator packages PLUS caret, xgboost, randomForest, rstanarm, mice, TernTablesR (GitHub: jdpreston30/TernTablesR)

**Install:**
```r
renv::restore()  # In Calculator/ or Publication/
```

---

## 🔄 Reproducibility

- **renv.lock** files specify exact package versions (including GitHub commits)
- **Docker images** provide identical environments across systems (linux/amd64)
- **config.yaml** centralizes computer-specific paths with auto-detection
- **Pre-computed models** (`all_model_results.rds`) skip expensive computations (set `run_modeling_pipeline: false` in `config.yaml`)

**Development environment:** macOS 14.5, R 4.5.1, Docker 28.5.2

---

## 📖 Citation

**This calculator/model (manuscript in review):**
```bibtex
@article{wagner2026ml,
  title={A MACHINE-LEARNING-BASED TOOL FOR STROKE RISK PREDICTION IN BLUNT CEREBROVASCULAR INJURY: DEVELOPMENT AND PRELIMINARY EVALUATION},
  author={Wagner, Victoria E. and Preston, Joshua D. and Sciarretta, Jason D. and others},
  journal={TBD},
  year={2026},
  note={In Review}
}
```

**Original cohort study:**  
Please also cite the foundational work on a subset of this cohort:

> Wagner, V., Preston, J.D., De Leon Castro, A., Adams, R.W., Garcia-Toca, M., Nguyen, J., Benjamin, E.R., Todd, S.R., & Sciarretta, J.D. (2025). A blunt look at stroke risk in BCVI: Do multiple injuries increase the risk of stroke? *The American Journal of Surgery*, 248, 116480. [https://doi.org/10.1016/j.amjsurg.2025.116480](https://doi.org/10.1016/j.amjsurg.2025.116480)

```bibtex
@article{wagner2025cohort,
  title={A blunt look at stroke risk in {BCVI}: Do multiple injuries increase the risk of stroke?},
  author={Wagner, Victoria and Preston, Joshua D. and De Leon Castro, Alejandro and Adams, Ronnie W. and Garcia-Toca, Manuel and Nguyen, Jonathan and Benjamin, Elizabeth R. and Todd, S. Rob and Sciarretta, Jason D.},
  journal={The American Journal of Surgery},
  volume={248},
  pages={116480},
  year={2025},
  issn={0002-9610},
  doi={10.1016/j.amjsurg.2025.116480}
}
```

---

## 👥 Authors

- **Victoria E. Wagner, MD** - First/Corresponding Author (UTHealth Houston – McGovern School of Medicine, Department of Surgery / Red Duke Trauma Institute) [[ORCID]](https://orcid.org/0009-0005-6287-9078)
- **Joshua D. Preston** - Data Science, Development, & Repository Maintainer (Emory University School of Medicine, Department of Surgery) ([@jdpreston30](https://github.com/jdpreston30)) [[ORCID]](https://orcid.org/0000-0001-9834-3017)
- **Jason D. Sciarretta, MD** - Senior Author (Emory University School of Medicine, Department of Surgery / Grady Memorial Hospital)

---

## 📂 Data Availability

Raw data are **not included** in this repository. A **deidentified dataset** is available from the corresponding authors upon reasonable request for research purposes, subject to:
- Appropriate data use agreements
- Institutional Review Board approval
- HIPAA compliance

**Contact:** See below for author emails.

---

## 🔗 Links

- **Live Calculator:** [https://grady-bcvi-calc.shinyapps.io/calculator/](https://grady-bcvi-calc.shinyapps.io/calculator/)
- **GitHub Repository:** [https://github.com/jdpreston30/grady-bcvi-calculator](https://github.com/jdpreston30/grady-bcvi-calculator)
- **Docker Hub (Pre-built Images):** [https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator](https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator)
- **Manuscript:** TBD (In Review)
- **Original Study:** [Am J Surg 2025;248:116480](https://doi.org/10.1016/j.amjsurg.2025.116480)

---

## 📧 Contact

- **Joshua Preston:** joshua.preston@emory.edu
- **Victoria Wagner:** Victoria.E.Wagner@uth.tmc.edu
- **Jason Sciarretta:** jason.d.sciarretta@emory.edu

**Report bugs:** [GitHub Issues](https://github.com/jdpreston30/grady-bcvi-calculator/issues)
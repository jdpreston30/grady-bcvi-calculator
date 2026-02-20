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
| `09_stratification.r` | Exploratory PPV/NPV stratification analysis | - |

**Tables:** T1-T3 (manuscript), ST1-ST3 (supplementary) → `Publication/Outputs/Tables/`  
**Figures:** Data exported to CSVs → `Publication/Outputs/Figures/`, final publication figures created in GraphPad Prism (`Publication/Figures.prism`)  
**Models:** → `Publication/Outputs/Models/`

---

## 📦 Dependencies

Both projects use **renv** for exact version locking. See `DESCRIPTION` and `renv.lock` files in each subfolder.

**Key packages:**
- **Calculator:** shiny, dplyr, ggplot2, patchwork, ggprism, among others
- **Publication:** All calculator packages PLUS caret, xgboost, randomForest, rstanarm, mice, [TernTablesR](https://github.com/jdpreston30/TernTablesR), among others

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

Wagner V, Preston JD, De Leon Castro A, Mueller WF, Nguyen J, Garcia-Toca M, Benjamin ER, Todd SR, Sciarretta JD. A Machine-Learning-Based Tool for Stroke Risk Prediction in Blunt Cerebrovascular Injury: Development and Preliminary Evaluation. *In Review*.

**Original cohort study:**

Wagner V, Preston JD, De Leon Castro A, Adams RW, Garcia-Toca M, Nguyen J, Benjamin ER, Todd SR, Sciarretta JD. A blunt look at stroke risk in BCVI: Do multiple injuries increase the risk of stroke? Am J Surg. 2025 Oct;248:116480. [doi: 10.1016/j.amjsurg.2025.116480](https://doi.org/10.1016/j.amjsurg.2025.116480). Epub 2025 Jun 11. [PMID: 40555567](https://pubmed.ncbi.nlm.nih.gov/40555567/); PMCID: PMC12286624.

---

## 👥 Authors

- **Victoria E. Wagner** - First/Corresponding Author (UTHealth Houston – McGovern School of Medicine, Department of Surgery / Red Duke Trauma Institute) [[ORCID]](https://orcid.org/0009-0005-6287-9078)
- **Joshua D. Preston** - Data Science, Development, & Repository Maintainer (Emory University School of Medicine, Department of Surgery) ([@jdpreston30](https://github.com/jdpreston30)) [[ORCID]](https://orcid.org/0000-0001-9834-3017)
- **Jason D. Sciarretta** - Senior Author (Emory University School of Medicine, Department of Surgery / Grady Memorial Hospital)

---

## 📂 Data Availability

Raw data are **not included** in this repository. A **deidentified dataset** is available upon reasonable request for research purposes, subject to:
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
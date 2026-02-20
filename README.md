# BCVI Stroke Risk Prediction

Reproducible research compendium for the BCVI (Blunt Cerebrovascular Injury) stroke risk prediction model. This repository contains both the **interactive web calculator** and the **complete analysis code** from Wagner et al. (2025).

## 📋 Repository Structure

```
grady-bcvi-calculator/
├── Calculator/          # Interactive Shiny web application
│   ├── R/              # Modular R code (models, plotting)
│   ├── data/           # Model coefficients (.rds files)
│   ├── www/            # Static assets (images, CSS)
│   ├── global.R        # Global variables and setup
│   ├── ui.R            # User interface
│   ├── server.R        # Server logic
│   ├── app.R           # App entry point
│   ├── DESCRIPTION     # Package dependencies
│   └── renv.lock       # Exact package versions
│
├── Publication/         # Complete reproducible analysis
│   ├── R/              # Analysis utilities and modeling functions
│   ├── scripts/        # 10 main analysis scripts (00-09)
│   ├── Outputs/        # Generated figures, tables, and models
│   │   ├── Figures/
│   │   ├── Tables/     # T1-T3, ST1-ST3 Excel files
│   │   └── Models/     # Saved model objects (.rds)
│   ├── config.yaml     # Computer-specific data paths
│   ├── DESCRIPTION     # Package dependencies
│   └── renv.lock       # Exact package versions
│
├── run_all.R            # Master script to run entire pipeline
├── Dockerfile.calculator    # Calculator Docker image
├── Dockerfile.publication   # Publication Docker image
└── docker-compose.yml       # Orchestration for both
```

## 🚀 Quick Start

### Option 1: Use Docker (Recommended for Reproducibility)

#### Run the Calculator
```bash
# Pull and run the Shiny app
docker pull jdpreston30/grady-bcvi-calculator:calculator
docker run -p 3838:3838 jdpreston30/grady-bcvi-calculator:calculator

# Access at http://localhost:3838
```

#### Run the Publication Analysis
```bash
# Pull and run the analysis environment
docker pull jdpreston30/grady-bcvi-calculator:publication
docker run -it -v $(pwd)/output:/home/analysis/Outputs jdpreston30/grady-bcvi-calculator:publication

# Inside the container, run the analysis:
source("scripts/00_setup.r")
source("scripts/01_import_and_preprocess.r")
# ... etc
```

#### Use Docker Compose (Both at once)
```bash
docker-compose up calculator    # Just the app
docker-compose up publication   # Just the analysis
docker-compose up              # Both
```

### Option 2: Local R Installation

#### Prerequisites
- R >= 4.5.1
- Data files (see **Data Setup** below)

#### Data Setup

The analysis requires raw data files that are **not included** in this repository due to privacy constraints.

**For Project Authors:**
The pipeline automatically detects your computer via `config.yaml` and uses the correct data paths. No manual configuration needed.

**For External Users:**
Add your computer to `Publication/config.yaml`:
```yaml
computers:
  my_computer:
    user: "your_username"
    base_path: "/path/to/grady-bcvi-calculator"
    data_path: "/path/to/your/data"
```

**Required Data Files** (must be in your `data_path`):
- `merged_data_DI.xlsx` - Main patient/injury data
- `descriptive_merged.xlsx` - Additional descriptive statistics (includes ASA_timing sheet)

#### Run Everything (Automated)
```bash
# From root directory
Rscript run_all.R
```

This script automatically:
1. Restores packages for Publication (via renv)
2. Auto-detects your computer and loads data paths from `config.yaml`
3. Runs all 10 analysis scripts in order (00-09)
4. Restores packages for Calculator (via renv)
5. Launches the Shiny app locally

#### Configuration Options

Edit `Publication/config.yaml` to customize behavior:

```yaml
# Set to true to run full modeling pipeline (~30+ minutes)
# Set to false to load pre-computed results (default)
run_modeling_pipeline: false

# Pre-computed model results file
model_results_path: "Outputs/Models/all_model_results.rds"
```

The repository includes pre-computed model results so you can skip the computationally expensive modeling step (script 04).

#### Manual Setup

##### Calculator Setup
```bash
cd Calculator
R -e "install.packages('renv')"
R -e "renv::restore()"  # Installs exact package versions
R -e "shiny::runApp()"
```

##### Publication Setup
```bash
cd Publication
R -e "install.packages('renv')"
R -e "renv::restore()"  # Installs exact package versions
# Run scripts in order: 00_setup.r through 09_stratification.r
```

## 📊 Calculator Usage

The interactive calculator predicts stroke risk in BCVI patients based on:
- **Age**
- **Injury grades** for 5 vascular segments (R/L Carotid, R/L Vertebral, Basilar)

Access the live calculator at: [https://grady-bcvi-calc.shinyapps.io/calculator/](https://grady-bcvi-calc.shinyapps.io/calculator/)

### Local Development
```r
# In Calculator directory
shiny::runApp()
```

## 🔬 Publication Analysis

The `Publication/` folder contains the complete analysis pipeline for the manuscript.

### Main Scripts (run in order):
0. `00_setup.r` - Load packages, set seeds, auto-detect data paths
1. `01_import_and_preprocess.r` - Import and clean raw data
2. `02_descriptive_statistics.r` - Generate T1, ST1, ST2
3. `03_trad_linear_modeling.r` - Logistic regression models
4. `04_modeling_and_performance.r` - ML models, generate ST3, T2
5. `05_ROC_construction.r` - ROC curves and performance metrics
6. `06_risk_simulations.r` - Risk stratification simulations
7. `07_asa_timing.r` - ASA timing analysis, generate T3
8. `08_equations.r` - Extract final model equations
9. `09_stratification.r` - PPV/NPV stratification analysis

### Generated Tables
**Manuscript Tables:**
- `T1.xlsx` - Baseline characteristics (script 02)
- `T2.xlsx` - Model performance comparison (script 04)
- `T3.xlsx` - ASA timing stratified by stroke (script 07)

**Supplementary Tables:**
- `ST1.xlsx` - Injury characteristics by location (script 02)
- `ST2.xlsx` - Logistic regression OR/CI (script 02)
- `ST3.xlsx` - Cross-validation metrics for all models (script 04)

**Additional:**
- `thresholds_not_shown.xlsx` - Risk threshold comparisons (script 09, not in manuscript)

### Outputs Structure
```
Publication/Outputs/
├── Figures/        # All manuscript figures
├── Tables/         # T1-T3, ST1-ST3 Excel files
└── Models/         # Saved model objects (.rds)
    └── all_model_results.rds  # Pre-computed results
```

## 🐳 Using Docker Images

### Calculator (Shiny App)
```bash
# Pull and run from Docker Hub
docker pull jdpreston30/grady-bcvi-calculator:calculator
docker run -p 3838:3838 jdpreston30/grady-bcvi-calculator:calculator

# Access at http://localhost:3838
```

### Publication (Analysis Environment)
```bash
# Pull and run interactively
docker pull jdpreston30/grady-bcvi-calculator:publication
docker run -it jdpreston30/grady-bcvi-calculator:publication R
```

## 📦 Dependencies

Both projects use **renv** for dependency management with exact version locking.

### Calculator
- **Core:** R >= 4.5.1, shiny, dplyr, ggplot2
- **Visualization:** patchwork, ggprism, gridtext, ggh4x
- **Data:** readxl, tibble
- See `Calculator/DESCRIPTION` and `Calculator/renv.lock` for complete list

### Publication
- **All Calculator dependencies PLUS:**
- **Machine Learning:** caret, xgboost, randomForest, e1071, nnet, kernlab
- **Bayesian:** rstanarm, BAS, arm
- **Statistical:** mice (imputation), pROC (ROC curves), ModelMetrics
- **Tables:** TernTablesR (custom package from GitHub: jdpreston30/TernTablesR)
- **Utilities:** conflicted, here, yaml
- See `Publication/DESCRIPTION` and `Publication/renv.lock` for complete list

### Installing TernTablesR
The TernTablesR package is automatically installed from GitHub when using `renv::restore()` or Docker. If you need to install manually:
```r
renv::install("jdpreston30/TernTablesR")
```

## 🔄 Reproducibility

This project ensures computational reproducibility through:

### Dependency Management (renv)
- **`renv.lock`** files specify exact package versions (including GitHub commits)
- **`DESCRIPTION`** files declare required packages
- Run `renv::restore()` to recreate the exact environment
- Separate lock files for Calculator and Publication ensure independence

### Platform Independence (Docker)
- Docker images provide identical environments across systems
- Based on `rocker/shiny:4.5.1` (calculator) and `rocker/r-ver:4.5.1` (publication)
- Platform set to `linux/amd64` for cross-platform compatibility (including Apple Silicon)

### Configuration Management
- `config.yaml` centralizes computer-specific paths
- Auto-detection prevents hardcoded paths
- Pre-computed results (`all_model_results.rds`) allow reproduction without re-running expensive computations

### Version Control
- All analysis code tracked in Git
- Docker images tagged and versioned
- Published to Docker Hub: `jdpreston30/grady-bcvi-calculator`

## 📖 Citation

If you use this calculator or code, please cite:

```bibtex
@article{wagner2025bcvi,
  title={Machine Learning-Based Stroke Risk Prediction in Blunt Cerebrovascular Injury},
  author={Wagner, Victoria E. and Preston, Joshua D. and Sciarretta, Jason D. and others},
  journal={European Journal of Trauma and Emergency Surgery},
  year={2025},
  note={In Review}
}
```

## 👥 Authors

- **Victoria E. Wagner, MD** - Lead Author (University of Texas Health Science Center at Houston)
- **Joshua D. Preston** - Data Science & Development ([@jdpreston30](https://github.com/jdpreston30))
- **Jason D. Sciarretta, MD** - Senior Author (Emory University)


## 🐛 Issues & Contributions

- Report bugs: [GitHub Issues](https://github.com/jdpreston30/grady-bcvi-calculator/issues)
- Contributions welcome via Pull Requests

## 🔗 Links

- **Live Calculator**: [https://grady-bcvi-calc.shinyapps.io/calculator/](https://grady-bcvi-calc.shinyapps.io/calculator/)
- **GitHub**: [https://github.com/jdpreston30/grady-bcvi-calculator](https://github.com/jdpreston30/grady-bcvi-calculator)
- **Docker Hub**: [https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator](https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator)
  - `:calculator` - Shiny app
  - `:publication` - Analysis environment
- **Manuscript**: European Journal of Trauma and Emergency Surgery (In Review)

## ❓ FAQ

**Q: Which Docker image should I use?**  
A: Use `:calculator` to run the web app locally. Use `:publication` to reproduce the analysis in an isolated environment.

**Q: Can I run the analysis without Docker?**  
A: Yes! Clone the repo and use `renv::restore()` in the Publication folder to install dependencies locally, then run `Rscript run_all.R`.

**Q: What about the TernTablesR package?**  
A: It's automatically installed from GitHub (`jdpreston30/TernTablesR`) when using `renv::restore()` or Docker. The exact commit is locked in `renv.lock`.

**Q: How long does the full analysis take?**  
A: With `run_modeling_pipeline: false` (default), about 2-5 minutes. With `run_modeling_pipeline: true`, 30+ minutes depending on your hardware (ML cross-validation is intensive).

**Q: Where are the generated tables?**  
A: All tables export to `Publication/Outputs/Tables/` as Excel files (T1-T3, ST1-ST3). Each table also prints to console when generated.

**Q: Can I use this calculator for my own patients?**  
A: The calculator is provided for research and educational purposes. Clinical use should be done in consultation with appropriate medical expertise. See manuscript for validation details.

## 📧 Contact

For questions about the code, model, or clinical applications, please contact:
- Joshua Preston: joshua.preston@emory.edu
- Victoria Wagner: Victoria.E.Wagner@uth.tmc.edu
- Jason Sciarretta: jason.d.sciarretta@emory.edu

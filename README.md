# BCVI Stroke Risk Prediction

Reproducible research compendium for the BCVI (Blunt Cerebrovascular Injury) stroke risk prediction model. This repository contains both the **interactive web calculator** and the **complete analysis code** from Wagner et al. (2025).

## 📋 Repository Structure

```
grady-bcvi-calculator/
├── Calculator/          # Interactive Shiny web application
│   ├── R/              # Modular R code
│   ├── data/           # Model coefficients
│   ├── www/            # Static assets (images, CSS)
│   ├── DESCRIPTION     # Package dependencies
│   └── renv.lock       # Exact package versions
│
├── Publication/         # Complete reproducible analysis
│   ├── R/              # Analysis functions
│   ├── scripts/        # Main analysis scripts
│   ├── Outputs/        # Generated figures and tables
│   ├── DESCRIPTION     # Package dependencies
│   └── renv.lock       # Exact package versions
│
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

**For Project Authors (Josh/Victoria):**
The pipeline automatically detects your computer (laptop vs desktop) and uses the correct data paths from Dropbox. No manual configuration needed.

**For External Users:**
You have two options:

1. **Add your computer to `config.yaml`:**
   ```yaml
   computers:
     my_computer:
       user: "your_username"
       base_path: "/path/to/grady-bcvi-calculator"
       data_path: "/path/to/your/data"
   ```
   The auto-detection will then work based on your username.

2. **Use a custom data path:**
   Edit `Publication/scripts/00_setup.r` line 29:
   ```r
   config <- load_config_and_paths(custom_data_path = "/your/custom/path/to/data")
   ```

**Required Data Files:**
- `merged_data_DI.xlsx` - Main patient/injury data (assigned to `raw_path` variable)
- `descriptive_merged.xlsx` - Merged descriptive statistics

#### Run Everything (Automated)
```bash
# From root directory - runs full pipeline then launches calculator
Rscript run_all.R
```

This script automatically:
1. Restores packages for Publication (via renv)
2. Auto-detects your computer and loads data paths from `config.yaml`
3. Runs all 9 analysis scripts in order (or loads pre-computed models if configured)
4. Restores packages for Calculator (via renv)
5. Launches the Shiny app in your browser

#### Configuration Options

Edit `config.yaml` to customize behavior:

```yaml
# Set to true to run full modeling pipeline (~hours)
# Set to false to load pre-computed results (default, uses included .rds file)
run_modeling_pipeline: false
```

The repository includes pre-computed model results (`Outputs/Models/all_model_results.rds`) so you can skip the computationally expensive modeling step.

#### Manual Setup

##### Prerequisites
- R >= 4.5.1

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
# Run analysis scripts in order (00, 01, 02, ...)
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

The `Publication/` folder contains the complete analysis pipeline:

### Main Scripts (run in order):
1. `00_dependencies_and_seeds.r` - Setup
2. `01_import_and_preprocess.r` - Data preparation
3. `02_descriptive_statistics.r` - Summary statistics
4. `03_trad_linear_modeling.r` - Traditional models
5. `04_modeling_and_performance.r` - ML model development
6. `05_ROC_construction.r` - Performance evaluation
7. `06_risk_simulations.r` - Risk stratification
8. `07_asa_timing.r` - Treatment timing analysis
9. `08_equations.r` - Final model equations

### Outputs
- Figures → `Publication/Outputs/`
- Tables → `Publication/Outputs/`
- Models → `Publication/Outputs/*.rds`

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

### Calculator
- R >= 4.5.1
- shiny, ggplot2, dplyr, patchwork, ggprism, gridtext, etc.
- See `Calculator/DESCRIPTION` for complete list

### Publication
- All Calculator dependencies PLUS:
- Machine learning: caret, xgboost, randomForest, e1071
- Bayesian: rstanarm, BAS, arm
- Custom: TernTablesR (from GitHub)
- See `Publication/DESCRIPTION` for complete list

## 🔄 Reproducibility

This project uses **renv** for dependency management:
- `renv.lock` files specify exact package versions
- DESCRIPTION files declare required packages
- Docker images ensure platform independence

### To recreate the exact environment:
```r
renv::restore()  # Installs packages from renv.lock
```

## 📖 Citation

If you use this tool or code, please cite:

```bibtex
@article{wagner2025bcvi,
  title={BCVI Stroke Risk Prediction Tool},
  author={Wagner, et al.},
  journal={TBD},
  year={2025},
  doi={TBD}
}
```

## 👥 Authors

- Victoria Wagner et al.
- Josh Preston ([@jdpreston30](https://github.com/jdpreston30))


## 🐛 Issues & Contributions

- Report bugs: [GitHub Issues](https://github.com/jdpreston30/grady-bcvi-calculator/issues)
- Contributions welcome via Pull Requests

## 🔗 Links

- **Live Calculator**: [https://grady-bcvi-calc.shinyapps.io/calculator/](https://grady-bcvi-calc.shinyapps.io/calculator/)
- **Paper**: TBD (manuscript in preparation)
- **GitHub**: [https://github.com/jdpreston30/grady-bcvi-calculator](https://github.com/jdpreston30/grady-bcvi-calculator)
- **Docker Hub**: [https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator](https://hub.docker.com/r/jdpreston30/grady-bcvi-calculator)
  - Tags: `:calculator` (Shiny app), `:publication` (analysis environment)

## ❓ FAQ

**Q: Which Docker image should I use?**  
A: Use `jdpreston30/grady-bcvi-calculator:calculator` to run the web app. Use `jdpreston30/grady-bcvi-calculator:publication` to reproduce the analysis.

**Q: Can I run the analysis without Docker?**  
A: Yes! Use `renv::restore()` in the Publication folder to install dependencies locally.

**Q: What about the TernTablesR package?**  
A: It's automatically installed from GitHub (jdpreston30/TernTablesR) when using renv or Docker.

## 📧 Contact

For questions about the code, model, or clinical applications, please contact:
- Joshua Preston: joshua.preston@emory.edu
- Victoria Wagner: Victoria.E.Wagner@uth.tmc.edu
- Jason Sciarretta: jason.d.sciarretta@emory.edu

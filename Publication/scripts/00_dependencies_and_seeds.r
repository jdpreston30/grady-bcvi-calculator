#* 0: Dependencies and setting seeds
  #+ 0.1: Dependencies
    #- 0.1.1: Install all packages
      install.packages(c("arm", "BAS", "broom", "caret", "glmnet", "iml", "kernlab", "knitr", "mice", "pROC", "progress", "PRROC", "randomForest", "readxl", "RSNNS", "rstanarm", "tibble", "tidyverse", "xgboost"))
    #- 0.1.2: Load libraries
      library(tidyverse)
      library(readxl)
      library(progress)
      library(mice)
      library(BAS)
      library(iml)
      library(ternG)
      library(caret)
      library(pROC)
      library(randomForest)
      library(ggplot2)
      library(broom)
      library(knitr)
      library(kernlab)
      library(arm)
      library(RSNNS)
      library(PRROC)
      library(rstanarm)
  #+ 0.2: Set seeds
    set.seed(2025)
    my_seeds_rf <- c(replicate(100, sample.int(1000, 5), simplify = FALSE), list(sample.int(1000, 1)))

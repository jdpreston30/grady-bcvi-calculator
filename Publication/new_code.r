#! Issue 1 (bookmarked): Check with Tori on this!!! Are we going to define multifocal as more than one in a location or in a side... this is not how we did it previously.
#! Issue 2: When combining models, we need to make multifocal a basilar here
#! Issue 3: How do we deal with the IRs?
#* 0: Dependencies and setting seeds
  #+ 0.1: Dependencies
    #- 0.1.1 Install all packages
      install.packages(c("broom", "caret", "glmnet", "kernlab", "knitr", "mice", "pROC", "randomForest", "readxl", "RSNNS", "tibble", "tidyverse", "xgboost"))
    #- 0.1.2: Load libraries
      library(tidyverse)
      library(readxl)
      library(mice)
      library(ternG)
      library(caret)
      library(pROC)
      library(randomForest)
      library(ggplot2)
      library(broom)
      library(knitr)
      library(kernlab)
      library(RSNNS)
  #+ 0.2: Set seeds
    set.seed(2025)
    my_seeds_rf <- c(replicate(100, sample.int(1000, 5), simplify = FALSE), list(sample.int(1000, 1)))
#* 1: Data import, preprocess and clean
  #+ 1.0 Skip data import below and just load RDS
    list2env(readRDS("/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/all_objects.rds"), envir = .GlobalEnv)
  #+ 1.1: Import data
    raw_path <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/Raw Data/merged_data_DI.xlsx"
    raw_iiii <- read_excel(raw_path, sheet = "data")
  #+ 1.2: Add max grade variable for each location
    #- 1.2.1: Define location columns
      cols_LC <- c(
        "LCCB_1", "LC1_1", "LC2_1", "LC3_1", "LC4_1", "LCO_1",
        "LCCB_2", "LC1_2", "LC2_2", "LC3_2", "LC4_2", "LCO_2")
      cols_RC <- c(
        "RCCB_1", "RC1_1", "RC2_1", "RC3_1", "RC4_1", "RCO_1",
        "RCCB_2", "RC1_2", "RC2_2", "RC3_2", "RC4_2", "RCO_2")
      cols_LV <- c(
        "LV1_1", "LV2_1", "LV3_1", "LV4_1",
        "LV1_2", "LV2_2", "LV3_2", "LV4_2",
        "LV1_3", "LV2_3", "LV3_3", "LV4_3")
      cols_RV <- c(
        "RV1_1", "RV2_1", "RV3_1", "RV4_1",
        "RV1_2", "RV2_2", "RV3_2", "RV4_2",
        "RV1_3", "RV2_3", "RV3_3", "RV4_3")
      cols_VB <- c("VB_1", "VB_2", "VB_3")
      #_Check for NAs
        cat("NA counts by location:\n")
        list(
          LC = cols_LC,
          RC = cols_RC,
          LV = cols_LV,
          RV = cols_RV,
          VB = cols_VB
        ) %>%
          lapply(\(cols) colSums(is.na(raw_iiii[, cols]))) %>%
          lapply(sum) %>%
          print()
    #- 1.2.2: Add max grade variables as well as bilateral variables
      raw_iii <- raw_iiii %>%
        mutate(
          # Max by territory
          Max_LC = pmax(!!!raw_iiii[, cols_LC], na.rm = TRUE),
          Max_RC = pmax(!!!raw_iiii[, cols_RC], na.rm = TRUE),
          Max_LV = pmax(!!!raw_iiii[, cols_LV], na.rm = TRUE),
          Max_RV = pmax(!!!raw_iiii[, cols_RV], na.rm = TRUE),
          Max_VB = pmax(!!!raw_iiii[, cols_VB], na.rm = TRUE)
        ) %>%
        mutate(
          # Max across sides
          max_vert = pmax(Max_LV, Max_RV, Max_VB),
          max_carotid = pmax(Max_RC, Max_LC)) %>%
        mutate(
          BLC = case_when(
            Max_LC == 0 & Max_RC == 0 ~ NA_real_,
            Max_LC > 0 & Max_RC > 0 ~ 1,
            TRUE ~ 0
          ),
          BLV = case_when(
            Max_LV == 0 & Max_RV == 0 ~ NA_real_,
            Max_LV > 0 & Max_RV > 0 ~ 1,
            TRUE ~ 0
          )
        )
    #- 1.2.3: Add multifocal, concom, no_segments variables, change other variables to factors
      raw <- raw_iii %>%
        mutate(
          # Left Carotid
          LC1_max = pmax(!!!raw_iii[, c("LC1_1", "LC1_2")], na.rm = TRUE),
          LC2_max = pmax(!!!raw_iii[, c("LC2_1", "LC2_2")], na.rm = TRUE),
          LC3_max = pmax(!!!raw_iii[, c("LC3_1", "LC3_2")], na.rm = TRUE),
          LC4_max = pmax(!!!raw_iii[, c("LC4_1", "LC4_2")], na.rm = TRUE),
          LCO_max = pmax(!!!raw_iii[, c("LCO_1", "LCO_2")], na.rm = TRUE),
          LCCB_max = pmax(!!!raw_iii[, c("LCCB_1", "LCCB_2")], na.rm = TRUE),
          # Right Carotid
          RC1_max = pmax(!!!raw_iii[, c("RC1_1", "RC1_2")], na.rm = TRUE),
          RC2_max = pmax(!!!raw_iii[, c("RC2_1", "RC2_2")], na.rm = TRUE),
          RC3_max = pmax(!!!raw_iii[, c("RC3_1", "RC3_2")], na.rm = TRUE),
          RC4_max = pmax(!!!raw_iii[, c("RC4_1", "RC4_2")], na.rm = TRUE),
          RCO_max = pmax(!!!raw_iii[, c("RCO_1", "RCO_2")], na.rm = TRUE),
          RCCB_max = pmax(!!!raw_iii[, c("RCCB_1", "RCCB_2")], na.rm = TRUE),
          # Left Vertebral
          LV1_max = pmax(!!!raw_iii[, c("LV1_1", "LV1_2", "LV1_3")], na.rm = TRUE),
          LV2_max = pmax(!!!raw_iii[, c("LV2_1", "LV2_2", "LV2_3")], na.rm = TRUE),
          LV3_max = pmax(!!!raw_iii[, c("LV3_1", "LV3_2", "LV3_3")], na.rm = TRUE),
          LV4_max = pmax(!!!raw_iii[, c("LV4_1", "LV4_2", "LV4_3")], na.rm = TRUE),
          # Right Vertebral
          RV1_max = pmax(!!!raw_iii[, c("RV1_1", "RV1_2", "RV1_3")], na.rm = TRUE),
          RV2_max = pmax(!!!raw_iii[, c("RV2_1", "RV2_2", "RV2_3")], na.rm = TRUE),
          RV3_max = pmax(!!!raw_iii[, c("RV3_1", "RV3_2", "RV3_3")], na.rm = TRUE),
          RV4_max = pmax(!!!raw_iii[, c("RV4_1", "RV4_2", "RV4_3")], na.rm = TRUE)
        ) %>%
        mutate(
          MFCL = as.integer(rowSums(select(., LC1_max:LCCB_max) > 0) > 1),
          MFCR = as.integer(rowSums(select(., RC1_max:RCCB_max) > 0) > 1),
          MFVL = as.integer(rowSums(select(., LV1_max:LV4_max) > 0) > 1),
          MFVR = as.integer(rowSums(select(., RV1_max:RV4_max) > 0) > 1)
        ) %>%
        mutate(
          no_MFC = MFCL + MFCR,
          no_MFV = MFVL + MFVR,
          MFC_present = as.integer(no_MFC > 0),
          MFV_present = as.integer(no_MFV > 0)
        ) %>%
        mutate(
          injury_complex = case_when(
            max_carotid > 0 & max_vert == 0 ~ "C",
            max_carotid == 0 & max_vert > 0 ~ "V",
            max_carotid > 0 & max_vert > 0 ~ "CV",
            max_carotid == 0 & max_vert == 0 ~ NA_character_
          )
        ) %>%
        mutate(
          injury_complex = factor(injury_complex, levels = c("C", "V", "CV"))
        ) %>%
        mutate(
          isolated_C = if_else(injury_complex == "C", 1, 0),
          isolated_V = if_else(injury_complex == "V", 1, 0),
          concom_CV  = if_else(injury_complex == "CV", 1, 0)
        ) %>%
        mutate(
          carotid_segments = as.integer(rowSums(select(., LC1_max:LCCB_max, RC1_max:RCCB_max) > 0, na.rm = TRUE)),
          vertebral_segments = as.integer(rowSums(select(., LV1_max:LV4_max, RV1_max:RV4_max, Max_VB) > 0, na.rm = TRUE))
        ) %>%
        mutate(
          tot_carotid_inj = rowSums(select(., all_of(c(cols_LC, cols_RC))) > 0, na.rm = TRUE),
          tot_vert_inj = rowSums(select(., all_of(c(cols_LV, cols_RV, cols_VB))) > 0, na.rm = TRUE)
        )
  #+ 1.3: Create a modeling version and impute
    #- 1.3.1: Select variables of interest; replace NAs with 0 where appropriate
      raw_modeling_i <- raw %>%
        select(ID,stroke:ISS, max_vert:BLV,no_MFC:MFV_present,isolated_C:concom_CV,MFC_present,MFV_present,carotid_segments:tot_vert_inj) %>%
        mutate(across(c(no_MFV,no_MFC,BLC,BLV), ~ replace_na(., 0))) %>%
        mutate(stroke = if_else(stroke == "Y", 1, 0)) %>%
        mutate(ASA = if_else(ASA == "Y", 1, 0)) %>%
        mutate(sexM = if_else(sex == "M", 1, 0)) %>%
        select(stroke, ASA, sexM, everything(),-sex)
    #- 1.3.2: Check NAs per column
      colSums(is.na(raw_modeling_i))
    #- 1.3.3: Impute missing values for GCS using mice
      raw_modeling <- as_tibble(complete(mice(raw_modeling_i, method = "pmm", m = 5, seed = 123), 1))
      sum(is.na(raw_modeling))
#* 2: Individual analysis of CV+V and CV+C injuries
  #+ 2.1: Convert them all to factors with levels "N" and "Y"
    nested_analysis <- raw_modeling %>%
      mutate(across(
        all_of(c(
          "stroke", "ASA", "sexM", "BLC", "BLV",
          "MFC_present", "MFV_present",
          "isolated_C", "isolated_V", "concom_CV"
        )),
        ~ factor(if_else(. == 1, "Y", "N"), levels = c("N", "Y"))
      ))
  #+ 2.2: Run ternG on only the carotid injuries
    #- 2.2.1: Filter to carotid and vertebral
      carotid_only <- nested_analysis %>% filter(isolated_C == "Y" | concom_CV == "Y")
      vertebral_only <- nested_analysis %>% filter(isolated_V == "Y" | concom_CV == "Y")
    #- 2.2.2: Run ternG
      carotid_results <- ternG(
        data = carotid_only,
        vars = c("BLC", "no_MFC", "MFC_present", "max_carotid", "concom_CV","carotid_segments","tot_carotid_inj"),
        group_var = "stroke",
        force_ordinal = c("max_carotid", "no_MFC"),
        output_docx = "carotid_only.docx"
      )
      vertebral_results <- ternG(
        data = vertebral_only,
        vars = c("BLV", "no_MFV", "MFV_present", "max_vert", "concom_CV","vertebral_segments","tot_vertebral_inj"),
        group_var = "stroke",
        force_ordinal = c("max_vert", "no_MFV"),
        output_docx = "vertebral_only.docx"
      )
#* 3: Traditional Linear Modeling
  #+ 3.1: Add a context variable for the model
    modeling <- nested_analysis %>%
      mutate(
        context_C = if_else(isolated_C == "Y" | concom_CV == "Y", 1, 0),
        context_V = if_else(isolated_V == "Y" | concom_CV == "Y", 1, 0)
      ) %>%
      arrange(desc(stroke)) %>%
      mutate(
        stroke_bin = if_else(stroke == "Y", 1, 0),
        stroke_weight = if_else(stroke == "Y", sum(stroke == "N") / sum(stroke == "Y"), 1)
      ) %>%
      mutate(stroke = factor(stroke, levels = c("N", "Y")))  # ensure it's binary with known order
  #+ 3.2: Try individual nested models for each injury type
    carotid_data <- modeling %>%
      filter(context_C == 1)
    carotid_model <- glm(
      stroke ~ ASA + age + sexM + GCS + ISS + max_carotid + BLC + MFC_present + no_MFC + carotid_segments + tot_carotid_inj,
      data = carotid_data,
      family = binomial()
    )
    summary(carotid_model)
    vertebral_data <- modeling %>%
      filter(context_V == 1)
    vertebral_model <- glm(
      stroke ~ ASA + age + sexM + GCS + ISS + max_vert + BLV + MFV_present + no_MFV + vertebral_segments + tot_vert_inj,
      data = vertebral_data,
      family = binomial()
    )
    summary(vertebral_model)
    #! This approach confirms that regardless of which way you run it, you get ASA, max vert, and max carotid as the most important predictors. Thus, we will proceed to running a full model with all predictors and interaction variables with context variables and see if this is reproduced.
  #+ 3.3: Run models with stepwise selection
    #! Carotid and vertebral segment variables removed due to perfect multicollinearity with the segment replicates variables. Generally, it does the same for the simpler model below
    #- 3.3.1: Run one with all kinds of interaction variables
      full_experimental_model <- glm(
        stroke ~ ASA + age + sexM + GCS + ISS + I(ISS^2) +
          max_vert + I(max_vert^2) +
          max_carotid + I(max_carotid^2) +
          max_vert:max_carotid + # interaction
          context_C * (BLC + MFC_present + tot_carotid_inj) +
          context_V * (BLV + MFV_present + tot_vert_inj) +
          concom_CV,
        data = modeling,
        family = binomial()
        # ,weights = stroke_weight
      )
      stepwise_experimental_model <- stepAIC(full_experimental_model, direction = "both", trace = TRUE)
      summary(stepwise_experimental_model)
    #- 3.3.2: Run a more simple one
      full_simpler_model <- glm(
        stroke ~ ASA + age + sexM + GCS + ISS + max_vert + max_carotid +
          context_C * (BLC + MFC_present + no_MFC + tot_carotid_inj) +
          context_V * (BLV + MFV_present + no_MFV + tot_vert_inj) +
          concom_CV,
        data = modeling,
        family = binomial()
        # ,weights = stroke_weight
      )
      stepwise_simpler_model <- stepAIC(full_simpler_model, direction = "both", trace = TRUE)
      summary(stepwise_simpler_model)
    #- 3.3.3: Create a function which tests performance
      run_caret_model <- function(model_object, dataset, use_weights = TRUE) {
        cv_ctrl <- trainControl(
          method = "cv",
          number = 10,
          classProbs = TRUE,
          summaryFunction = twoClassSummary
        )

        args <- list(
          form = formula(model_object),
          data = dataset,
          method = "glm",
          family = binomial(),
          trControl = cv_ctrl,
          metric = "ROC"
        )

        if (use_weights) {
          args$weights <- dataset$stroke_weight
        }

        trained <- do.call(train, args)
        return(trained)
      }
    #- 3.3.4: Now we will test the performance of each of these without weighting
      caret_model_nowt_simpler <- run_caret_model(stepwise_simpler_model, modeling, use_weights = FALSE)
      caret_model_complex_nowt_complex <- run_caret_model(stepwise_experimental_model, modeling, use_weights = FALSE)
    #! No weighting leads to obviously terrible performance in specificity
    #- 3.3.5: Now we will test the performance of each of these with weighting
      caret_model_wt_simpler <- run_caret_model(stepwise_simpler_model, modeling, use_weights = TRUE)
      caret_model_complex_wt_complex <- run_caret_model(stepwise_experimental_model, modeling, use_weights = TRUE)
    #! Comparing these, we see that the model performance really is about equal between the complex and more simple model
  #+ 3.4: Run models with stepwise except ASA is now an interaction term
    #- 3.4.1: Run one with all kinds of interaction variables
      asa_intx_experimental <- glm(
        stroke ~ ASA * (context_C + context_V + max_carotid + max_vert + concom_CV) +
          age + sexM + GCS + ISS + I(ISS^2) +
          I(max_vert^2) + I(max_carotid^2) +
          max_vert:max_carotid +
          context_C * (BLC + MFC_present + tot_carotid_inj) +
          context_V * (BLV + MFV_present + tot_vert_inj),
        data = modeling,
        family = binomial()
      )
      ASA_intx_exp_stepwise <- stepAIC(asa_intx_experimental, direction = "both", trace = TRUE)
      summary(ASA_intx_exp_stepwise)
    #- 3.4.2: Run a more simple one
      ASA_intx_simple <- glm(
        stroke ~ ASA * (context_C + context_V + max_vert + max_carotid + concom_CV) +
          age + sexM + GCS + ISS +
          context_C * (BLC + MFC_present + no_MFC + tot_carotid_inj) +
          context_V * (BLV + MFV_present + no_MFV + tot_vert_inj),
        data = modeling,
        family = binomial()
        # ,weights = stroke_weight
      )
      ASA_intx_simp_stepwise <- stepAIC(ASA_intx_simple, direction = "both", trace = TRUE)
      summary(ASA_intx_simp_stepwise)
    #- 3.4.3: Now we will test the performance of each of these with weighting
      caret_model_asa_intx_wt_simpler <- run_caret_model(ASA_intx_simp_stepwise, modeling, use_weights = TRUE)
      caret_model_asa_intx_complex_wt_complex <- run_caret_model(ASA_intx_exp_stepwise, modeling, use_weights = TRUE)
  #+ 3.5: Compare the ROC/Sens/Spec of all models
    #- 3.5.1: Function to extract performance metrics
      get_perf <- function(model, name) {
        res <- model$results
        cat("\nModel:", name, "\n")
        cat("  ROC:  ", round(res$ROC, 4), "\n")
        cat("  Sens: ", round(res$Sens, 4), "\n")
        cat("  Spec: ", round(res$Spec, 4), "\n")
      }
    #- 3.5.2: Print metrics for all four models
      get_perf(caret_model_wt_simpler, "Simpler model (weighted)")
      get_perf(caret_model_complex_wt_complex, "Complex model (weighted)")
      get_perf(caret_model_asa_intx_wt_simpler, "Simpler ASA interaction model (weighted)")
      get_perf(caret_model_asa_intx_complex_wt_complex, "Complex ASA interaction model (weighted)")
    #! What we have found here is that the weighting works best,simpler and complex have near equal performance, and the ASA interaction model is somewhat worse but not entirely worse compared to the not interaction models. Regardless, stepAIC may not be the best way to go about this, since AIC is just balancing fit and complexity and is not assessing predictive performance per se. Thus, we will move onto new methods.
  #+ 3.6: Prepare data for testing various models by removing redundant variables and focusing on clinically accessible variables
    ml_modeling_data <- modeling %>%
      select(stroke:age, max_vert, max_carotid,ID,tot_vert_inj,tot_carotid_inj) %>%
      left_join(raw_iii %>% select(ID, Max_LC:Max_VB), by = "ID")
#* 4: Test multiple methods and compare performance
  #+ 4.1 LASSO (variable selection and model fitting)
    #- 4.1.1: Write a full pipeline function to run LASSO
      run_lasso_analysis <- function(data, model_label = "LASSO") {
        y <- data$stroke
        X <- data %>%
          select(-stroke, -c(Max_LC:Max_VB), -ID) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.matrix()
        y_bin <- as.numeric(y == "Y")
        class_weights <- ifelse(y == "Y", sum(y == "N") / sum(y == "Y"), 1)

        # Store out-of-fold predictions
        oof_preds <- purrr::map_dfr(1:10, function(i) {
          set.seed(2025 + i)
          foldid <- caret::createFolds(y, k = 10, list = FALSE)

          purrr::map_dfr(1:10, function(k) {
            test_idx <- which(foldid == k)
            train_idx <- which(foldid != k)

            lasso_fit <- glmnet::cv.glmnet(
              x = X[train_idx, ],
              y = y_bin[train_idx],
              family = "binomial",
              alpha = 1,
              type.measure = "auc",
              weights = class_weights[train_idx]
            )

            test_probs <- predict(lasso_fit, newx = X[test_idx, ], s = "lambda.min", type = "response")
            tibble(
              prob = as.vector(test_probs),
              truth = y[test_idx],
              rep = i,
              fold = k
            )
          })
        })

        # Cross-validated ROC curve object
        roc_cv <- pROC::roc(factor(oof_preds$truth, levels = c("N", "Y")), oof_preds$prob)

        # CV summary stats
        youden_j <- pROC::coords(roc_cv, "best", ret = "youden", transpose = FALSE)
        summary_cv <- tibble(
          Model = paste(model_label, "(CV Prediction, Realistic)"),
          AUC = as.numeric(pROC::auc(roc_cv)),
          Sensitivity = youden_j$sensitivity,
          Specificity = youden_j$specificity,
          YoudensJ = youden_j$youden
        )

        # Final (train/optimistic) model
        final_foldid <- caret::createFolds(y, k = 10, list = FALSE)
        lasso_fit_final <- glmnet::cv.glmnet(
          x = X,
          y = y_bin,
          family = "binomial",
          alpha = 1,
          type.measure = "auc",
          foldid = final_foldid,
          weights = class_weights
        )
        lasso_probs_train <- predict(lasso_fit_final, newx = X, s = "lambda.min", type = "response")
        lasso_preds_train <- ifelse(lasso_probs_train > 0.5, "Y", "N")
        truth_train <- factor(y, levels = c("N", "Y"))
        confmat_train <- caret::confusionMatrix(factor(lasso_preds_train, levels = c("N", "Y")), truth_train, positive = "Y")
        auc_train <- as.numeric(pROC::auc(truth_train, as.vector(lasso_probs_train)))

        summary_train <- tibble(
          Model = paste(model_label, "(Train Prediction, Optimistic)"),
          AUC = auc_train,
          Sensitivity = confmat_train$byClass["Sensitivity"],
          Specificity = confmat_train$byClass["Specificity"]
        )

        # Selected variables
        lasso_coefs <- coef(lasso_fit_final, s = "lambda.min")
        selected_vars <- as.matrix(lasso_coefs) %>%
          as.data.frame() %>%
          tibble::rownames_to_column("Variable") %>%
          dplyr::rename(Coefficient = s1) %>%
          dplyr::filter(Coefficient != 0)

        return(list(
          summary_train = summary_train,
          summary_cv = summary_cv,
          selected_variables = selected_vars,
          roc_curve_cv = roc_cv,
          preds_cv = oof_preds # optional: for external plotting
        ))
      }
    #- 4.1.2: Run version WITH tot inj
      lasso_result <- run_lasso_analysis(data = ml_modeling_data, model_label = "LASSO")
    #- 4.1.3: Version WITHOUT tot_inj
      lasso_result_no_tot <- run_lasso_analysis(data = ml_modeling_data %>% select(-c(tot_vert_inj, tot_carotid_inj)), model_label = "LASSO (no tot_inj)")
  #+ 4.2 Random Forest model fitting
    #- 4.2.1: Set up data for random forest and xgb later
      #_With tot variable
        rf_xgb <- ml_modeling_data %>%
          select(-c(max_carotid, max_vert), -ID)
      #_Without tot variable
        rf_xgb_no_tots <- rf_xgb %>%
          select(-c(tot_carotid_inj, tot_vert_inj))
    #- 4.2.2: Write full pipeline function to run random forest
      run_rf_analysis <- function(data, model_label, my_seeds) {
        # Train control
        ctrl <- trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 10,
          seeds = my_seeds,
          classProbs = TRUE,
          summaryFunction = twoClassSummary,
          sampling = "down",
          savePredictions = "final"
        )
        # Train model
        rf_fit <- caret::train(
          stroke ~ .,
          data = data,
          method = "rf",
          metric = "ROC",
          trControl = ctrl,
          tuneLength = 5
        )
        # Internal performance (optimistic)
        rf_probs_train <- predict(rf_fit, data, type = "prob")[, "Y"]
        rf_preds_train <- predict(rf_fit, data)
        truth_train <- data$stroke
        rf_confmat_train <- caret::confusionMatrix(rf_preds_train, truth_train, positive = "Y")
        rf_auc_train <- as.numeric(pROC::auc(truth_train, rf_probs_train))
        # CV performance (realistic)
        rf_preds_cv <- rf_fit$pred %>%
          filter(mtry == rf_fit$bestTune$mtry)
        truth_cv <- factor(rf_preds_cv$obs, levels = c("N", "Y"))
        preds_cv <- factor(rf_preds_cv$pred, levels = c("N", "Y"))
        probs_cv <- rf_preds_cv$Y
        rf_confmat_cv <- caret::confusionMatrix(preds_cv, truth_cv, positive = "Y")
        rf_auc_cv <- as.numeric(pROC::auc(truth_cv, probs_cv))
        # Output summaries
        summary_train <- tibble(
          Model = paste(model_label, "(Train Prediction, Optimistic)"),
          AUC = rf_auc_train,
          Sensitivity = rf_confmat_train$byClass["Sensitivity"],
          Specificity = rf_confmat_train$byClass["Specificity"]
        )
        summary_cv <- tibble(
          Model = paste(model_label, "(CV Prediction, Realistic)"),
          AUC = rf_auc_cv,
          Sensitivity = rf_confmat_cv$byClass["Sensitivity"],
          Specificity = rf_confmat_cv$byClass["Specificity"]
        )
        varimp <- caret::varImp(rf_fit, scale = TRUE)$importance %>%
          rownames_to_column("Variable") %>%
          arrange(desc(Overall))
        return(list(
          rf_fit = rf_fit,
          summary_train = summary_train,
          summary_cv = summary_cv,
          variable_importance = varimp
        ))
      }
    #- 4.2.3: Version WITH tot_inj
      rf_result_tot <- run_rf_analysis(
        data = rf_xgb,
        model_label = "Random Forest",
        my_seeds = my_seeds_rf
      )
    #- 4.2.4: Version WITHOUT tot_inj
      rf_result_no_tot <- run_rf_analysis(
        data = rf_xgb_no_tots,
        model_label = "Random Forest (No tot_inj)",
        my_seeds = my_seeds_rf
      )
  #+ 4.3 Gradient Boosting model fitting
    #- 4.3.1: Write full pipeline function to run XGBoost
      run_xgb_analysis <- function(data, model_label = "XGBoost", repeats = 10, folds = 10, seed_base = 2025) {
        # Prepare data
        y <- data$stroke
        X <- data %>%
          select(-stroke) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.matrix()
        y_bin <- as.numeric(y == "Y")

        # Class weights
        pos_weight <- sum(y_bin == 0) / sum(y_bin == 1)

        # Collect CV results
        cv_results <- purrr::map_dfr(1:repeats, function(i) {
          set.seed(seed_base + i)
          fold_id <- caret::createFolds(y_bin, k = folds, list = FALSE)

          purrr::map_dfr(1:folds, function(k) {
            test_idx <- which(fold_id == k)
            train_idx <- which(fold_id != k)

            # Downsample training data
            train_df <- data[train_idx, ]
            train_down <- train_df %>%
              group_by(stroke) %>%
              sample_n(min(table(train_df$stroke))) %>%
              ungroup()

            dtrain <- xgboost::xgb.DMatrix(
              data = train_down %>% select(-stroke) %>% mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>% as.matrix(),
              label = as.numeric(train_down$stroke == "Y")
            )
            dtest <- xgboost::xgb.DMatrix(data = X[test_idx, ], label = y_bin[test_idx])

            xgb_fit <- xgboost::xgb.train(
              data = dtrain,
              objective = "binary:logistic",
              eval_metric = "auc",
              nrounds = 100,
              verbose = 0,
              params = list(scale_pos_weight = pos_weight, max_depth = 3, eta = 0.1)
            )

            prob <- predict(xgb_fit, dtest)
            pred <- ifelse(prob > 0.5, "Y", "N")
            truth <- factor(y[test_idx], levels = c("N", "Y"))
            pred <- factor(pred, levels = c("N", "Y"))
            confmat <- caret::confusionMatrix(pred, truth, positive = "Y")
            auc_val <- as.numeric(pROC::auc(truth, prob))

            tibble(
              rep = i, fold = k,
              AUC = auc_val,
              Sensitivity = confmat$byClass["Sensitivity"],
              Specificity = confmat$byClass["Specificity"]
            )
          })
        })

        # Conservative CV results
        summary_cv <- cv_results %>%
          summarise(
            Model = paste(model_label, "(CV Prediction, Realistic)"),
            AUC = mean(AUC),
            Sensitivity = mean(Sensitivity),
            Specificity = mean(Specificity)
          )

        # Final train (optimistic)
        dtrain_full <- xgboost::xgb.DMatrix(data = X, label = y_bin)
        xgb_fit_final <- xgboost::xgb.train(
          data = dtrain_full,
          objective = "binary:logistic",
          eval_metric = "auc",
          nrounds = 100,
          verbose = 0,
          params = list(scale_pos_weight = pos_weight, max_depth = 3, eta = 0.1)
        )
        prob_train <- predict(xgb_fit_final, dtrain_full)
        pred_train <- ifelse(prob_train > 0.5, "Y", "N")
        confmat_train <- caret::confusionMatrix(factor(pred_train, levels = c("N", "Y")), factor(y, levels = c("N", "Y")), positive = "Y")
        auc_train <- as.numeric(pROC::auc(factor(y, levels = c("N", "Y")), prob_train))

        summary_train <- tibble(
          Model = paste(model_label, "(Train Prediction, Optimistic)"),
          AUC = auc_train,
          Sensitivity = confmat_train$byClass["Sensitivity"],
          Specificity = confmat_train$byClass["Specificity"]
        )

        # Return results
        return(list(
          summary_cv = summary_cv,
          summary_train = summary_train,
          final_model = xgb_fit_final
        ))
      }
    #- 4.3.2: Run XGBoost with tot_inj
      xgb_result_tot <- run_xgb_analysis(rf_xgb, model_label = "XGBoost")
    #- 4.3.3: Run XGBoost without tot_inj
      xgb_result_no_tot <- run_xgb_analysis(rf_xgb_no_tots, model_label = "XGBoost (No tot_inj)")
  #+ 4.4: SVM model fitting
    #- 4.4.1: Write full pipeline function to run SVM
      run_svm_model <- function(df, response = "stroke", seed = 2025) {
        set.seed(seed)
        df[[response]] <- factor(df[[response]], levels = c("N", "Y"))
        y <- df[[response]]
        X <- df %>%
          select(-all_of(response)) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.data.frame()

        class_weights <- list(N = 1, Y = sum(y == "N") / sum(y == "Y"))

        # Final (train) model
        model <- e1071::svm(
          x = X,
          y = y,
          class.weights = class_weights,
          probability = TRUE,
          kernel = "radial"
        )

        pred_prob <- attr(predict(model, X, probability = TRUE), "probabilities")[, "Y"]
        pred_class <- ifelse(pred_prob > 0.5, "Y", "N")
        confmat_train <- caret::confusionMatrix(factor(pred_class, levels = c("N", "Y")), y, positive = "Y")
        auc_train <- pROC::auc(y, pred_prob)

        # CV model via caret to get predictions
        ctrl <- caret::trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 10,
          classProbs = TRUE,
          summaryFunction = twoClassSummary,
          savePredictions = "final",
          allowParallel = FALSE
        )

        cv_result <- caret::train(
          x = X, y = y,
          method = "svmRadialWeights",
          weights = unname(unlist(class_weights[y])),
          trControl = ctrl,
          metric = "ROC"
        )

        best_sigma <- cv_result$bestTune$sigma
        best_C <- cv_result$bestTune$C
        svm_preds <- cv_result$pred %>%
          filter(sigma == best_sigma, C == best_C)

        truth_cv <- factor(svm_preds$obs, levels = c("N", "Y"))
        pred_cv <- factor(svm_preds$pred, levels = c("N", "Y"))
        prob_cv <- svm_preds$Y

        confmat_cv <- caret::confusionMatrix(pred_cv, truth_cv, positive = "Y")
        auc_cv <- pROC::auc(truth_cv, prob_cv)

        list(
          model = model,
          summary_train = tibble::tibble(
            Model = "SVM (Train Prediction, Optimistic)",
            AUC = as.numeric(auc_train),
            Sensitivity = confmat_train$byClass["Sensitivity"],
            Specificity = confmat_train$byClass["Specificity"]
          ),
          summary_cv = tibble::tibble(
            Model = "SVM (CV Prediction, Realistic)",
            AUC = as.numeric(auc_cv),
            Sensitivity = confmat_cv$byClass["Sensitivity"],
            Specificity = confmat_cv$byClass["Specificity"]
          )
        )
      }
    #- 4.4.2: Run SVM with tot_inj
      svm_result_tot <- run_svm_model(
        df = rf_xgb,
        response = "stroke",
        seed = 2025
      )
    #- 4.3.3: Run SVM without tot_inj
      svm_result_no_tot <- run_svm_model(
        df = rf_xgb_no_tots,
        response = "stroke",
        seed = 2025
      )
  #+ 4.5: GAM model fitting
    #- 4.5.1: Write full pipeline function to run GAM
      run_gam_model <- function(df, response = "stroke", seed = 2025) {
        set.seed(seed)
        df[[response]] <- factor(df[[response]], levels = c("N", "Y"))
        y <- df[[response]]

        X <- df %>%
          select(-all_of(response)) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.data.frame()

        ctrl <- caret::trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 10,
          classProbs = TRUE,
          summaryFunction = twoClassSummary,
          savePredictions = "final"
        )

        gam_fit <- caret::train(
          x = X,
          y = y,
          method = "gam",
          trControl = ctrl,
          metric = "ROC"
        )

        list(
          gam_fit = gam_fit
        )
      }
    #- 4.5.2: Run GAM with tot_inj
      gam_result_tot <- run_gam_model(
        df = rf_xgb,
        response = "stroke",
        seed = 2025
      )
    #- 4.5.3: Run GAM without tot_inj
      gam_result_no_tot <- run_gam_model(
        df = rf_xgb_no_tots,
        response = "stroke",
        seed = 2025
      )
  #+ 4.6: MLP model fitting
    #- 4.6.1: Write full pipeline function to run MLP
      run_mlp_model <- function(df, response = "stroke", seed = 2025) {
        set.seed(seed)
        df[[response]] <- factor(df[[response]], levels = c("N", "Y"))
        y <- df[[response]]
        X <- df %>%
          select(-all_of(response)) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.data.frame()

        ctrl <- caret::trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 10,
          classProbs = TRUE,
          summaryFunction = twoClassSummary,
          sampling = "down",
          savePredictions = "final",
          allowParallel = FALSE # <--- force single-threaded CV
        )

        mlp_fit <- caret::train(
          x = X,
          y = y,
          method = "nnet", # <--- safer
          metric = "ROC",
          trControl = ctrl,
          tuneLength = 5,
          trace = FALSE
        )

        # Optimistic
        prob_train <- predict(mlp_fit, X, type = "prob")[, "Y"]
        class_train <- predict(mlp_fit, X)
        confmat_train <- caret::confusionMatrix(class_train, y, positive = "Y")
        auc_train <- pROC::auc(y, prob_train)

        # Realistic (from saved CV predictions)
        cv_preds <- mlp_fit$pred %>%
          filter(size == mlp_fit$bestTune$size)
        truth_cv <- factor(cv_preds$obs, levels = c("N", "Y"))
        preds_cv <- factor(cv_preds$pred, levels = c("N", "Y"))
        probs_cv <- cv_preds$Y
        confmat_cv <- caret::confusionMatrix(preds_cv, truth_cv, positive = "Y")
        auc_cv <- pROC::auc(truth_cv, probs_cv)

        list(
          summary_train = tibble::tibble(
            Model = "MLP (Train Prediction, Optimistic)",
            AUC = as.numeric(auc_train),
            Sensitivity = confmat_train$byClass["Sensitivity"],
            Specificity = confmat_train$byClass["Specificity"]
          ),
          summary_cv = tibble::tibble(
            Model = "MLP (CV Prediction, Realistic)",
            AUC = as.numeric(auc_cv),
            Sensitivity = confmat_cv$byClass["Sensitivity"],
            Specificity = confmat_cv$byClass["Specificity"]
          ),
          mlp_fit = mlp_fit # <--- ADD THIS LINE
        )
      }
    #- 4.6.2: Run MLP with tot_inj
      mlp_result_tot <- run_mlp_model(
        df = rf_xgb,
        response = "stroke",
        seed = 2025
      )
    #- 4.6.3: Run MLP without tot_inj
      mlp_result_no_tot <- run_mlp_model(
        df = rf_xgb_no_tots,
        response = "stroke",
        seed = 2025
      )
#* 5: Compare all models
#+ 5.1 Combine all model summaries, add youdens_j
  # all_model_results <- bind_rows(
    # LASSO
    lasso_result$summary_cv %>% mutate(model = "LASSO", Prediction = "CV", total_inj_included = "Y"),
    lasso_result$summary_train %>% mutate(model = "LASSO", Prediction = "Train", total_inj_included = "Y"),
    lasso_result_no_tot$summary_cv %>% mutate(model = "LASSO", Prediction = "CV", total_inj_included = "N"),
    lasso_result_no_tot$summary_train %>% mutate(model = "LASSO", Prediction = "Train", total_inj_included = "N"),
    # Random Forest
    rf_result_tot$summary_cv %>% mutate(model = "Random Forest", Prediction = "CV", total_inj_included = "Y"),
    rf_result_tot$summary_train %>% mutate(model = "Random Forest", Prediction = "Train", total_inj_included = "Y"),
    rf_result_no_tot$summary_cv %>% mutate(model = "Random Forest", Prediction = "CV", total_inj_included = "N"),
    rf_result_no_tot$summary_train %>% mutate(model = "Random Forest", Prediction = "Train", total_inj_included = "N"),

    # XGBoost
    xgb_result_tot$summary_cv %>% mutate(model = "XGBoost", Prediction = "CV", total_inj_included = "Y"),
    xgb_result_tot$summary_train %>% mutate(model = "XGBoost", Prediction = "Train", total_inj_included = "Y"),
    xgb_result_no_tot$summary_cv %>% mutate(model = "XGBoost", Prediction = "CV", total_inj_included = "N"),
    xgb_result_no_tot$summary_train %>% mutate(model = "XGBoost", Prediction = "Train", total_inj_included = "N"),

    # SVM
    svm_result_tot$summary_cv %>% mutate(model = "SVM", Prediction = "CV", total_inj_included = "Y")
    svm_result_tot$summary_train %>% mutate(model = "SVM", Prediction = "Train", total_inj_included = "Y")
    svm_result_no_tot$summary_cv %>% mutate(model = "SVM", Prediction = "CV", total_inj_included = "N")
    svm_result_no_tot$summary_train %>% mutate(model = "SVM", Prediction = "Train", total_inj_included = "N")

    # GAM
    gam_result_tot$summary_cv %>% mutate(model = "GAM", Prediction = "CV", total_inj_included = "Y"),
    gam_result_tot$summary_train %>% mutate(model = "GAM", Prediction = "Train", total_inj_included = "Y"),
    gam_result_no_tot$summary_cv %>% mutate(model = "GAM", Prediction = "CV", total_inj_included = "N"),
    gam_result_no_tot$summary_train %>% mutate(model = "GAM", Prediction = "Train", total_inj_included = "N"),

    # MLP
    mlp_result_tot$summary_cv %>% mutate(model = "MLP", Prediction = "CV", total_inj_included = "Y"),
    mlp_result_tot$summary_train %>% mutate(model = "MLP", Prediction = "Train", total_inj_included = "Y"),
    mlp_result_no_tot$summary_cv %>% mutate(model = "MLP", Prediction = "CV", total_inj_included = "N"),
    mlp_result_no_tot$summary_train %>% mutate(model = "MLP", Prediction = "Train", total_inj_included = "N")

    mutate(across(c(AUC, Sensitivity, Specificity), ~ round(., 3))) %>%
    select(model, Prediction, total_inj_included, AUC, Sensitivity, Specificity) %>%
    mutate(
    youdens_j = Sensitivity + Specificity - 1
  ) 
  mutate(Category = case_when(
    model == "LASSO" ~ "Penalized GLM",
    model == "Random Forest" ~ "Ensemble (bagging)",
    model == "GAM" ~ "Additive spline model",
    model == "XGBoost" ~ "Ensemble (boosting)",
    model == "SVM" ~ "Margin-based classifier",
    model == "MLP" ~ "Neural network",
    TRUE ~ NA_character_
  )) 
  mutate(Feature_Selection = case_when(
    model == "LASSO" ~ "Built-in (penalty)",
    model == "Random Forest" ~ "Implicit (importance)",
    model == "GAM" ~ "None",
    model == "XGBoost" ~ "Implicit (importance)",
    model == "SVM" ~ "None",
    model == "MLP" ~ "None",
    TRUE ~ NA_character_
  )) 
#+ 5.2 Break them into CV vs train
  #- 5.2.1: CV results
    all_model_results_cv <- all_model_results %>%
      filter(Prediction == "CV") %>%
      select(-Prediction) %>%
      arrange(desc(youdens_j))
  #- 5.2.2: Train results
    all_model_results_train <- all_model_results %>%
      filter(Prediction == "Train") %>%
      select(-Prediction)
#+ 5.3 Now make a excluded total_inj and export
  table1 <- all_model_results_cv %>%
    filter(total_inj_included == "N") %>%
    select(model,Category,Feature_Selection,youdens_j, everything(),-total_inj_included) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  write.xlsx(table1, "model_comparison_no_tot_inj.xlsx")
#+ 5.4 Construct ROC curves for all of those model.Selector(
    #- 5.4.1 Define common 100-point grid for smoothed ROC
      smoothed_points <- seq(0, 1, length.out = 100)
    #- 5.4.2 Helper function to compute smoothed ROC curve
      get_smoothed_roc <- function(truth, probs, model_name, smoothed_points = seq(0, 1, length.out = 100)) {
        roc_curve <- pROC::roc(truth, probs, levels = c("N", "Y"), direction = "<")
        smoothed_roc <- data.frame(
          FPR = smoothed_points,
          TPR = approx(1 - roc_curve$specificities, roc_curve$sensitivities, xout = smoothed_points)$y
        )
        smoothed_roc$Model <- model_name
        smoothed_roc$AUC <- as.numeric(pROC::auc(roc_curve))
        return(smoothed_roc)
      }
    #- 5.4.3 Compute smoothed ROC for each model
      #_LASSO
        lasso_roc <- get_smoothed_roc(
          truth = factor(lasso_result_no_tot$preds_cv$truth, levels = c("N", "Y")),
          probs = lasso_result_no_tot$preds_cv$prob,
          model_name = "LASSO"
        )
      #_Random Forest
        rf_roc <- get_smoothed_roc(
          truth = factor(rf_result_no_tot$rf_fit$pred$obs, levels = c("N", "Y")),
          probs = rf_result_no_tot$rf_fit$pred$Y,
          model_name = "Random Forest"
        )
      #_GAM
        gam_roc <- get_smoothed_roc(
          truth = factor(gam_result_no_tot$gam_fit$pred$obs, levels = c("N", "Y")),
          probs = gam_result_no_tot$gam_fit$pred$Y,
          model_name = "GAM"
        )
      #_XGBoost
        xgb_matrix <- xgboost::xgb.DMatrix(
          data = rf_xgb_no_tots %>%
            select(-stroke) %>%
            mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
            as.matrix()
        )
        xgb_probs <- predict(xgb_result_no_tot$final_model, xgb_matrix)
        xgb_truth <- factor(rf_xgb_no_tots$stroke, levels = c("N", "Y"))
        xgb_roc <- get_smoothed_roc(
          truth = xgb_truth,
          probs = xgb_probs,
          model_name = "XGBoost"
        )
      #_SVM
        X_svm <- rf_xgb_no_tots %>%
          select(-stroke) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.data.frame()
        svm_probs <- attr(
          predict(svm_result_no_tot$model, X_svm, probability = TRUE),
          "probabilities"
        )[, "Y"]
        svm_truth <- factor(rf_xgb_no_tots$stroke, levels = c("N", "Y"))
        svm_roc <- get_smoothed_roc(
          truth = svm_truth,
          probs = svm_probs,
          model_name = "SVM"
        )
      #_MLP
        best_size <- mlp_result_no_tot$mlp_fit$bestTune$size
        mlp_preds <- mlp_result_no_tot$mlp_fit$pred %>%
          filter(size == best_size)
        mlp_roc <- get_smoothed_roc(
          truth = factor(mlp_preds$obs, levels = c("N", "Y")),
          probs = mlp_preds$Y,
          model_name = "MLP"
        )
    #- 5.4.4: Combine all smoothed ROC curves and export for graphing in Prism
      smoothed_rocs <- bind_rows(
        lasso_roc,
        rf_roc,
        xgb_roc,
        svm_roc,
        gam_roc,
        mlp_roc
      ) %>%
        select(Model,FPR,TPR,AUC)
      write.csv(smoothed_rocs, "smoothed_roc_curves_no_tot_inj.csv", row.names = FALSE)

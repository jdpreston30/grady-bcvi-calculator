#! Issue 1 (bookmarked): Check with Tori on this!!! Are we going to define multifocal as more than one in a location or in a side... this is not how we did it previously.
#! Issue 2: When combining models, we need to make multifocal a basilar here
#! Issue 3: How do we deal with the IRs?
#! Contents
  #! Table 1 is created in section 5.3
  #! Suppl. Table 1 is created in section 5.4
#* 0: Dependencies and setting seeds
  #+ 0.1: Dependencies
    #- 0.1.1: Install all packages
      install.packages(c("broom", "caret", "glmnet", "kernlab", "knitr", "mice", "pROC","PRROC", "randomForest", "readxl", "RSNNS", "tibble", "tidyverse", "xgboost"))
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
      library(PRROC)
  #+ 0.2: Set seeds
    set.seed(2025)
    my_seeds_rf <- c(replicate(100, sample.int(1000, 5), simplify = FALSE), list(sample.int(1000, 1)))
#* 1: Data import, preprocess and clean
  #+ 1.0: Skip data import below and just load RDS
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
  #+ 4.1: LASSO (variable selection and model fitting)
    #- 4.1.1: Write a full pipeline function to run LASSO
      run_lasso_analysis <- function(data, model_label = "LASSO", use_weights = TRUE) {
        y <- data$stroke
        X <- data %>%
          select(-stroke, -c(Max_LC:Max_VB), -ID) %>%
          mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
          as.matrix()
        y_bin <- as.numeric(y == "Y")

        weights <- if (use_weights) {
          ifelse(y == "Y", sum(y == "N") / sum(y == "Y"), 1)
        } else {
          rep(1, length(y))
        }

        # Repeat CV
        lasso_repeat_results <- purrr::map_dfr(1:10, function(i) {
          set.seed(2025 + i)
          foldid <- caret::createFolds(y, k = 10, list = FALSE)
          lasso_fit <- glmnet::cv.glmnet(
            x = X, y = y_bin,
            family = "binomial",
            alpha = 1,
            type.measure = "auc",
            foldid = foldid,
            weights = weights
          )
          lasso_prob <- predict(lasso_fit, newx = X, s = "lambda.min", type = "response")
          lasso_pred <- ifelse(lasso_prob > 0.5, "Y", "N")
          truth <- factor(y, levels = c("N", "Y"))
          confmat <- caret::confusionMatrix(factor(lasso_pred, levels = c("N", "Y")), truth, positive = "Y")
          auc_val <- as.numeric(pROC::auc(truth, as.vector(lasso_prob)))
          tibble::tibble(
            rep = i,
            lambda_min = lasso_fit$lambda.min,
            AUC = auc_val,
            Sensitivity = confmat$byClass["Sensitivity"],
            Specificity = confmat$byClass["Specificity"],
            truth = as.character(truth),
            prob = as.vector(lasso_prob)
          )
        })

        # Final model fit
        final_foldid <- caret::createFolds(y, k = 10, list = FALSE)
        lasso_fit_final <- glmnet::cv.glmnet(
          x = X, y = y_bin,
          family = "binomial",
          alpha = 1,
          type.measure = "auc",
          foldid = final_foldid,
          weights = weights
        )

        lasso_probs_train <- predict(lasso_fit_final, newx = X, s = "lambda.min", type = "response")
        lasso_preds_train <- ifelse(lasso_probs_train > 0.5, "Y", "N")
        truth_train <- factor(y, levels = c("N", "Y"))
        confmat_train <- caret::confusionMatrix(factor(lasso_preds_train, levels = c("N", "Y")), truth_train, positive = "Y")
        auc_train <- as.numeric(pROC::auc(truth_train, as.vector(lasso_probs_train)))

        # Coefficients
        lasso_coefs <- coef(lasso_fit_final, s = "lambda.min")
        selected_vars <- as.matrix(lasso_coefs) %>%
          as.data.frame() %>%
          tibble::rownames_to_column("Variable") %>%
          dplyr::rename(Coefficient = s1) %>%
          dplyr::filter(Coefficient != 0)

        return(list(
          summary_train = tibble::tibble(
            Model = paste(model_label, "(Train Prediction, Optimistic)"),
            AUC = auc_train,
            Sensitivity = confmat_train$byClass["Sensitivity"],
            Specificity = confmat_train$byClass["Specificity"]
          ),
          summary_cv = lasso_repeat_results %>%
            summarise(
              Model = paste(model_label, "(CV Prediction, Realistic)"),
              AUC = mean(AUC),
              Sensitivity = mean(Sensitivity),
              Specificity = mean(Specificity)
            ),
          selected_variables = selected_vars,
          preds_cv = lasso_repeat_results %>% select(truth, prob)
        ))
      }
    #- 4.1.2: Run version WITH tot inj
      lasso_result <- run_lasso_analysis(data = ml_modeling_data, model_label = "LASSO", use_weights = TRUE)
      lasso_unweighted <- run_lasso_analysis(ml_modeling_data, model_label = "LASSO Unweighted", use_weights = FALSE)
    #- 4.1.3: Version WITHOUT tot_inj
      lasso_result_no_tot <- run_lasso_analysis(data = ml_modeling_data %>% select(-c(tot_vert_inj, tot_carotid_inj)), model_label = "LASSO (no tot_inj)", use_weights = TRUE)
      lasso_result_no_tot_unweighted <- run_lasso_analysis(data = ml_modeling_data %>% select(-c(tot_vert_inj, tot_carotid_inj)), model_label = "LASSO (no tot_inj)", use_weights = FALSE)
  #+ 4.2: Random Forest model fitting
    #- 4.2.1: Set up data for random forest and xgb later
      #_With tot variable
        rf_xgb <- ml_modeling_data %>%
          select(-c(max_carotid, max_vert), -ID)
      #_Without tot variable
        rf_xgb_no_tots <- rf_xgb %>%
          select(-c(tot_carotid_inj, tot_vert_inj))
    #- 4.2.2: Write full pipeline function to run random forest
      run_rf_analysis <- function(data, model_label, my_seeds, sampling_method = "down") {
        # Train control
        ctrl <- trainControl(
          method = "repeatedcv",
          number = 10,
          repeats = 10,
          seeds = my_seeds,
          classProbs = TRUE,
          summaryFunction = twoClassSummary,
          sampling = sampling_method,
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
      rf_result_tot_no_downsampling <- run_rf_analysis(
        data = rf_xgb,
        model_label = "Random Forest (No downsampling)",
        my_seeds = my_seeds_rf,
        sampling_method = NULL
      )
    #- 4.2.4: Version WITHOUT tot_inj
      rf_result_no_tot <- run_rf_analysis(
        data = rf_xgb_no_tots,
        model_label = "Random Forest (No tot_inj)",
        my_seeds = my_seeds_rf
      )
      rf_result_no_tot_no_downsampling <- run_rf_analysis(
        data = rf_xgb_no_tots,
        model_label = "Random Forest (No tot_inj) No Downsampling",
        my_seeds = my_seeds_rf,
        sampling_method = NULL
      )
  #+ 4.3: Gradient Boosting model fitting
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

        # caret CV
        cv_result <- caret::train(
          x = X, y = y,
          method = "svmRadialWeights",
          weights = unname(unlist(class_weights[y])),
          trControl = caret::trainControl(
            method = "repeatedcv", number = 10, repeats = 10,
            classProbs = TRUE, summaryFunction = twoClassSummary
          ),
          metric = "ROC"
        )
        prob_cv <- predict(cv_result, X, type = "prob")[, "Y"]
        pred_cv <- ifelse(prob_cv > 0.5, "Y", "N")
        confmat_cv <- caret::confusionMatrix(factor(pred_cv, levels = c("N", "Y")), y, positive = "Y")
        auc_cv <- pROC::auc(y, prob_cv)
        return(list(
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
          ),
          model = model)) # This must be present to be able to get ROC later
      }
    #- 4.4.2: Run SVM with tot_inj
      svm_result_tot <- run_svm_model(
        df = rf_xgb,
        response = "stroke",
        seed = 2025
      )
    #- 4.4.3: Run SVM without tot_inj
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
          sampling = "down",
          savePredictions = "final"
        )

        gam_fit <- caret::train(
          x = X, y = y,
          method = "gam",
          metric = "ROC",
          trControl = ctrl,
          tuneLength = 5
        )

        prob_train <- predict(gam_fit, X, type = "prob")[, "Y"]
        class_train <- predict(gam_fit, X)
        confmat_train <- caret::confusionMatrix(class_train, y, positive = "Y")
        auc_train <- pROC::auc(y, prob_train)

        cv_preds <- gam_fit$pred %>%
          filter(method == gam_fit$bestTune$method)
        truth_cv <- factor(cv_preds$obs, levels = c("N", "Y"))
        preds_cv <- factor(cv_preds$pred, levels = c("N", "Y"))
        probs_cv <- cv_preds$Y
        confmat_cv <- caret::confusionMatrix(preds_cv, truth_cv, positive = "Y")
        auc_cv <- pROC::auc(truth_cv, probs_cv)

        return(list(
          summary_train = tibble::tibble(
            Model = "GAM (Train Prediction, Optimistic)",
            AUC = as.numeric(auc_train),
            Sensitivity = confmat_train$byClass["Sensitivity"],
            Specificity = confmat_train$byClass["Specificity"]
          ),
          summary_cv = tibble::tibble(
            Model = "GAM (CV Prediction, Realistic)",
            AUC = as.numeric(auc_cv),
            Sensitivity = confmat_cv$byClass["Sensitivity"],
            Specificity = confmat_cv$byClass["Specificity"]
          ),
          gam_fit = gam_fit # ← ADD THIS
        ))
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
          savePredictions = "final"
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
#* 5: Compare all models in tables (Table 1, ST1 and ST2)
  #+ 5.1: Combine all model summaries, add youdens_j
    all_model_results <- bind_rows(
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
      svm_result_tot$summary_cv %>% mutate(model = "SVM", Prediction = "CV", total_inj_included = "Y"),
      svm_result_tot$summary_train %>% mutate(model = "SVM", Prediction = "Train", total_inj_included = "Y"),
      svm_result_no_tot$summary_cv %>% mutate(model = "SVM", Prediction = "CV", total_inj_included = "N"),
      svm_result_no_tot$summary_train %>% mutate(model = "SVM", Prediction = "Train", total_inj_included = "N"),

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
    ) %>%
      mutate(across(c(AUC, Sensitivity, Specificity), ~ round(., 3))) %>%
      mutate(youdens_j = Sensitivity + Specificity - 1) %>%
      mutate(
        Category = case_when(
          model == "LASSO" ~ "Penalized GLM",
          model == "Random Forest" ~ "Ensemble (bagging)",
          model == "GAM" ~ "Additive spline model",
          model == "XGBoost" ~ "Ensemble (boosting)",
          model == "SVM" ~ "Margin-based classifier",
          model == "MLP" ~ "Neural network",
          TRUE ~ NA_character_
        ),
        Feature_Selection = case_when(
          model == "LASSO" ~ "Built-in (penalty)",
          model == "Random Forest" ~ "Implicit (importance)",
          model == "GAM" ~ "None",
          model == "XGBoost" ~ "Implicit (importance)",
          model == "SVM" ~ "None",
          model == "MLP" ~ "None",
          TRUE ~ NA_character_
        )
      ) %>%
      pivot_wider(
        names_from = Prediction,
        values_from = c(AUC, Sensitivity, Specificity, youdens_j),
        names_glue = "{.value}_{Prediction}"
      ) %>%
      group_by(model, total_inj_included) %>%
      summarise(
        AUC_CV = max(AUC_CV, na.rm = TRUE),
        AUC_Train = max(AUC_Train, na.rm = TRUE),
        Sensitivity_CV = max(Sensitivity_CV, na.rm = TRUE),
        Sensitivity_Train = max(Sensitivity_Train, na.rm = TRUE),
        Specificity_CV = max(Specificity_CV, na.rm = TRUE),
        Specificity_Train = max(Specificity_Train, na.rm = TRUE),
        youdens_j_CV = max(youdens_j_CV, na.rm = TRUE),
        youdens_j_Train = max(youdens_j_Train, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        bind_rows(
          tibble(model = "LASSO", Category = "Penalized GLM", Feature_Selection = "Built-in (penalty)"),
          tibble(model = "Random Forest", Category = "Ensemble (bagging)", Feature_Selection = "Implicit (importance)"),
          tibble(model = "GAM", Category = "Additive spline model", Feature_Selection = "None"),
          tibble(model = "XGBoost", Category = "Ensemble (boosting)", Feature_Selection = "Implicit (importance)"),
          tibble(model = "SVM", Category = "Margin-based classifier", Feature_Selection = "None"),
          tibble(model = "MLP", Category = "Neural network", Feature_Selection = "None")
        ),
        by = "model"
      ) %>%
      mutate(Model = model) %>%
      relocate(Model, .before = model) %>%
      rename(TI_included = total_inj_included) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      mutate(
        delta_AUC = AUC_Train - AUC_CV,
        delta_Youdens_J = youdens_j_Train - youdens_j_CV
      ) %>%
      select(model, TI_included, Category, Feature_Selection, delta_AUC, delta_Youdens_J, youdens_j_CV, AUC_CV, Sensitivity_CV, Specificity_CV, youdens_j_Train, AUC_Train, Sensitivity_Train, Specificity_Train)
  #+ 5.3: Now make a excluded total_inj + CV only and export
    table1 <- all_model_results %>%
      filter(TI_included == "N") %>%
      select(model,Category,Feature_Selection,delta_AUC, delta_Youdens_J, youdens_j_CV, AUC_CV,Sensitivity_CV,Specificity_CV) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      arrange(desc(youdens_j_CV))
    write.xlsx(table1, "table1.xlsx")
  #+ 5.4: Now make a full comparison table and export
    ST1 <- all_model_results %>%
      select(-c(Category,Feature_Selection)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      arrange(TI_included,desc(youdens_j_CV))
    write.xlsx(ST1, "ST1.xlsx")
#* 6: Construct ROC curves for all no tot inj included models
  #+ 6.1: Prepare for ROC construction
    #- 6.1.1: Define common 100-point grid for smoothed ROC
      smoothed_points <- seq(0, 1, length.out = 100)
    #- 6.1.2: Helper function to compute smoothed ROC curve
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
  #+ 6.2: Compute smoothed ROC for each model
    #- 6.2.1: Random Forest
      rf_roc <- get_smoothed_roc(
        truth = factor(rf_result_no_tot$rf_fit$pred$obs, levels = c("N", "Y")),
        probs = rf_result_no_tot$rf_fit$pred$Y,
        model_name = "Random Forest"
      )
    #- 6.2.2: XGBoost
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
    #- 6.2.3: MLP
      mlp_roc <- get_smoothed_roc(
        truth = factor(mlp_result_no_tot$mlp_fit$pred$obs, levels = c("N", "Y")),
        probs = mlp_result_no_tot$mlp_fit$pred$Y,
        model_name = "MLP"
      )
    #- 6.2.4: SVM
      svm_roc <- get_smoothed_roc(
        truth = factor(rf_xgb_no_tots$stroke, levels = c("N", "Y")),
        probs = attr(predict(
          svm_result_no_tot$model,
          rf_xgb_no_tots %>%
            select(-stroke) %>%
            mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
            as.data.frame(),
          probability = TRUE
        ), "probabilities")[, "Y"],
        model_name = "SVM"
      )
    #- 6.2.5: GAM
      gam_roc <- get_smoothed_roc(
        truth = factor(gam_result_no_tot$gam_fit$pred$obs, levels = c("N", "Y")),
        probs = gam_result_no_tot$gam_fit$pred$Y,
        model_name = "GAM"
      )
    #- 6.2.6: LASSO
      lasso_roc <- get_smoothed_roc(
        truth = factor(lasso_result_no_tot$preds_cv$truth, levels = c("N", "Y")),
        probs = lasso_result_no_tot$preds_cv$prob,
        model_name = "LASSO"
      )
  #+ 6.3: Export smoothed ROC curves for Prism graphing
    smoothed_rocs <- bind_rows(
      lasso_roc,
      rf_roc,
      mlp_roc,
      gam_roc,
      xgb_roc,
      svm_roc
    ) %>%
      select(Model, FPR, TPR, AUC)
    write.csv(smoothed_rocs, "smoothed_rocs.csv", row.names = FALSE)
#* 7: Construct PR curves for all no tot inj included models
  #+ 7.1: Function for smoothed PR curves
    get_smoothed_pr <- function(truth, probs, model_name, smoothed_points = seq(0, 1, length.out = 100)) {
      # Convert to binary (1 = Y, 0 = N)
      truth_bin <- as.numeric(truth == "Y")

      # PRROC wants scores from the positive and negative classes separately
      pr_obj <- PRROC::pr.curve(
        scores.class0 = probs[truth_bin == 1], # Positive class scores
        scores.class1 = probs[truth_bin == 0], # Negative class scores
        curve = TRUE
      )

      # Extract and interpolate to a common Recall grid
      recall_grid <- smoothed_points
      interp_precision <- approx(x = pr_obj$curve[, 1], y = pr_obj$curve[, 2], xout = recall_grid)$y

      pr_df <- data.frame(
        Recall = recall_grid,
        Precision = interp_precision,
        Model = model_name,
        PRAUC = pr_obj$auc.integral
      )

      return(pr_df)
    }
  #+ 7.2: Compute smoothed PR curves for each model
    #- 7.2.1: Random Forest
      rf_pr <- get_smoothed_pr(
        truth = factor(rf_result_no_tot$rf_fit$pred$obs, levels = c("N", "Y")),
        probs = rf_result_no_tot$rf_fit$pred$Y,
        model_name = "Random Forest"
      )
    #- 7.2.2: XGBoost
      xgb_pr <- get_smoothed_pr(
        truth = factor(rf_xgb_no_tots$stroke, levels = c("N", "Y")),
        probs = xgb_probs,
        model_name = "XGBoost"
      )
    #- 7.2.3: MLP
      mlp_pr <- get_smoothed_pr(
        truth = factor(mlp_result_no_tot$mlp_fit$pred$obs, levels = c("N", "Y")),
        probs = mlp_result_no_tot$mlp_fit$pred$Y,
        model_name = "MLP"
      )
    #- 7.2.4: SVM
      svm_pr <- get_smoothed_pr(
        truth = factor(rf_xgb_no_tots$stroke, levels = c("N", "Y")),
        probs = attr(predict(
          svm_result_no_tot$model,
          rf_xgb_no_tots %>%
            select(-stroke) %>%
            mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
            as.data.frame(),
          probability = TRUE
        ), "probabilities")[, "Y"],
        model_name = "SVM"
      )
    #- 7.2.5: GAM
      gam_pr <- get_smoothed_pr(
        truth = factor(gam_result_no_tot$gam_fit$pred$obs, levels = c("N", "Y")),
        probs = gam_result_no_tot$gam_fit$pred$Y,
        model_name = "GAM"
      )
    #- 7.2.6: LASSO
      lasso_pr <- get_smoothed_pr(
        truth = factor(lasso_result_no_tot$preds_cv$truth, levels = c("N", "Y")),
        probs = lasso_result_no_tot$preds_cv$prob,
        model_name = "LASSO"
      )
  #+ 7.3: Export PR curves for Prism graphing
    smoothed_prs <- bind_rows(
      lasso_pr,
      rf_pr,
      mlp_pr,
      gam_pr,
      xgb_pr,
      svm_pr,
    ) %>%
      select(Model, Recall, Precision, PRAUC)
    write.csv(smoothed_prs, "smoothed_prs.csv", row.names = FALSE)
#* 8: Generate various risk scenario graphs
  #+ 8.1 Make a risk scenario graph based on the LASSO
    #- 8.1.1 Extract coefficients from lasso_result_no_tot
      lasso_coefs <- lasso_result_no_tot$selected_variables
      coef_vec <- setNames(lasso_coefs$Coefficient, lasso_coefs$Variable)
    #- 8.1.2 Define logistic prediction function from LASSO
      predict_stroke_risk_lasso <- function(newdata, coefs) {
        # Ensure all variables are present in newdata
        stopifnot(all(names(coefs)[-1] %in% names(newdata)))

        # Compute linear predictor (logit)
        lp <- coefs["(Intercept)"] +
          rowSums(sweep(newdata[, names(coefs)[-1], drop = FALSE], 2, coefs[-1], `*`))

        # Convert to probability using logistic function
        prob <- 1 / (1 + exp(-lp))
        return(prob * 100) # Convert to percent
      }
    #- 8.1.3 Generate prediction scenarios
      pred_data_no_ASA <- rbind(
        expand.grid(
          max_carotid = 0:5,
          max_vert = 0,
          ASA = 0,
          sexM = 1,
          age = 65,
          scenario = "Carotid"
        ),
        expand.grid(
          max_carotid = 0,
          max_vert = 0:5,
          ASA = 0,
          sexM = 1,
          age = 65,
          scenario = "Vertebral"
        ),
        expand.grid(
          max_carotid = 0:5,
          max_vert = 0:5,
          ASA = 0,
          sexM = 1,
          age = 65,
          scenario = "Combined"
        )
      )
    #- 8.1.4: Repeat for ASA = 1
      pred_data_ASA <- pred_data_no_ASA %>%
        mutate(ASA = 1)
    #- 8.1.5: Combine all, predict, save
      pred_data_all <- bind_rows(pred_data_no_ASA, pred_data_ASA)
      pred_data_all$predicted_prob <- predict_stroke_risk_lasso(pred_data_all, coef_vec)
      write.csv(pred_data_all, "predicted_stroke_risk_with_LASSO.csv", row.names = FALSE)

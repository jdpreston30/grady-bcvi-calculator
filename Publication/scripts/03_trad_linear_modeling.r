#* 3: Traditional Linear Modeling
  #+ 3.1: Add a context variable for the model
    modeling <- descriptive_data %>%
      mutate(
        context_C = if_else(isolated_C == "Y" | concom_CV == "Y", 1, 0),
        context_V = if_else(isolated_V == "Y" | concom_CV == "Y", 1, 0)
      ) %>%
      arrange(desc(stroke)) %>%
      mutate(
        stroke_bin = if_else(stroke == "Y", 1, 0),
        stroke_weight = if_else(stroke == "Y", sum(stroke == "N") / sum(stroke == "Y"), 1)
      ) %>%
      mutate(stroke = factor(stroke, levels = c("N", "Y")))  #_ ensure it's binary with known order
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
          max_vert:max_carotid + #_ interaction
          context_C * (BLC + MFC_present + tot_carotid_inj) +
          context_V * (BLV + MFV_present + tot_vert_inj) +
          concom_CV,
        data = modeling,
        family = binomial()
        #_ ,weights = stroke_weight
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
        #_ ,weights = stroke_weight
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
        #_ ,weights = stroke_weight
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

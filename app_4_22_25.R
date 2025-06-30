#* Setup
#+ Load libraries and set options
  library(shiny)
  library(ggplot2)
  library(gbm)
  library(tibble)
  library(caret)
  library(pROC)
  library(randomForest)
  options(shiny.maxRequestSize = 30 * 1024^2)
  options(shiny.sanitize.errors = FALSE)
#+ Load models
  rf_model_no_asa <- readRDS("model_no_asa.rds")
  rf_model_with_asa <- readRDS("model_with_asa.rds")
  results_with_benefit_rf <- readRDS("results_with_benefit.rds")
  # glm_model_no_asa <- readRDS("glm_model_no_asa.rds")
  # glm_model_with_asa <- readRDS("glm_model_with_asa.rds")
  # results_with_benefit_glm <- readRDS("glm_results_with_benefit.rds")
  gbm_model_no_asa <- readRDS("gbm_model_no_asa.rds")
  gbm_model_with_asa <- readRDS("gbm_model_with_asa.rds")
  results_with_benefit_gbm <- readRDS("gbm_results_with_benefit.rds")
#+Define the counterfactual function for both models
  counterfactual_asa_risk_calc_rf <- function(model_no_asa, model_with_asa, Max_RC, Max_LC, Max_RV, Max_LV, Max_VB, Sex, Age) {
    new_patient <- tibble(
      Max_RC = Max_RC,
      Max_LC = Max_LC,
      Max_RV = Max_RV,
      Max_LV = Max_LV,
      Max_VB = Max_VB,
      Sex = as.numeric(Sex),
      Age = Age,
      ASA = 0
    )

    predictions_no_asa <- predict(model_no_asa, newdata = new_patient, type = "prob")[, 2]
    new_patient_with_asa <- new_patient
    new_patient_with_asa$ASA <- 1
    predictions_with_asa_given <- predict(model_with_asa, newdata = new_patient_with_asa, type = "prob")[, 2]

    results <- tibble(
      Baseline_Risk = predictions_no_asa,
      Risk_If_Given_ASA = predictions_with_asa_given,
      ASA_Benefit = predictions_no_asa - predictions_with_asa_given
    )

    return(results)
  }
  counterfactual_asa_risk_calc_gbm <- function(model_no_asa, model_with_asa, Max_RC, Max_LC, Max_RV, Max_LV, Max_VB, Sex, Age) {
    new_patient <- tibble(
      Max_RC = Max_RC,
      Max_LC = Max_LC,
      Max_RV = Max_RV,
      Max_LV = Max_LV,
      Max_VB = Max_VB,
      Sex = as.numeric(Sex),
      Age = Age,
      ASA = 0
    )

    # Predict without ASA
    predictions_no_asa <- predict(model_no_asa, newdata = new_patient, n.trees = model_no_asa$best.iter, type = "response")

    if (is.na(predictions_no_asa) || length(predictions_no_asa) == 0) {
      warning("GBM: Prediction for Baseline Risk is missing or zero")
    }

    new_patient_with_asa <- new_patient
    new_patient_with_asa$ASA <- 1

    # Predict with ASA
    predictions_with_asa_given <- predict(model_with_asa, newdata = new_patient_with_asa, n.trees = model_with_asa$best.iter, type = "response")

    if (is.na(predictions_with_asa_given) || length(predictions_with_asa_given) == 0) {
      warning("GBM: Prediction for Risk If Given ASA is missing or zero")
    }

    # Calculate ASA Benefit
    results <- tibble(
      Baseline_Risk = predictions_no_asa,
      Risk_If_Given_ASA = predictions_with_asa_given,
      ASA_Benefit = predictions_no_asa - predictions_with_asa_given
    )

    return(results)
  }
  # counterfactual_asa_risk_calc_glm <- function(model_no_asa, model_with_asa, Max_RC, Max_LC, Max_RV, Max_LV, Max_VB, Sex, Age) {
  #   new_patient <- tibble(
  #     Max_RC = Max_RC,
  #     Max_LC = Max_LC,
  #     Max_RV = Max_RV,
  #     Max_LV = Max_LV,
  #     Max_VB = Max_VB,
  #     Sex = as.numeric(Sex),
  #     Age = Age,
  #     ASA = 0 # No ASA for baseline risk
  #   )

  #   # Predict baseline risk using "prob" for caret models to get probabilities
  #   predictions_no_asa <- predict(model_no_asa, newdata = new_patient, type = "prob")[, 2] # Extract probability for positive class

  #   if (is.na(predictions_no_asa) || length(predictions_no_asa) == 0) {
  #     warning("GLM: Prediction for Baseline Risk is missing or zero")
  #   }

  #   new_patient_with_asa <- new_patient
  #   new_patient_with_asa$ASA <- 1

  #   # Predict risk with ASA using "prob"
  #   predictions_with_asa_given <- predict(model_with_asa, newdata = new_patient_with_asa, type = "prob")[, 2] # Extract probability for positive class

  #   if (is.na(predictions_with_asa_given) || length(predictions_with_asa_given) == 0) {
  #     warning("GLM: Prediction for Risk If Given ASA is missing or zero")
  #   }

  #   # Calculate ASA Benefit
  #   results <- tibble(
  #     Baseline_Risk = predictions_no_asa,
  #     Risk_If_Given_ASA = predictions_with_asa_given,
  #     ASA_Benefit = predictions_no_asa - predictions_with_asa_given
  #   )

  #   return(results)
  # }
#+Define UI
  ui <- fluidPage(
    titlePanel("BCVI Stroke Risk Tool"),
    sidebarLayout(
      sidebarPanel(
      
        # Set a fixed width for the panel
        width = 3.0,

        # Add the image from the www folder
        tags$div(
          tags$img(src = "C_map.png", width = "300px", align = "center"),
          tags$img(src = "V_map.png", width = "300px", align = "center"),
          # Position the input boxes over the image
          tags$div(numericInput("Max_RC", NULL, value = NA, min = 0, max = 50),
                  style = "position: absolute; top: 107px; left: 84px; width: 60px;"),
          tags$div(numericInput("Max_LC", NULL, value = NA, min = 0, max = 50),
                  style = "position: absolute; top: 107px; left: 231px; width: 60px;"),
          tags$div(numericInput("Max_RV", NULL, value = NA, min = 0, max = 50),
                  style = "position: absolute; top: 740px; left: 60px; width: 60px;"),
          tags$div(numericInput("Max_LV", NULL, value = NA, min = 0, max = 50),
                  style = "position: absolute; top: 740px; left: 253px; width: 60px;"),
          tags$div(numericInput("Max_VB", NULL, value = NA, min = 0, max = 50),
                  style = "position: absolute; top: 503px; left: 158px; width: 60px;")
        ),

        # Dropdown for selecting sex
        radioButtons("Sex", "Sex:", choices = list("Female" = 1, "Male" = 0), selected = 1),

        # Age input
        numericInput("Age", "Age:", value = NA, min = 0, max = 120, width = "70px"),
              # Radio buttons to select model type (GLM or RF)
        radioButtons("model_choice", "Select Model:",
          choices = list("Random Forest" = "rf", "GBM" = "gbm"),
          selected = "rf"
        ),

        # Submit button
        actionButton("submit", "Predict Stroke Risk & ASA Benefit")
      ),
      mainPanel(
        htmlOutput("baseline_risk"),
        htmlOutput("risk_with_asa"),
        htmlOutput("asa_benefit"),
        plotOutput("asa_benefit_plot"),
        uiOutput("note")
      )
    )
  )
#+Define server logic
  server <- function(input, output) {
    observeEvent(input$submit, {
      # Initialize results_with_benefit based on model choice
      if (input$model_choice == "rf") {
        selected_model_no_asa <- rf_model_no_asa
        selected_model_with_asa <- rf_model_with_asa
        results_with_benefit <- results_with_benefit_rf
      } 
      # else if (input$model_choice == "glm") {
      #   selected_model_no_asa <- glm_model_no_asa
      #   selected_model_with_asa <- glm_model_with_asa
      #   results_with_benefit <- results_with_benefit_glm}
      else if (input$model_choice == "gbm") {
        selected_model_no_asa <- gbm_model_no_asa
        selected_model_with_asa <- gbm_model_with_asa
        results_with_benefit <- results_with_benefit_gbm
      }

      # Create new patient predictions based on the selected model
      # if (input$model_choice == "glm") {
      #   new_patient_results <- counterfactual_asa_risk_calc_glm(
      #     model_no_asa = selected_model_no_asa,
      #     model_with_asa = selected_model_with_asa,
      #     Max_RC = input$Max_RC,
      #     Max_LC = input$Max_LC,
      #     Max_RV = input$Max_RV,
      #     Max_LV = input$Max_LV,
      #     Max_VB = input$Max_VB,
      #     Sex = input$Sex,
      #     Age = input$Age
      #   )
      # } 
      if (input$model_choice == "rf") {
        new_patient_results <- counterfactual_asa_risk_calc_rf(
          model_no_asa = selected_model_no_asa,
          model_with_asa = selected_model_with_asa,
          Max_RC = input$Max_RC,
          Max_LC = input$Max_LC,
          Max_RV = input$Max_RV,
          Max_LV = input$Max_LV,
          Max_VB = input$Max_VB,
          Sex = input$Sex,
          Age = input$Age
        )
      } else if (input$model_choice == "gbm") {
        new_patient_results <- counterfactual_asa_risk_calc_gbm(
          model_no_asa = selected_model_no_asa,
          model_with_asa = selected_model_with_asa,
          Max_RC = input$Max_RC,
          Max_LC = input$Max_LC,
          Max_RV = input$Max_RV,
          Max_LV = input$Max_LV,
          Max_VB = input$Max_VB,
          Sex = input$Sex,
          Age = input$Age
        )
      }

      # Render the output for baseline risk
      output$baseline_risk <- renderUI({
        div(
          style = "text-align: center;",
          HTML(paste("Baseline Stroke Risk: <b><u>", round(new_patient_results$Baseline_Risk * 100, 1), "%</u></b>"))
        )
      })

      # Render the output for risk with ASA
      output$risk_with_asa <- renderUI({
        div(
          style = "text-align: center;",
          HTML(paste("Stroke Risk If Given ASA: <b><u>", round(new_patient_results$Risk_If_Given_ASA * 100, 1), "%</u></b>"))
        )
      })

      # Render the output for ASA benefit
      output$asa_benefit <- renderUI({
        div(
          style = "text-align: center;",
          HTML(paste("ASA Benefit (Risk Reduction): <b><u>", round(new_patient_results$ASA_Benefit * 100, 1), "%</u></b>"))
        )
      })

      # Render the plot for ASA benefit
      output$asa_benefit_plot <- renderPlot({
        ggplot() +
          geom_point(
            data = results_with_benefit,
            aes(x = Baseline_Risk * 100, y = ASA_Benefit * 100, color = "Training Dataset"),
            alpha = 0.6, size = 3
          ) +
          geom_smooth(
            data = results_with_benefit,
            aes(x = Baseline_Risk * 100, y = ASA_Benefit * 100),
            method = "lm", se = FALSE, color = "blue", size = 1.5
          ) +
          geom_point(
            data = new_patient_results,
            aes(x = Baseline_Risk * 100, y = ASA_Benefit * 100, color = "Your Patient"),
            size = 6
          ) +
          labs(
            title = "Estimated Stroke Risk Reduction from ASA",
            x = "Baseline Stroke Risk (%)",
            y = "Estimated ASA Benefit (Risk Reduction, %)"
          ) +
          scale_color_manual(
            values = c("Training Dataset" = "black", "Your Patient" = "red"),
            labels = c("Training Dataset (n = 934 BCVI pts., Grady Mem Hosp)", "Your Patient")
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            legend.text = element_text(face = "bold", size = 11),
            legend.key.height = unit(1.5, "lines"),
            legend.position = "bottom",
            legend.direction = "vertical",
            legend.title = element_blank(),
            aspect.ratio = 1,
            panel.border = element_rect(color = "black", fill = NA, size = 1.5) # Dark frame around the plot
          )
      })
    })

    # Render a note with information about the model
    output$note <- renderUI({
      div(
        style = "text-align: center;",
        HTML("<br>", "<p style='font-size:small; font-style:italic;'>Note: Predictions are based on a preliminary dataset and may not reflect all risk factors.", "<br>", " This model is preliminary and should not be used clinically.</p>")
      )
    })
  }
#+ Run the app
  shinyApp(ui = ui, server = server)
  
  # Run below to deploy:
  #rsconnect::deployApp('/Users/jdp2019/Desktop/Shiny')

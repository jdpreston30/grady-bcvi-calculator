#* Stroke Risk Calculator using LASSO with Platt Scaling
#+ Load libraries and set options
setwd("/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/Risk Calculator/grady-bcvi-calculator/Calculator")
library(shiny)
library(ggplot2)
library(tibble)
options(shiny.maxRequestSize = 30 * 1024^2)
options(shiny.sanitize.errors = FALSE)

#+ Load model files
lasso_coefs <- readRDS("lasso_weighted_coefs.rds")
platt_model <- readRDS("platt_model.rds")

#+ Define prediction function
counterfactual_asa_risk_calc_lasso <- function(coefs_tbl, platt_model, Max_RC, Max_LC, Max_RV, Max_LV, Max_VB, Sex, Age) {
  coefs <- setNames(coefs_tbl$Coefficient, coefs_tbl$Variable)
  
  max_carotid <- max(Max_RC, Max_LC, na.rm = TRUE)
  max_vert <- max(Max_RV, Max_LV, Max_VB, na.rm = TRUE)

  input_df <- tibble(
    ASA = c(0, 1),
    sexM = Sex,
    max_carotid = max_carotid,
    age_ASA = Age * c(0, 1),
    age_max_vert = Age * max_vert
  )

  linear_pred <- coefs["(Intercept)"] +
    coefs["ASA"] * input_df$ASA +
    coefs["sexM"] * input_df$sexM +
    coefs["max_carotid"] * input_df$max_carotid +
    coefs["age_ASA"] * input_df$age_ASA +
    coefs["age_max_vert"] * input_df$age_max_vert

  prob <- predict(platt_model, newdata = tibble(prob = plogis(linear_pred)), type = "response")

  tibble(
    Baseline_Risk = prob[1],
    Risk_If_Given_ASA = prob[2],
    ASA_Benefit = prob[1] - prob[2]
  )
}

#+ Define UI
ui <- fluidPage(
  titlePanel("BCVI Stroke Risk Tool (LASSO Model)"),
  radioButtons("view_mode", "View Mode:", choices = c("Mobile" = "mobile", "Desktop" = "desktop"), selected = "mobile", inline = TRUE),
  uiOutput("dynamic_ui")
)

#+ Define server
server <- function(input, output) {
  output$dynamic_ui <- renderUI({
    if (input$view_mode == "mobile") {
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$div(
            tags$img(src = "C_map.png", width = "300px"),
            tags$img(src = "V_map.png", width = "300px"),
            tags$div(numericInput("Max_RC", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 107px; left: 84px; width: 60px;"),
            tags$div(numericInput("Max_LC", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 107px; left: 231px; width: 60px;"),
            tags$div(numericInput("Max_RV", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 740px; left: 60px; width: 60px;"),
            tags$div(numericInput("Max_LV", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 740px; left: 253px; width: 60px;"),
            tags$div(numericInput("Max_VB", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 503px; left: 158px; width: 60px;")
          ),
          radioButtons("Sex", "Sex:", choices = list("Female" = 1, "Male" = 0), selected = 1),
          numericInput("Age", "Age:", value = NA, min = 0, max = 120),
          actionButton("submit", "Predict Stroke Risk & ASA Benefit")
        ),
        mainPanel(
          htmlOutput("baseline_risk"),
          htmlOutput("risk_with_asa"),
          htmlOutput("asa_benefit"),
          uiOutput("note")
        )
      )
    } else {
      tagList(
        tags$div(
          style = "width: 100%;",
          tags$div(
            style = "display: flex; justify-content: flex-start; align-items: flex-start; gap: 30px; padding: 20px;",
            tags$div(
              style = "position: relative; width: 300px;",
              tags$img(src = "C_map.png", width = "300px"),
              tags$div(numericInput("Max_RC", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 92px; left: 45px; width: 60px;"),
              tags$div(numericInput("Max_LC", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 92px; left: 193px; width: 60px;")
            ),
            tags$div(
              style = "position: relative; width: 300px;",
              tags$img(src = "V_map.png", width = "300px"),
              tags$div(numericInput("Max_RV", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 276px; left: 24px; width: 60px;"),
              tags$div(numericInput("Max_LV", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 276px; left: 215px; width: 60px;"),
              tags$div(numericInput("Max_VB", NULL, value = NA, min = 0, max = 50), style = "position: absolute; top: 37px; left: 123px; width: 60px;")
            ),
            tags$div(
              style = "width: 250px; display: flex; flex-direction: column; gap: 10px; margin-top: 60px; transform: translateX(50px);",
              radioButtons("Sex", "Sex:", choices = list("Female" = 1, "Male" = 0), selected = 1),
              tags$div(numericInput("Age", "Age:", value = NA, min = 0, max = 120), style = "width: 80px;"),
              actionButton("submit", "Predict", style = "width: 100px;")
            ),
            tags$div(
              style = "width: 350px; transform: translateX(-75px);",
              conditionalPanel(
                condition = "input.submit > 0",
                htmlOutput("baseline_risk"),
                htmlOutput("risk_with_asa"),
                htmlOutput("asa_benefit")
              )
            )
          ),
          tags$div(
            style = "width: 100%; text-align: center; margin-top: 30px; transform: translateX(-340px);",
            HTML("<p style='font-size:small; font-style:italic;'>Note: Predictions are based on a preliminary dataset and may not reflect all risk factors.<br>This model is preliminary and should not be used clinically.</p>")
          )
        )
      )
    }
  })

  observeEvent(input$submit, {
    # Coerce inputs
    age <- suppressWarnings(as.numeric(input$Age))
    sex <- suppressWarnings(as.numeric(input$Sex))
    max_rc <- suppressWarnings(as.numeric(input$Max_RC))
    max_lc <- suppressWarnings(as.numeric(input$Max_LC))
    max_rv <- suppressWarnings(as.numeric(input$Max_RV))
    max_lv <- suppressWarnings(as.numeric(input$Max_LV))
    max_vb <- suppressWarnings(as.numeric(input$Max_VB))

    # Check for missing or invalid values
    if (any(is.na(c(age, sex, max_rc, max_lc, max_rv, max_lv, max_vb)))) {
      showNotification("Please enter valid numeric values for all fields.", type = "error")
      return(NULL)
    }

    # Compute max injury grades
    max_carotid <- max(max_rc, max_lc)
    max_vert <- max(max_rv, max_lv, max_vb)

    # Call the function with both ASA values inside
    new_patient_results <- counterfactual_asa_risk_calc_lasso(
      coefs_tbl = lasso_coefs,
      platt_model = platt_model,
      Max_RC = max_rc,
      Max_LC = max_lc,
      Max_RV = max_rv,
      Max_LV = max_lv,
      Max_VB = max_vb,
      Sex = sex,
      Age = age
    )

    # [continue rendering outputs...]
  })

  output$note <- renderUI({
    div(style = "text-align: center;", HTML("<br><p style='font-size:small; font-style:italic;'>Note: Predictions are based on a preliminary dataset and may not reflect all risk factors.<br>This model is preliminary and should not be used clinically.</p>"))
  })
}

#+ Run the app
shinyApp(ui = ui, server = server)

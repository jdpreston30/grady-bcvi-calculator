options(ggplot2.suppress_deprecated = TRUE)
server <- function(input, output, session) {
  #+ Enable Enter to trigger prediction
  session$sendCustomMessage(type = "bindEnterKey", message = list(buttonId = "submit"))

  patient_data <- reactiveVal(NULL)
  show_plot <- reactiveVal(FALSE)
  error_message <- reactiveVal(NULL)

  # Text for disclaimer

  disclaimer_text <- "<strong>Note: This clinical tool is designed for healthcare professionals and is for informational purposes only.<br>Predictions are estimates based on a model that has not yet been externally validated.<br>Predictions outside the training age range (12-97 years) represent extrapolations and should be interpreted with caution.<br>This tool does not offer medical advice nor guarantee that a specific event will or will not occur.<br>This model is preliminary and is not a replacement for clinical judgement.</strong><br><br>
    For further details, see the published manuscript associated with this work
    (<a href='https://pubmed.ncbi.nlm.nih.gov/40555567/' target='_blank'>Wagner et al. 2025</a>;
    <a href='https://doi.org/10.1234/example2' target='_blank'>Wagner et al. 2025</a>).<br>
    The GitHub repository associated with this project can be found
    <a href='https://github.com/jdpreston30/grady-bcvi-calculator' target='_blank'>here</a>.<br>
    *AT = The use of ASA or anticoagulant therapy was consolidated into a binary antithrombotic variable (“AT”) during model development."

  output$mobile_disclaimer <- renderUI({
    style_margin <- if (show_plot()) "margin-top: 30px;" else "margin-top: 10px;"
    div(
      style = paste(style_margin, "font-size: small; font-style: italic;"),
      HTML(disclaimer_text)
    )
  })
  #+ Dynamic UI for desktop vs. mobile
  output$dynamic_ui <- renderUI({
    plot_area <- plotOutput("asa_benefit_plot", height = if (input$view_mode == "mobile") "475px" else "500px")

    error_box <- uiOutput("error_text")

    if (input$view_mode == "mobile") {
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$div(
            style = "position: relative;",
            tags$img(src = "C_map_mobile.png", width = "300px"),
            tags$img(src = "V_map_mobile.png", width = "300px"),
            tags$div(numericInput("Max_RC", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 87px; left: 48px; width: 60px;"),
            tags$div(numericInput("Max_LC", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 87px; left: 195px; width: 60px;"),
            tags$div(numericInput("Max_RV", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 720px; left: 24px; width: 60px;"),
            tags$div(numericInput("Max_LV", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 720px; left: 217px; width: 60px;"),
            tags$div(numericInput("Max_VB", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 483px; left: 122px; width: 60px;"),
            tags$div(numericInput("Age", "Age:", value = NA, min = 0), style = "position: absolute; top: 878px; left: 122px; width: 60px; text-align: center;")
          ),
          tags$div(
            style = "display: flex; flex-direction: column; gap: 10px; margin-top: 60px;",
            actionButton("submit", "Predict"),
            error_box,

            # Always render plot container + disclaimer
            div(
              style = "margin-top: 20px;",
              conditionalPanel(
                condition = "output.plotVisible == true",
                div(
                  style = "transform: translateX(-15px);", # << correct: applied to inner div
                  plot_area
                )
              ),
              uiOutput("mobile_disclaimer")
            )
          )
        ),
        mainPanel()
      )
    } else {
      tags$div(
        style = "width: 100%;",
        tags$div(
          style = "display: flex; justify-content: flex-start; align-items: flex-start; gap: 30px; padding: 20px;",
          tags$div(
            style = "position: relative; width: 300px;",
            tags$img(src = "C_map.png", width = "300px"),
            tags$div(numericInput("Max_RC", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 92px; left: 45px; width: 60px;"),
            tags$div(numericInput("Max_LC", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 92px; left: 193px; width: 60px;")
          ),
          tags$div(
            style = "position: relative; width: 300px;",
            tags$img(src = "V_map.png", width = "300px"),
            tags$div(numericInput("Max_RV", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 276px; left: 27px; width: 60px;"),
            tags$div(numericInput("Max_LV", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 276px; left: 218px; width: 60px;"),
            tags$div(numericInput("Max_VB", NULL, value = NA, min = 0, max = 5), style = "position: absolute; top: 37px; left: 121px; width: 60px;")
          ),
          tags$div(
            style = "width: 250px; display: flex; flex-direction: column; gap: 10px; margin-top: 60px; transform: translateX(10px);",
            # radioButtons("Sex", "Sex:", choices = list("Female" = 1, "Male" = 0), selected = 1),
            div(style = "width: 65px;", numericInput("Age", "Age:", value = NA, min = 0)),
            actionButton("submit", "Predict", style = "width: 100px;"),
            error_box
          ),
          conditionalPanel(condition = "output.plotVisible == true", tags$div(style = "width: 75%; margin: 0px 0px 0px 0px; position: relative; left: -50px; top = -30px", plot_area))
        ),
        tags$div(
          style = "width: 100%; text-align: center; margin-top: 30px; transform: translateX(-240px) translateY(-50px);",
          HTML(disclaimer_text)
        )
      )
    }
  })

  output$error_text <- renderUI({
    req(error_message())
    div(style = "color: red; margin-top: 10px;", strong(error_message()))
  })

  output$plotVisible <- reactive({
    show_plot()
  })
  outputOptions(output, "plotVisible", suspendWhenHidden = FALSE)

  observeEvent(input$submit, {
    # req(input$Sex)
    injury_vals <- c(input$Max_RC, input$Max_LC, input$Max_RV, input$Max_LV, input$Max_VB)

    # Check for errors
    if (all(is.na(injury_vals)) || all(injury_vals == 0, na.rm = TRUE)) {
      error_message("Please enter values between 1 and 5 for injury grades. 0s or blank entries indicate absence of injury")
      show_plot(FALSE)
      return()
    }
    if (any(injury_vals < 0, na.rm = TRUE) || any(injury_vals > 5, na.rm = TRUE)) {
      error_message("Please enter values between 1 and 5 for injury grades. 0s or blank entries indicate absence of injury")
      show_plot(FALSE)
      return()
    }
    if (is.na(input$Age) || input$Age == "") {
      error_message("Please enter age.")
      show_plot(FALSE)
      return()
    }
    if (input$Age > 120) {
      error_message("Age must be 120 or below.")
      show_plot(FALSE)
      return()
    }

    error_message(NULL)
    show_plot(TRUE)

    pd <- list(
      Max_RC = ifelse(is.na(input$Max_RC), 0, input$Max_RC),
      Max_LC = ifelse(is.na(input$Max_LC), 0, input$Max_LC),
      Max_RV = ifelse(is.na(input$Max_RV), 0, input$Max_RV),
      Max_LV = ifelse(is.na(input$Max_LV), 0, input$Max_LV),
      Max_VB = ifelse(is.na(input$Max_VB), 0, input$Max_VB),
      Age = as.numeric(input$Age)
    )

    patient_data(pd)

    output$asa_benefit_plot <- renderPlot(
      {
        validate(
          need(!is.null(patient_data()), "Waiting for input..."),
          need(nrow(as.data.frame(patient_data())) > 0, "No data")
        )

        par(mar = c(4, 4, 2, 1)) # ← ensure margins are sane

        if (input$view_mode == "mobile") {
          suppressWarnings(
            plot_asa_benefit_mobile(
              coefs_tbl = lasso_coefs,
              platt_model = platt_model,
              patient_data = patient_data()
            )
          )
        } else {
          suppressWarnings(
            plot_asa_benefit(
              coefs_tbl = lasso_coefs,
              platt_model = platt_model,
              patient_data = patient_data()
            )
          )
        }
      },
      width = if (input$view_mode == "mobile") 353 else 600,
      height = if (input$view_mode == "mobile") 400 else 500
    ) # ← add this explicitly
  })
}

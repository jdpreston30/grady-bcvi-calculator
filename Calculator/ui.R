ui <- fluidPage(
  titlePanel("BCVI Stroke Risk Prediction Tool"),
  radioButtons("view_mode", "View Mode:", choices = c("Mobile" = "mobile", "Desktop" = "desktop"), selected = "mobile", inline = TRUE),
  # uiOutput("auth_ui"), #! replaces uiOutput("dynamic_ui"), remove once public
  uiOutput("dynamic_ui"),  # ← restore this line and remove auth_ui when public
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('bindEnterKey', function(message) {
        $(document).on('keypress', function(e) {
          if (e.which === 13) {
            $('#' + message.buttonId).click();
          }
        });
      });
    "))
  )
)

source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)

rsconnect::applications(account = "bcvi-calc")

# cd /Users/JoshsMacbook2015/Desktop/grady-bcvi-calculator/Calculator && R -e "shiny::runApp(port = 8080)"
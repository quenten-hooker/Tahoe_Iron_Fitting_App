library(shiny)

# load global functions
source("global.R")
# load server elements
source("server.R")
# load ui elements
source("ui.R")


# Run the application 
shinyApp(ui = ui, server = server)
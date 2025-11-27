# This file sources the global, ui and server code and runs the app.

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

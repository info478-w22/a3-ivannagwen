# load packages
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

require(EpiModel)


# source ui and server
source('app_ui.R')
source('app_server.R')

# run app
shinyApp(ui = ui, server = server)
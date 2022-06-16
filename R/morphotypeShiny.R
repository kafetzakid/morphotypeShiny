rm(list = ls())

library(highcharter)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(visNetwork)
library(ggplot2)
library(reshape2)
library(plotly)
library(shinyWidgets)
library(altair)
library(RCurl)
library(magick)
library(stad)
library(igraph)
library(GGally)
library(DT)

logowd = "C:/Users/u0112360/Documents/____/Sagalassos/__PhD/measurements/scripts/Shared-with-Jan-Danai/Viz_27May2020/app_input"
readwd = "C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/"

source("C:/Users/u0112360/Documents/myRpacks/morphotypeShiny/morphotypeShiny/R/get_data.R")
source("C:/Users/u0112360/Documents/myRpacks/morphotypeShiny/morphotypeShiny/R/ui.R")
source("C:/Users/u0112360/Documents/myRpacks/morphotypeShiny/morphotypeShiny/R/server.R")

shinyApp(ui = ui, server = server)




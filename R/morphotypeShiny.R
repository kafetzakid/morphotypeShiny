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

logowd = "The_logos_path"
readwd = "The_data_path"

source("~/R/get_data.R")
source("~/R/ui.R")
source("~/R/server.R")

shinyApp(ui = ui, server = server)

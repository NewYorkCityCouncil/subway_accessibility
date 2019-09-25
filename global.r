#libraries ----
library(sf)
library(leaflet)
library(councildown)
#library(htmlwidgets)
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(purrr)
#library(leaflet.extras)
#library(shinyWidgets)
options(scipen = 999)

#stops
allstops1=readRDS("data/allstops.rds")
#lines
sublines2=readRDS("data/sublines.rds")
#boroughs
bb=readRDS("data/boroughs.rds")


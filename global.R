# global.R
library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(psych)
library(lavaan)


# Source helper functions and modules
source("utils/cronbach_helpers.R")
source("modules/import_module.R")
source("modules/selection_module.R")
source("modules/grouping_module.R")
source("modules/likert_module.R")
source("modules/analysis_module.R")
source("modules/cfa_module.R")

# Reactive store
values <- reactiveValues()


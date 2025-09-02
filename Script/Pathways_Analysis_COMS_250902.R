# Title:    Analysis of CoE Students who transferred to Computer Science
# File:     Pathways_Analysis_COMS_250902.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(readxl)
# if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey") # install sankey package

# Title:    Analysis of CoE Student Pathways Data August 2025
# File:     Pathways_Analysis_250822.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(readxl)
# if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey") # install sankey package

# LOAD AND PREPARE DATA ------------------------------------
pathway_summary <- # import the previously prepared pathway_summary csv file
  read_csv("./Data/Student_Pathway_Summary_251106.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
  as_tibble()

pathway_summary <- 
  pathway_summary %>% 
  mutate(undeclared_start = if_else( # categorize whether started CoE as undeclared, 1 is yes
    (major_first == "Undeclared Engineering"), 1, 0
  )) %>% 
  relocate(undeclared_start, .after = coe_duration) # move column to better position
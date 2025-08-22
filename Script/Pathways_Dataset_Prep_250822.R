# Title:    Analysis of CoE Student Pathways Data August 2025
# File:     Pathways_Dataset_Prep_250822.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(readxl)
# if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey") # install sankey package

# LOAD AND PREPARE DATA ------------------------------------
raw_data <- # import the raw excel file
  read_excel("./Data/CoE_curriculum_request_072825.xlsx") %>% 
  as_tibble()

# RECODING OF DATE AND SEMESTER DATA -----------------------
processed_data <-
  raw_data %>% 
  mutate(sem_sequence_id = case_when(
    (year_current == '2015' & semester_current == 'Fall') ~ 1,
    (year_current == '2016' & semester_current == 'Spring') ~ 2,
    (year_current == '2016' & semester_current == 'Fall') ~ 4,
    (year_current == '2017' & semester_current == 'Spring') ~ 5,
    (year_current == '2017' & semester_current == 'Fall') ~ 7,
    (year_current == '2018' & semester_current == 'Spring') ~ 8,
    (year_current == '2018' & semester_current == 'Fall') ~ 10,
    (year_current == '2019' & semester_current == 'Spring') ~ 11,
    (year_current == '2019' & semester_current == 'Fall') ~ 13,
    (year_current == '2020' & semester_current == 'Spring') ~ 14,
    (year_current == '2020' & semester_current == 'Fall') ~ 16,
    (year_current == '2021' & semester_current == 'Spring') ~ 17,
    (year_current == '2021' & semester_current == 'Fall') ~ 19,
    (year_current == '2022' & semester_current == 'Spring') ~ 20,
    (year_current == '2022' & semester_current == 'Fall') ~ 21,
    (year_current == '2023' & semester_current == 'Spring') ~ 22,
    (year_current == '2023' & semester_current == 'Fall') ~ 23,
    (year_current == '2024' & semester_current == 'Spring') ~ 24
  )) # assign ordinal integer code to semester with Fall 2015 as start
  

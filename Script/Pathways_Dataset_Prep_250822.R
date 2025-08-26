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
    (year_current == '2015' & semester_current == 'Fall') ~ 2,
    (year_current == '2016' & semester_current == 'Spring') ~ 3,
    (year_current == '2016' & semester_current == 'Fall') ~ 5,
    (year_current == '2017' & semester_current == 'Spring') ~ 6,
    (year_current == '2017' & semester_current == 'Fall') ~ 8,
    (year_current == '2018' & semester_current == 'Spring') ~ 9,
    (year_current == '2018' & semester_current == 'Fall') ~ 11,
    (year_current == '2019' & semester_current == 'Spring') ~ 12,
    (year_current == '2019' & semester_current == 'Fall') ~ 14,
    (year_current == '2020' & semester_current == 'Spring') ~ 15,
    (year_current == '2020' & semester_current == 'Fall') ~ 17,
    (year_current == '2021' & semester_current == 'Spring') ~ 18,
    (year_current == '2021' & semester_current == 'Fall') ~ 20,
    (year_current == '2022' & semester_current == 'Spring') ~ 21,
    (year_current == '2022' & semester_current == 'Fall') ~ 23,
    (year_current == '2023' & semester_current == 'Spring') ~ 24,
    (year_current == '2023' & semester_current == 'Fall') ~ 26,
    (year_current == '2024' & semester_current == 'Spring') ~ 27
  )) %>%  # assign ordinal integer code to semester with Summer 2015 as start
  mutate(admsn_sem_id = case_when( # assign integer code to admission semester
    (admsn_term == '115') ~ 1,
    (admsn_term == 'F15') ~ 2,
    (admsn_term == 'S16') ~ 3,
    (admsn_term == '116') ~ 4,
    (admsn_term == 'F16') ~ 5,
    (admsn_term == 'S17') ~ 6,
    (admsn_term == '117') ~ 7,
    (admsn_term == 'F17') ~ 8,
    (admsn_term == 'S18') ~ 9,
    (admsn_term == '118') ~ 10,
    (admsn_term == 'F18') ~ 11,
    (admsn_term == 'S19') ~ 12,
    (admsn_term == '119') ~ 13,
    (admsn_term == 'F19') ~ 14,
    (admsn_term == 'S20') ~ 15,
    (admsn_term == '120') ~ 16,
    (admsn_term == 'F20') ~ 17,
    (admsn_term == 'S21') ~ 18,
    (admsn_term == '121') ~ 19,
    (admsn_term == 'F21') ~ 20,
    (admsn_term == 'S22') ~ 21,
    (admsn_term == '122') ~ 22,
    (admsn_term == 'F22') ~ 23,
    (admsn_term == 'S23') ~ 24,
    (admsn_term == '123') ~ 25,
    (admsn_term == 'F23') ~ 26,
    (admsn_term == 'S24') ~ 27
  ))

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
  read_excel("./Data/CoE_curriculum_request_072825_edited.xlsx") %>% 
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
  )) %>% 
  mutate(grad_sem_id = case_when( # code the graduation semester
    (degree_year == '2015' & degree_term == 'Summer') ~ 1,
    (degree_year == '2015' & degree_term == 'Fall') ~ 2,
    (degree_year == '2016' & degree_term == 'Spring') ~ 3,
    (degree_year == '2016' & degree_term == 'Summer') ~ 4,
    (degree_year == '2016' & degree_term == 'Fall') ~ 5,
    (degree_year == '2017' & degree_term == 'Spring') ~ 6,
    (degree_year == '2017' & degree_term == 'Summer') ~ 7,
    (degree_year == '2017' & degree_term == 'Fall') ~ 8,
    (degree_year == '2018' & degree_term == 'Spring') ~ 9,
    (degree_year == '2018' & degree_term == 'Summer') ~ 10,
    (degree_year == '2018' & degree_term == 'Fall') ~ 11,
    (degree_year == '2019' & degree_term == 'Spring') ~ 12,
    (degree_year == '2019' & degree_term == 'Summer') ~ 13,
    (degree_year == '2019' & degree_term == 'Fall') ~ 14,
    (degree_year == '2020' & degree_term == 'Spring') ~ 15,
    (degree_year == '2020' & degree_term == 'Summer') ~ 16,
    (degree_year == '2020' & degree_term == 'Fall') ~ 17,
    (degree_year == '2021' & degree_term == 'Spring') ~ 18,
    (degree_year == '2021' & degree_term == 'Summer') ~ 19,
    (degree_year == '2021' & degree_term == 'Fall') ~ 20,
    (degree_year == '2022' & degree_term == 'Spring') ~ 21,
    (degree_year == '2012' & degree_term == 'Summer') ~ 22,
    (degree_year == '2022' & degree_term == 'Fall') ~ 23,
    (degree_year == '2023' & degree_term == 'Spring') ~ 24,
    (degree_year == '2023' & degree_term == 'Summer') ~ 25,
    (degree_year == '2023' & degree_term == 'Fall') ~ 26,
    (degree_year == '2024' & degree_term == 'Spring') ~ 27,
    (degree_year == '2024' & degree_term == 'Summer') ~ 28,
  )) %>% 
  group_by(study_id) %>% # group by student to continue coding
  arrange(study_id, sem_sequence_id) %>%  # arrange in chronological order
  mutate(first_college = first(college_prevsem)) %>%  # assign first college at ISU start
  mutate(start_status_isu = if_else( # determine whether first ISU semester was in CoE
   (first_college == 'New' | first_college == 'Not Enrolled'),'CoE', 'non-CoE')
  ) %>% 
  mutate(major_first = first(major_current), major_second = nth(major_current,2), major_third = nth(major_current,3))  # track major changes
  # mutate(degree_program)
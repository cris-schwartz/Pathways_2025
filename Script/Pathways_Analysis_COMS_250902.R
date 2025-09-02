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

# LOAD AND PREPARE DATA ------------------------------------
resolved_students <- # import the raw csv file of the student summary data
  read_csv("./Data/Degree_Outcome_Resolved_Students_250901.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
  as_tibble()

semester_based_data <- # load the semester-by-semester data
  read_csv("./Data/Processed_Student_Dataset_250902.csv", guess_max = 1000) %>% 
  as_tibble()

coms_transfer_students <- 
  semester_based_data %>% 
  filter(major_nextsem == 'Computer Science') %>% 
  select(study_id,sem_sequence_id, academic_standing) %>%  # select fields 
  rename(transfer_sem_id = sem_sequence_id, academic_standing_at_transfer = academic_standing) %>% 
  left_join(.,resolved_students,by = "study_id") # match up summaries for each student
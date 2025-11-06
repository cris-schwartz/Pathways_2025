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

outcome_resolved <- # identify students who have a resolved degree status
  pathway_summary %>% 
  filter(degree_outcome != 'Undetermined') 
  
# ANALYSIS OF UNDECLARED STUDENTS ####
started_undeclared <- 
  outcome_resolved %>% 
  filter(undeclared_start == 1)

outcome_by_transfer_history <- # determine how many times student changed majors
  started_undeclared %>% 
  group_by(major_changes, degree_outcome) %>% 
  summarise(n = n())

outcome_by_retention <- # determine how many graduated and in what college
  started_undeclared %>% 
  group_by(degree_outcome, graduated_college) %>% 
  summarise(n = n())
  
never_declared <- # identify those who never declare
  started_undeclared %>% 
  filter(is.na(major_second))

outcome_never_declared <- # determine where never declared students go
  never_declared %>% 
  group_by(degree_outcome, graduated_college) %>% 
  summarise(n = n())
  
declared_starts <- # identify those who started with a declared CoE major
  outcome_resolved %>% 
  filter(undeclared_start == 0)

# Comparison of never declared to started declared cohorts and outcomes
outcome_resolved_never_declared_grouping <- 
  outcome_resolved %>% 
  mutate(never_declared_outcome = case_when(
    (undeclared_start == 1 & is.na(major_second)) ~ 'Never Declared',
    (undeclared_start == 0) ~ 'Started in CoE Major'
  ))

print(
plot_gpa_comparison <-
  outcome_resolved_never_declared_grouping %>%
  filter(!is.na(never_declared_outcome)) %>%
  ggplot(aes(x= first_sem_gpa, fill = never_declared_outcome)) +
    geom_density(alpha = 0.5) +
    labs (x = "First Semester GPA", y = "Density", fill = "CoE Start") +
    theme_minimal()
)
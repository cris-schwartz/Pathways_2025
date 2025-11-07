# Title:    Analysis of CoE Student Pathways Data August 2025
# File:     Pathways_Analysis_250822.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(readxl)
library(MatchIt) # load the library for the Propensity Score Matching
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
  
# OVERVIEW ANALYSIS OF UNDECLARED STUDENTS ####
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

# This is a template script for histogram with x as a continuous variable
# print(
#   plot_gpa_comparison <-
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     ggplot(aes(x= first_sem_gpa, y = ..density.., fill = never_declared_outcome)) +
#     geom_histogram(position = "identity", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "First Semester GPA", y = "Proportion of Total", fill = "CoE Start") +
#     theme_minimal()
# )
# 

# This is a template script for column chart with x as a factor variable and y
# as the proportion of the total in the cohort
# print(
#   plot_start_status_comparison <- # comparison by what college they were admitted into when started at ISU
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     mutate(start_status_isu = factor(start_status_isu)) %>% 
#     group_by(never_declared_outcome, start_status_isu) %>% 
#     summarize(count = n()) %>% 
#     ungroup() %>% 
#     group_by(never_declared_outcome) %>% 
#     mutate(proportion = count/sum(count)) %>% 
#     ggplot(aes(x = start_status_isu, y = proportion, fill = never_declared_outcome)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "ISU Entrance College", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
#   )

# print(
#   plot_sex_comparison <- # comparison of student sex
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     mutate(sex = factor(sex)) %>% 
#     group_by(never_declared_outcome, sex) %>% 
#     summarize(count = n()) %>% 
#     ungroup() %>% 
#     group_by(never_declared_outcome) %>% 
#     mutate(proportion = count/sum(count)) %>% 
#     ggplot(aes(x = sex, y = proportion, fill = never_declared_outcome)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Student Sex", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )

# print(
#   plot_ethnicity_comparison <- # comparison of student ethnicity
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     mutate(ethnicity = factor(ethnicity)) %>% 
#     group_by(never_declared_outcome, ethnicity) %>% 
#     summarize(count = n()) %>% 
#     ungroup() %>% 
#     group_by(never_declared_outcome) %>% 
#     mutate(proportion = count/sum(count)) %>% 
#     ggplot(aes(x = ethnicity, y = proportion, fill = never_declared_outcome)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Student Ethnicity", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )

# print(
#   plot_first_gen_comparison <- # comparison of first-generation student status
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     mutate(first_generation = factor(first_generation)) %>% 
#     group_by(never_declared_outcome, first_generation) %>% 
#     summarize(count = n()) %>% 
#     ungroup() %>% 
#     group_by(never_declared_outcome) %>% 
#     mutate(proportion = count/sum(count)) %>% 
#     ggplot(aes(x = first_generation, y = proportion, fill = never_declared_outcome)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "First Generation Status", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )

# print(
#   plot_residency_comparison <- # comparison of residency status
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     mutate(residency = factor(residency)) %>% 
#     group_by(never_declared_outcome, residency) %>% 
#     summarize(count = n()) %>% 
#     ungroup() %>% 
#     group_by(never_declared_outcome) %>% 
#     mutate(proportion = count/sum(count)) %>% 
#     ggplot(aes(x = residency, y = proportion, fill = never_declared_outcome)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Residency Status", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )

# print(
#   plot_adm_type_comparison <- # comparison via direct from HS vs. transfer admission
#     outcome_resolved_never_declared_grouping %>%
#     filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
#     mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
#     mutate(admission_type = factor(admission_type)) %>% 
#     group_by(never_declared_outcome, admission_type) %>% 
#     summarize(count = n()) %>% 
#     ungroup() %>% 
#     group_by(never_declared_outcome) %>% 
#     mutate(proportion = count/sum(count)) %>% 
#     ggplot(aes(x = admission_type, y = proportion, fill = never_declared_outcome)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(outcome_resolved_never_declared_grouping$never_declared_outcome)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Admission Type", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )

# 

# PROPENSITY SCORE MATCHING ASSEMBLY OF NEVER DECLARED CONTROL COHORTS ####
psm_coded_resolved <- # need to select and properly encode variables of interest
  outcome_resolved_never_declared_grouping %>% 
  filter(!is.na(never_declared_outcome)) %>% 
  select(study_id, first_sem_gpa, start_status_isu, 
         sex, ethnicity, first_generation, residency, admission_type,
         never_declared_outcome, degree_duration, degree_outcome) %>% 
  mutate(start_status_isu = factor(start_status_isu), sex = factor(sex),
         ethnicity = factor(ethnicity), first_generation = factor (first_generation),
         residency = factor(residency), admission_type = factor (admission_type),
         never_declared_outcome = factor(never_declared_outcome),
         degree_outcome = factor(degree_outcome))

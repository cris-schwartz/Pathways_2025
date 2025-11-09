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
library(cobalt)
library(broom)
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
  filter(!is.na(first_sem_gpa)) %>% 
  select(study_id, first_sem_gpa, start_status_isu, 
         sex, ethnicity, first_generation, residency, admission_type,
         never_declared_outcome, degree_duration, degree_outcome, graduated_college) %>% 
  mutate(start_status_isu = factor(start_status_isu), sex = factor(sex),
         ethnicity = factor(ethnicity), first_generation = factor (first_generation),
         residency = factor(residency), admission_type = factor (admission_type),
         never_declared_outcome = factor(never_declared_outcome),
         degree_outcome = factor(degree_outcome), graduated_college = factor(graduated_college)) %>% 
  mutate(never_declared_flag = as.integer(never_declared_outcome == 'Never Declared'))

covars <- c("first_sem_gpa", "start_status_isu", "sex", "ethnicity",
            "first_generation", "residency", "admission_type")

psm_m <- 
  psm_coded_resolved %>% 
  select(study_id, never_declared_outcome, never_declared_flag, all_of(covars), degree_duration, degree_outcome) %>% 
  drop_na(all_of(covars), never_declared_flag)

form_ps <- as.formula(paste("never_declared_flag ~", paste (covars, collapse = " + ")))

ps_mod <- glm(form_ps, data = psm_m, family = binomial())

# psm_m <- 
#   psm_m %>% 
#   mutate(ps = predict(ps_mod, type = "response"), ps_logit = qlogis(ps))

ps_vec <- predict(ps_mod, type = "response")
ps_logit_vec <- qlogis(ps_vec)

caliper_val <- 0.2 * sd(ps_logit_vec)

psm_nops <- psm_m

m_out <- matchit(
  formula = form_ps,
  data = psm_nops,
  method = "nearest",
  distance = ps_vec,
  distance2 = "logit",
  caliper = caliper_val,
  ratio = 6,
  discard = "both"
)

md <- 
  match.data(m_out) %>% 
  as_tibble() %>% 
  rename(ps = distance)

 
never_declared_ps_median <- md %>%
  filter(never_declared_flag == 1) %>%
  summarise(med = median(ps)) %>%
  pull(med)  

md <- 
  md %>% 
  mutate(
    twin_score = abs(ps - never_declared_ps_median),
    Twin_Candidate = as.integer(never_declared_flag == 0 & weights > 0)
  )

psm_out <- 
  psm_coded_resolved %>% 
  left_join(.,
    md %>% select(study_id, ps, twin_score, Twin_Candidate),
    by = "study_id") %>% 
  mutate(Twin_Candidate = replace_na(Twin_Candidate, 0L)) %>% 
  mutate(twin_candidate = factor(Twin_Candidate)) %>%  # convert to factor
  mutate(never_declared_flag = factor(never_declared_flag)) %>% 
  select(!Twin_Candidate) %>%  # remove redundant column
  filter(never_declared_flag == 1 | twin_candidate == 1) %>% 
  mutate(cohort_label = factor(if_else(
    (never_declared_flag == 1),"Never Declared", "Declared"
  )))
  
bal.tab(m_out, un = TRUE)
love.plot(m_out, thresholds = c(m= 0.1))


# 
# # This is a template script for histogram with x as a continuous variable
# print(
#   plot_psm_gpa_comparison <-
#     psm_out %>%
#     ggplot(aes(x= first_sem_gpa, y = ..density.., fill = cohort_label)) +
#     geom_histogram(position = "identity", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "First Semester GPA", y = "Proportion of Total", fill = "CoE Start") +
#     # scale_fill_manual(values = c("Declared", "Never Declared")) +
#     theme_minimal()
# )
# # 
# 
# # This is a template script for column chart with x as a factor variable and y
# # as the proportion of the total in the cohort
# print(
#   plot_psm_start_status_comparison <- # comparison by what college they were admitted into when started at ISU
#     psm_out %>%
#     group_by(cohort_label, start_status_isu) %>%
#     summarize(count = n()) %>%
#     ungroup() %>%
#     group_by(cohort_label) %>%
#     mutate(proportion = count/sum(count)) %>%
#     ggplot(aes(x = start_status_isu, y = proportion, fill = cohort_label)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "ISU Entrance College", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
#   )
# 
# print(
#   plot_psm_sex_comparison <- # comparison of student sex
#     psm_out %>%
#     group_by(cohort_label, sex) %>%
#     summarize(count = n()) %>%
#     ungroup() %>%
#     group_by(cohort_label) %>%
#     mutate(proportion = count/sum(count)) %>%
#     ggplot(aes(x = sex, y = proportion, fill = cohort_label)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Student Sex", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )
# 
# print(
#   plot_psm_ethnicity_comparison <- # comparison of student ethnicity
#     psm_out %>%
#     group_by(cohort_label, ethnicity) %>%
#     summarize(count = n()) %>%
#     ungroup() %>%
#     group_by(cohort_label) %>%
#     mutate(proportion = count/sum(count)) %>%
#     ggplot(aes(x = ethnicity, y = proportion, fill = cohort_label)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Student Ethnicity", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )
# 
# print(
#   plot_psm_first_gen_comparison <- # comparison of first-generation student status
#     psm_out %>%
#     group_by(cohort_label, first_generation) %>%
#     summarize(count = n()) %>%
#     ungroup() %>%
#     group_by(cohort_label) %>%
#     mutate(proportion = count/sum(count)) %>%
#     ggplot(aes(x = first_generation, y = proportion, fill = cohort_label)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "First Generation Status", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )
# 
# print(
#   plot_psm_residency_comparison <- # comparison of residency status
#     psm_out %>%
#     group_by(cohort_label, residency) %>%
#     summarize(count = n()) %>%
#     ungroup() %>%
#     group_by(cohort_label) %>%
#     mutate(proportion = count/sum(count)) %>%
#     ggplot(aes(x = residency, y = proportion, fill = cohort_label)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Residency Status", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )
# 
# print(
#   plot_psm_adm_type_comparison <- # comparison via direct from HS vs. transfer admission
#     psm_out %>%
#     group_by(cohort_label, admission_type) %>%
#     summarize(count = n()) %>%
#     ungroup() %>%
#     group_by(cohort_label) %>%
#     mutate(proportion = count/sum(count)) %>%
#     ggplot(aes(x = admission_type, y = proportion, fill = cohort_label)) +
#     geom_col(position = "dodge", alpha = 0.5) +
#     scale_fill_discrete( # calculate sample size to add to legend
#       labels = function(x) { # creates a label entry based on calculation of sample size
#         n_vals <- table(psm_out$cohort_label)
#         paste0(x, " (n = ", n_vals[x], ")")
#       }
#     ) +
#     labs (x = "Admission Type", y = "Proportion of Cohort", fill = "CoE Start") +
#     theme_minimal()
# )

# Analysis of success outcomes for declared twin cohort vs never declareds

never_declared_degree_comparison <- # compared degree completion rates with twin cohort
  psm_out %>% 
  group_by(cohort_label, degree_outcome, graduated_college) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(cohort_label) %>% 
  summarize(degree_outcome = degree_outcome, graduated_college = graduated_college,
            count = count, proportion = count/sum(count))

never_declared_duration_comparison <- # compare degree completion time with twin cohort
  psm_out %>%
  filter(degree_outcome == "Degree") %>% 
  group_by(cohort_label) %>% 
  summarize(count = n(), mean_duration = mean(degree_duration))

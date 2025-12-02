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
library(patchwork)
library(glue)
library(ggalluvial)
library(tidygraph)
library(ggraph)
if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey") # install sankey package
library(ggsankey)

# LOAD AND PREPARE DATA ------------------------------------
pathway_summary <- # import the previously prepared pathway_summary csv file
  read_csv("./Data/Student_Pathway_Summary_251114.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
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

# ANALYSIS OF STUDENTS WHO NEVER DECLARED COE MAJOR ####
never_declared_analysis = FALSE # set a switch to run the analysis
if (never_declared_analysis == TRUE) {
# Comparison of never declared to started declared cohorts and outcomes
outcome_resolved_never_declared_grouping <- 
  outcome_resolved %>% 
  mutate(never_declared_outcome = case_when(
    (undeclared_start == 1 & is.na(major_second)) ~ 'Never Declared',
    (undeclared_start == 0) ~ 'Started in CoE Major'
  ))

# Set default color options to ensure good contrast if printed in grayscale
options(ggplot2.discrete.fill = c("#1F78B4", "#E69F00", "#33A02C"))
options(ggplot2.discrete.color = c("#1F78B4", "#E69F00", "#33A02C"))

## This is a template script for histogram with x as a continuous variable
outcome_resolved_with_gpa <- # because first semester gpa is a point of comparison, only select students with a first sem gpa
  outcome_resolved_never_declared_grouping %>% 
  filter(!is.na(first_sem_gpa))
  
plot_gpa_comparison <-
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    ggplot(aes(x= first_sem_gpa, y = after_stat(density), fill = never_declared_outcome)) +
    # ggplot(aes(x= first_sem_gpa, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "First Semester GPA", y = "Proportion of Cohort", fill = "CoE Start") + 
    theme_minimal()

## This is a template script for column chart with x as a factor variable and y
## as the proportion of the total in the cohort
plot_start_status_comparison <- # comparison by what college they were admitted into when started at ISU
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(start_status_isu = factor(start_status_isu)) %>%
    group_by(never_declared_outcome, start_status_isu) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = start_status_isu, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "ISU College of Admittance", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()

plot_sex_comparison <- # comparison of student sex
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(sex = factor(sex)) %>%
    group_by(never_declared_outcome, sex) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = sex, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Student Sex", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()


plot_ethnicity_comparison <- # comparison of student ethnicity
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(ethnicity = case_when(
      (ethnicity == "American Indian or Alaska Native") ~ "A.In.",
      (ethnicity == "Asian") ~ "As.",
      (ethnicity == "Black or African American") ~ "Af.Am.",
      (ethnicity == "Hispanic") ~ "His.",
      (ethnicity == "International") ~ "Int.",
      (ethnicity == "Native Hawaiian or Other Pacific Islander") ~ "NaHa.",
      (ethnicity == "Two or more races") ~ "2/mo",
      (ethnicity == "Unknown race and ethnicity") ~ "Un.",
      (ethnicity == "White") ~ "Wh."
      
    )) %>% 
    mutate(ethnicity = factor(ethnicity)) %>%
    group_by(never_declared_outcome, ethnicity) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = ethnicity, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Student Ethnicity", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot_first_gen_comparison <- # comparison of first-generation student status
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(first_generation = factor(first_generation)) %>%
    group_by(never_declared_outcome, first_generation) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    filter(first_generation == 1) %>% 
    ggplot(aes(x = first_generation, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "First Generation Status", y = "Proportion of Cohort", fill = "CoE Start") +
    scale_x_discrete(labels = NULL) +
    theme_minimal()


plot_residency_comparison <- # comparison of residency status
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(residency = factor(residency)) %>%
    group_by(never_declared_outcome, residency) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = residency, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Residency Status", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()


plot_adm_type_comparison <- # comparison via direct from HS vs. transfer admission
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(admission_type = factor(admission_type)) %>%
    group_by(never_declared_outcome, admission_type) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = admission_type, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Admission Type", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()


plot_grad_status_comparison <- # comparison of graduation rates
    outcome_resolved_with_gpa %>%
    filter(!is.na(never_declared_outcome)) %>% # get rid of rows that are not in either group
    mutate(never_declared_outcome = factor(never_declared_outcome)) %>% # change to factor for counting samples
    mutate(degree_outcome = factor(degree_outcome)) %>%
    group_by(never_declared_outcome, degree_outcome) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(never_declared_outcome) %>%
    mutate(proportion = count/sum(count)) %>%
    filter(degree_outcome == 'Degree') %>% 
    ggplot(aes(x = degree_outcome, y = proportion, fill = never_declared_outcome)) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(outcome_resolved_with_gpa$never_declared_outcome)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "ISU Degree Completion (any degree)", y = "Proportion who earned degree", fill = "CoE Start") +
    scale_x_discrete(labels = NULL) +
    theme_minimal()

# combine all into a single figure    
plot_gpa_comparison <- plot_gpa_comparison + guides(fill = "none") # turn off legend of gpa plot so patchwork does not count it as different
print(plot_gpa_comparison + plot_start_status_comparison + plot_sex_comparison + plot_ethnicity_comparison + plot_first_gen_comparison +
  plot_residency_comparison + plot_adm_type_comparison + plot_grad_status_comparison + guide_area() +
  plot_layout(ncol = 3, axes = "collect", guides = "collect") +
  plot_annotation(title = "Demographics and graduation outcomes of CoE Students who never declared a CoE major"))



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
  ratio = 5,
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
    (never_declared_flag == 1),"Never Declared", "'Twins' who Declared"
  )))
  
bal.tab(m_out, un = TRUE)
love.plot(m_out, thresholds = c(m= 0.1))

#reset color options for psm plots
options(ggplot2.discrete.fill = c("#1F78B4", "#c09f00", "#33A02C"))
options(ggplot2.discrete.color = c("#1F78B4", "#c09f00", "#33A02C"))

plot_psm_gpa_comparison <-
    psm_out %>%
    ggplot(aes(x= first_sem_gpa, y = after_stat(density), fill = fct_rev(cohort_label))) + # fct_rev makes order same as earlier plots
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "First Semester GPA", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()

plot_psm_start_status_comparison <- # comparison by what college they were admitted into when started at ISU
    psm_out %>%
    group_by(cohort_label, start_status_isu) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(cohort_label) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = start_status_isu, y = proportion, fill = fct_rev(cohort_label))) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "ISU College of Admittance", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()

plot_psm_sex_comparison <- # comparison of student sex
    psm_out %>%
    group_by(cohort_label, sex) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(cohort_label) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = sex, y = proportion, fill = fct_rev(cohort_label))) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Student Sex", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()

plot_psm_ethnicity_comparison <- # comparison of student ethnicity
    psm_out %>%
    mutate(ethnicity = case_when(
      (ethnicity == "American Indian or Alaska Native") ~ "A.In.",
      (ethnicity == "Asian") ~ "As.",
      (ethnicity == "Black or African American") ~ "Af.Am.",
      (ethnicity == "Hispanic") ~ "His.",
      (ethnicity == "International") ~ "Int.",
      (ethnicity == "Native Hawaiian or Other Pacific Islander") ~ "NaHa.",
      (ethnicity == "Two or more races") ~ "2/mo",
      (ethnicity == "Unknown race and ethnicity") ~ "Un.",
      (ethnicity == "White") ~ "Wh."
    )) %>% 
    mutate(ethnicity = factor(ethnicity)) %>%
    group_by(cohort_label, ethnicity) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(cohort_label) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = ethnicity, y = proportion, fill = fct_rev(cohort_label))) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Student Ethnicity", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
plot_psm_first_gen_comparison <- # comparison of first-generation student status
    psm_out %>%
    group_by(cohort_label, first_generation) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(cohort_label) %>%
    mutate(proportion = count/sum(count)) %>%
    filter(first_generation == 1) %>% 
    ggplot(aes(x = first_generation, y = proportion, fill = fct_rev(cohort_label))) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "First Generation Status", y = "Proportion of Cohort", fill = "CoE Start") +
    scale_x_discrete(labels = NULL) +
    theme_minimal()

plot_psm_residency_comparison <- # comparison of residency status
    psm_out %>%
    group_by(cohort_label, residency) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(cohort_label) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = residency, y = proportion, fill = fct_rev(cohort_label))) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Residency Status", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()

plot_psm_adm_type_comparison <- # comparison via direct from HS vs. transfer admission
    psm_out %>%
    group_by(cohort_label, admission_type) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(cohort_label) %>%
    mutate(proportion = count/sum(count)) %>%
    ggplot(aes(x = admission_type, y = proportion, fill = fct_rev(cohort_label))) +
    ylim(0,1) + # set y axis limits from 0 to 1
    geom_col(position = "dodge", alpha = 1) +
    scale_fill_discrete( # calculate sample size to add to legend
      labels = function(x) { # creates a label entry based on calculation of sample size
        n_vals <- table(psm_out$cohort_label)
        paste0(x, " (n = ", n_vals[x], ")")
      }
    ) +
    labs (x = "Admission Type", y = "Proportion of Cohort", fill = "CoE Start") +
    theme_minimal()

plot_psm_grad_status_comparison <- # comparison of graduation rates
  psm_out %>%
  group_by(cohort_label, degree_outcome) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(cohort_label) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(degree_outcome == 'Degree') %>% 
  ggplot(aes(x = degree_outcome, y = proportion, fill = fct_rev(cohort_label))) +
  ylim(0,1) + # set y axis limits from 0 to 1
  geom_col(position = "dodge", alpha = 1) +
  scale_fill_discrete( # calculate sample size to add to legend
    labels = function(x) { # creates a label entry based on calculation of sample size
      n_vals <- table(psm_out$cohort_label)
      paste0(x, " (n = ", n_vals[x], ")")
    }
  ) +
  labs (x = "ISU Degree Completion (any degree)", y = "Proportion who earned degree", fill = "CoE Start") +
  scale_x_discrete(labels = NULL) +
  theme_minimal()

# combine all into a single figure    
plot_psm_gpa_comparison <- plot_psm_gpa_comparison + guides(fill = "none") # turn off legend of gpa plot so patchwork does not count it as different
print(plot_psm_gpa_comparison + plot_psm_start_status_comparison + plot_psm_sex_comparison + plot_psm_ethnicity_comparison + plot_psm_first_gen_comparison +
  plot_psm_residency_comparison + plot_psm_adm_type_comparison + plot_psm_grad_status_comparison + guide_area() +
  plot_layout(ncol = 3, axes = "collect", guides = "collect") +
  plot_annotation(title = "Comparison of never declared students with CoE demographic 'twins' "))


never_declared_degree_comparison <- # compared degree completion rates with twin cohort
  psm_out %>% 
  group_by(cohort_label, degree_outcome, graduated_college) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(cohort_label) %>% 
  mutate(proportion = count/sum(count))

never_declared_duration_comparison <- # compare degree completion time with twin cohort
  psm_out %>%
  filter(degree_outcome == "Degree") %>% 
  group_by(cohort_label) %>% 
  summarize(count = n(), mean_duration = mean(degree_duration)) 
}

# STUDY OF COE DURATION AND DEGREE OUTCOME FOR UNDECLARED STARTS ####
outcomes_duration_analysis = FALSE # set switch to run analysis
if (outcomes_duration_analysis == TRUE){
outcomes_duration_normalized <- # structure data to plot results vs. number of semesters in CoE
  outcome_resolved %>% 
  mutate(coe_duration = factor(coe_duration), undeclared_start = factor(undeclared_start),
         major_first = factor(major_first), start_status_isu = factor(start_status_isu), admission_type = factor(admission_type),
         grad_status_dataset = factor(grad_status_dataset), degree_outcome = factor(degree_outcome))

# Reset default color options
options(ggplot2.discrete.fill = c("#1f78ff", "#E69F00", "#33A02C"))
options(ggplot2.discrete.color = c("#1f78ff", "#E69F00", "#33A02C"))

# Added functionality to plot outcomes by major or other defined group

cohort_outcomes_study = 0 # set the trigger to analyze by specified cohort
if (cohort_outcomes_study == 1) { # modify the tibble by filtering if necessary
  outcomes_duration_normalized <- 
  outcomes_duration_normalized %>%
  mutate(coe_program = if_else( # get assigned to program if started undeclared
    (undeclared_start == 0),major_first,major_second
  )) %>% 
  filter(coe_program == 'Software Engineering') # define the cohort
  major_declared <- first(outcomes_duration_normalized$coe_program)
  
} else { # cohort study not flagged, revert to all started declared
  major_declared = 'Declared Major'
  }



plot_isu_degree_outcome_by_coe_duration <- # get plot of ISU degree outcome performance by number of semesters spent in CoE
  outcomes_duration_normalized %>% 
  group_by(coe_duration, undeclared_start, degree_outcome) %>% 
  summarize(count = n()) %>% 
  ungroup %>%
  group_by(coe_duration, undeclared_start) %>% 
  mutate(grad_prop_isu = count/sum(count)) %>% 
  filter(degree_outcome == 'Degree') %>% 
  ungroup() %>% 
  complete(coe_duration, undeclared_start, fill = list(degree_outcome = 'Degree', count = 0, grad_prop_isu = 0)) %>% # fill in missing rows for plot
  mutate(undeclared_start = if_else(
    (undeclared_start == 0), major_declared, 'Undeclared'
  )) %>% 
  ggplot(aes(x = coe_duration, y = grad_prop_isu, fill = fct_rev(undeclared_start))) +
  ylim(0,1) +
  geom_col(position = "dodge") +
  (if (cohort_outcomes_study == 0) {
    labs(y = "Proportion of CoE students earning any ISU degree")
  } else {
    labs(y = glue("Proportion of {major_declared} students earning any ISU degree"))
  })+
  labs(x = "Number of semesters that student spent in CoE", fill = "First CoE Semester") +
  theme_minimal()

isu_degree_outcomes_summary <- # summary table of population sizes to accompany plot
  outcomes_duration_normalized %>% 
  group_by(coe_duration, undeclared_start, degree_outcome) %>% 
  summarize(count = n()) %>% 
  ungroup %>%
  group_by(coe_duration, undeclared_start) %>%
  mutate(total_students = sum(count)) %>%
  filter(degree_outcome == 'Degree')

isu_degrees_outcomes_admission_type <- # calculate total numbers of transfer students within these groups
  outcomes_duration_normalized %>% 
  group_by(coe_duration, admission_type) %>% 
  summarise(count = n())

plot_coe_degree_outcome_by_coe_duration <- # get plot of ISU degree outcome performance by number of semesters spent in CoE
  outcomes_duration_normalized %>% 
  group_by(coe_duration, undeclared_start, grad_status_dataset) %>% 
  summarize(count = n()) %>% 
  ungroup %>%
  group_by(coe_duration, undeclared_start) %>% 
  mutate(grad_prop_coe = count/sum(count)) %>% 
  filter(grad_status_dataset == 'Engineering Degree') %>% 
  ungroup() %>% 
  complete(coe_duration, undeclared_start, fill = list(grad_status_dataset = 'Engineering Degree', count = 0, grad_prop_coe = 0)) %>% # fill in missing rows for plot
  mutate(undeclared_start = if_else(
    (undeclared_start == 0), major_declared, 'Undeclared'
  )) %>% 
  ggplot(aes(x = coe_duration, y = grad_prop_coe, fill = fct_rev(undeclared_start))) +
  ylim(0,1) +
  geom_col(position = "dodge") +
  (if (cohort_outcomes_study == 0) {
    labs(y = "Proportion of CoE students earning CoE degree")
  } else {
    labs(y = glue("Proportion of {major_declared} students earning {major_declared} degree"))
  })+
  labs (x = "Number of semesters that student spent in CoE", fill = "First CoE Semester") +
  theme_minimal()  

coe_degree_outcomes_summary <- # summary table of population sizes to accompany plot
  outcomes_duration_normalized %>% 
  group_by(coe_duration, undeclared_start, grad_status_dataset) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  group_by(coe_duration, undeclared_start) %>%
  mutate(total_students = sum(count)) %>%
  filter(grad_status_dataset == 'Engineering Degree')

print(plot_isu_degree_outcome_by_coe_duration + plot_coe_degree_outcome_by_coe_duration + # combine into single figure
        plot_layout(axes = "collect", guides = "collect") +
        plot_annotation(title = glue("Graduation outcomes for students starting in {major_declared} based on semesters spent in CoE")))

}

# STUDY OF THE PATHWAYS HISTORY OF UNDECLARED STARTS ####
undeclared_pathway_history_analysis = FALSE # set switch to run analysis
if(undeclared_pathway_history_analysis == TRUE){
  outcome_resolved_pathway <- 
    outcome_resolved %>% 
    mutate(pathway_history = case_when( # being building pathway string for undeclareds
      (undeclared_start == 1) ~ "S1U:" 
    ))

  semester_records <- # import the previously prepared processed_student_dataset csv file
    read_csv("./Data/Processed_Student_Dataset_251114.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
    as_tibble()
    
  pathways_undeclared <- # encode full pathway history for undeclared students
    semester_records %>%   
    semi_join(.,filter(outcome_resolved_pathway,!is.na(pathway_history)), by = 'study_id') %>%  # select only records for undeclareds
    arrange(study_id, sem_sequence_id) %>%  # put record in chronological order based on semester
    group_by(study_id) %>% 
    mutate(sem_2_major = nth(major_current,2), sem_3_major = nth(major_current,3)) %>% # capture the majors for semesters 2 and 3
    mutate(grad_outcome = grad_status_dataset) %>%   # duplicate row to make it easier to work with visualizations
    slice_head() %>%  # retain only the first row of records for each student
    select(study_id,sem_2_major,sem_3_major,grad_outcome) %>% 
    left_join(outcome_resolved_pathway,.,by = 'study_id') %>% 
    relocate(c(sem_2_major,sem_3_major,grad_outcome, pathway_history), .after = study_id) %>%  # move to more convenient spot
    filter(!is.na(pathway_history)) %>% # select only undeclareds
    mutate(sem_2_major = case_when( # properly assign NA's for semester 2 data
      (is.na(sem_2_major) & (grad_status_dataset == 'No Degree')) ~ 'Departed',
      (is.na(sem_2_major) & (grad_status_dataset == 'Non-Engineering Degree')) ~ 'Non-Engineering',
      (!is.na(sem_2_major)) ~ sem_2_major
    )) %>% 
    mutate(sem_3_major = case_when( # properly assign NA's for semester 3 data
      (is.na(sem_3_major) & (grad_status_dataset == 'No Degree')) ~ 'Departed',
      (is.na(sem_3_major) & (grad_status_dataset == 'Non-Engineering Degree')) ~ 'Non-Engineering',
      (!is.na(sem_3_major)) ~ sem_3_major
    )) %>%   
    mutate(pathway_history = str_c(pathway_history,"S2")) %>% # build the next entry in history string
    mutate(pathway_history = case_when(
      (sem_2_major == "Undeclared Engineering") ~ str_c(pathway_history,"U:"),
      (str_detect(sem_2_major,' Engineering')) ~ str_c(pathway_history,"E:"),
      (sem_2_major == 'Non-Engineering') ~ str_c(pathway_history,"N:"),
      (sem_2_major == 'Departed') ~ str_c(pathway_history,"D:")
    )) %>% 
    mutate(pathway_history = str_c(pathway_history,"S3")) %>% # build the next entry in history string
    mutate(pathway_history = case_when(
      (sem_3_major == "Undeclared Engineering") ~ str_c(pathway_history,"U:"),
      (str_detect(sem_3_major,' Engineering')) ~ str_c(pathway_history,"E:"),
      (sem_3_major == 'Non-Engineering') ~ str_c(pathway_history,"N:"),
      (sem_3_major == 'Departed') ~ str_c(pathway_history,"D:")
    )) %>% 
    mutate(pathway_history = str_c(pathway_history,"G")) %>% # final entry for graduation result
    mutate(pathway_history = case_when(
      (grad_status_dataset == 'Engineering Degree') ~ str_c(pathway_history,"E"),
      (grad_status_dataset == 'Non-Engineering Degree') ~ str_c(pathway_history,"N"),
      (grad_status_dataset == 'No Degree') ~ str_c(pathway_history,"D")
    )) %>% 
    mutate(sem_3_major = if_else(
     (sem_2_major == 'Departed'),NA,sem_3_major 
    )) %>% 
    mutate(grad_outcome = if_else(
      (sem_3_major == 'Departed' | is.na(sem_3_major)), NA, grad_outcome
    )) %>% 
    mutate(gpa_change = first_sem_gpa - final_coe_gpa) # add metric to show GPA change from start to leaving CoE

  pathways_undeclared_steps <- # process the pathways to individual steps for later restructuring
    pathways_undeclared %>% 
    separate(col = pathway_history, into = c("step1", "step2", "step3", "step4"), sep = ":") %>%   # prep for network visualization
    mutate(step2 = str_c(step1, step2, sep = ":")) %>% 
    mutate(step3 = str_c(step2, step3, sep = ":")) %>% 
    mutate(step4 = str_c(step3, step4, sep = ":")) %>%
    mutate(step3 = if_else(
      (str_detect(step2,'D')),NA,step3)
    ) %>% 
    mutate(step4 = if_else(
      (str_detect(step3,'D') | is.na(step3)),NA,step4)
    ) %>% 
    mutate(step1 = "Started Undeclared") %>%
    mutate(step2 = case_when(
      (step2 == 'S1U:S2U') ~ 'Stayed Undeclared (S2)',
      (step2 == 'S1U:S2N') ~ 'Left CoE, earned ISU degree (S2)',
      (step2 == 'S1U:S2E') ~ 'Declared CoE major (S2)',
      (step2 == 'S1U:S2D') ~ 'Left ISU (S2)'
    )) %>%
    mutate(step3 = case_when(
      (step3 == 'S1U:S2U:S3U') ~ 'Stayed Undeclared (S3)',
      (step3 == 'S1U:S2U:S3N') ~ 'Declared non-CoE major (S3)',
      (step3 == 'S1U:S2U:S3E') ~ 'Declared CoE major (S3)',
      (step3 == 'S1U:S2U:S3D') ~ 'Left ISU (S3)',
      (step3 == 'S1U:S2N:S3N') ~ 'Still in non-CoE major (S3)',
      (step3 == 'S1U:S2E:S3U') ~ 'Back to Undeclared (S3)',
      (step3 == 'S1U:S2E:S3E') ~ 'Still in CoE major (S3)',
      (step3 == 'S1U:S2E:S3D') ~ 'Declared, Left ISU (S3)',
      (step3 == 'S1U:S2E:S3N') ~ 'Left for non-CoE major (S3)'
    )) %>%
    mutate(step4 = case_when(
      (step4 == 'S1U:S2E:S3E:GE') ~ 'CoE Degree*',
      (step4 == 'S1U:S2E:S3E:GD') ~ 'Left ISU*',
      (step4 == 'S1U:S2E:S3N:GN') ~ 'non-CoE Degree ,',
      (step4 == 'S1U:S2U:S3E:GD') ~ 'Left ISU_',
      (step4 == 'S1U:S2U:S3E:GE') ~ 'CoE Degree_',
      (step4 == 'S1U:S2U:S3D:GD') ~ 'Sem3 Left ISU',
      (step4 == 'S1U:S2D:S3D:GD') ~ 'Sem2 Left ISU',
      (step4 == 'S1U:S2U:S3U:GD') ~ 'Left ISU .',
      (step4 == 'S1U:S2E:S3E:GN') ~ 'non-CoE Degree*',
      (step4 == 'S1U:S2N:S3N:GN') ~ 'non-CoE Degree,',
      (step4 == 'S1U:S2E:S3D:GD') ~ 'Left ISU .',
      (step4 == 'S1U:S2U:S3E:GN') ~ 'non-CoE Degree_',
      (step4 == 'S1U:S2U:S3U:GE') ~ 'CoE Degree .',
      (step4 == 'S1U:S2E:S3U:GD') ~ 'Left ISU ,',
      (step4 == 'S1U:S2U:S3N:GN') ~ 'non-CoE Degree.',
      (step4 == 'S1U:S2U:S3U:GN') ~ 'non-CoE Degree .'
    )) 

  tree_structure_pathways_undeclared <- 
    pathways_undeclared_steps %>% 
    pivot_longer(cols = starts_with("step"), names_to = "step_index", values_to = "state") %>%    # rows by student-semester
    arrange(study_id, step_index) %>% 
    group_by(study_id) %>% 
    mutate(next_state = lead(state), # record info on next state for each student-semester
           next_step = lead(step_index)) %>% 
    ungroup() %>% 
    filter(!is.na(next_state)) %>% # remove ends of record for each student
    relocate(last_col(3):last_col(), .after = study_id)  # make tibble easier to examine visually

  tree_edge_counts <- # determine the network edge weights (aka number of students on each leg of pathway)
    tree_structure_pathways_undeclared %>% 
    group_by(state, next_state) %>% 
    summarize(n_students = n(), first_gpa = mean(first_sem_gpa, na.rm = TRUE),
              gpa_change = mean(gpa_change, na.rm = TRUE)) # count graph edge weights and mapping factor gpa
    
  tree_edges_graph <- # need to change name of colums to prepare for creation of tidygraph object
    tree_edge_counts %>% 
    rename(from = state, to = next_state)
 
  tree_graph_pathways_undeclared <- tbl_graph( # build a tidygraph object manually using edge_graph df
    nodes = NULL,
    edges = tree_edges_graph, # indicate the edge info
    directed = TRUE
    ) %>% 
    activate(nodes) %>% # select the nodes df to manipulate
    mutate(state = name) # change the column name

  plot_tree_graph_pathways_undeclared_first_sem_gpa <- # plot pathways by first semester GPA
    tree_graph_pathways_undeclared %>%
    ggraph(layout = "tree") + # tree diagram (igraph layout, not same as ggraph 'treemap' layout)
    geom_edge_diagonal(aes(edge_width = n_students, edge_colour = first_gpa), alpha = 1, lineend = "round",
                       linejoin = "round", strength = 0.5) + # define graph edges
    scale_edge_color_gradient(name = "mean first semester GPA", low = "red", high = "darkgreen", trans = "exp") + # better colors
    geom_node_label(aes(label = str_wrap(state, width = 20)), size = 5, fill = "white") + # format node labels
    scale_edge_width (range = c(0.5, 30), guide = "none") + # edge size scale and turn off legend for edge width
    coord_flip(clip = "off") + # swap to horizontal layout and make room so labels are not clipped when plotted
    scale_y_reverse() + # flip horizontally to get root on the left
    theme_void() + # turn off background
    theme(legend.position = "top", 
          axis.title.x = element_text()) + # move legend
    labs(title = "Pathways of CoE Students who Started Undeclared 2015 - 2024") + # add plot title
    labs(y = "Progression by semester of study at ISU (first semester is when student entered Engineering Undeclared)") +
    theme(plot.margin = margin(20, 80, 20, 80, "pt")) # pad the margins so that node labels not clipped when plotted

  # print(plot_tree_graph_pathways_undeclared_first_sem_gpa)
  
  plot_tree_graph_pathways_undeclared_gpa_change <- # plot pathways by evolution of GPA
    tree_graph_pathways_undeclared %>%
    ggraph(layout = "tree") + # tree diagram (igraph layout, not same as ggraph 'treemap' layout)
    geom_edge_diagonal(aes(edge_width = n_students, edge_colour = gpa_change), alpha = 1, lineend = "round",
                       linejoin = "round", strength = 0.5) + # define graph edges
    scale_edge_colour_gradient2( # visualize improvement vs degradation of GPA
      name      = "Change in GPA from start to CoE departure",
      low       = "red",       # negative
      mid       = "gray90",     # zero
      high      = "darkgreen", # positive
      midpoint  = 0,           # where 0 lies on the scale
      limits    = c(-0.55, 0.55) # force full range to show
    ) +
      geom_node_label(aes(label = str_wrap(state, width = 20)), size = 5, fill = "white") + # format node labels
    scale_edge_width (range = c(0.5, 30), guide = "none") + # edge size scale and turn off legend for edge width
    coord_flip(clip = "off") + # swap to horizontal layout and make room so labels are not clipped when plotted
    scale_y_reverse() + # flip horizontally to get root on the left
    theme_void() + # turn off background
    theme(legend.position = "top", 
          axis.title.x = element_text()) + # move legend
    labs(title = "Pathways of CoE Students who Started Undeclared 2015 - 2024") + # add plot title
    labs(y = "Progression by semester of study at ISU (first semester is when student entered Engineering Undeclared)") +
    theme(plot.margin = margin(20, 80, 20, 80, "pt")) # pad the margins so that node labels not clipped when plotted
  
  # print(plot_tree_graph_pathways_undeclared_gpa_change)

  # print(plot_tree_graph_pathways_undeclared_first_sem_gpa / plot_tree_graph_pathways_undeclared_gpa_change + # combine into single figure
  #         plot_layout(axes = "collect", guides = "collect") +
  #         plot_annotation(title = "Pathways of CoE Students who Started Undeclared 2015 - 2024"))
  
  sankey_structure_pathways_undeclared <- # duplicate the tree layout using a Sankey
    pathways_undeclared %>% 
    separate(col = pathway_history, into = c("step1", "step2", "step3", "step4"), sep = ":") %>%   # prep for network visualization
    mutate(step2 = str_c(step1, step2, sep = ":")) %>% 
    mutate(step3 = str_c(step2, step3, sep = ":")) %>% 
    mutate(step4 = str_c(step3, step4, sep = ":")) %>%
    mutate(step3 = if_else(
      (str_detect(step2,'D')),NA,step3)
    ) %>% 
    mutate(step4 = if_else(
      (str_detect(step3,'D') | is.na(step3)),NA,step4)
    ) %>% 
    mutate(step1 = "Started Undeclared") %>%
    mutate(step2 = case_when(
      (step2 == 'S1U:S2U') ~ 'Remained Undeclared',
      (step2 == 'S1U:S2N') ~ 'Left CoE, earned ISU degree',
      (step2 == 'S1U:S2E') ~ 'Declared CoE major',
      (step2 == 'S1U:S2D') ~ 'z_Left ISU'
    )) %>%
    mutate(step3 = case_when(
      (step3 == 'S1U:S2U:S3U') ~ 'Remained Undeclared',
      (step3 == 'S1U:S2U:S3N') ~ 'Declared non-CoE major',
      (step3 == 'S1U:S2U:S3E') ~ 'Declared CoE major',
      (step3 == 'S1U:S2U:S3D') ~ 'Left ISU',
      (step3 == 'S1U:S2N:S3N') ~ 'Continued non-CoE major',
      (step3 == 'S1U:S2E:S3U') ~ 'Back to Undeclared',
      (step3 == 'S1U:S2E:S3E') ~ 'Remained in CoE major',
      (step3 == 'S1U:S2E:S3D') ~ 'Declared, Left ISU',
      (step3 == 'S1U:S2E:S3N') ~ 'Declared non-CoE major'
    )) %>%
    mutate(step4 = case_when(
      (step4 == 'S1U:S2E:S3E:GE') ~ 'CoE Degree',
      (step4 == 'S1U:S2E:S3E:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2E:S3N:GN') ~ 'non-CoE Degree',
      (step4 == 'S1U:S2U:S3E:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2U:S3E:GE') ~ 'CoE Degree',
      (step4 == 'S1U:S2U:S3D:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2D:S3D:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2U:S3U:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2E:S3E:GN') ~ 'non-CoE Degree',
      (step4 == 'S1U:S2N:S3N:GN') ~ 'non-CoE Degree',
      (step4 == 'S1U:S2E:S3D:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2U:S3E:GN') ~ 'non-CoE Degree',
      (step4 == 'S1U:S2U:S3U:GE') ~ 'CoE Degree',
      (step4 == 'S1U:S2E:S3U:GD') ~ 'Left ISU',
      (step4 == 'S1U:S2U:S3N:GN') ~ 'non-CoE Degree',
      (step4 == 'S1U:S2U:S3U:GN') ~ 'non-CoE Degree'
    ))
 
  gpa_pathways_undeclared <- # get summaries to use for sankey fill
    pathways_undeclared %>% 
    group_by(pathway_history) %>% 
    summarize(cohort_first_gpa = mean(first_sem_gpa, na.rm = TRUE)) # mean first semester GPA of all with same pathway
  
  gpa_summary_joined <- 
    pathways_undeclared %>% 
    left_join(.,gpa_pathways_undeclared, by = "pathway_history") %>% 
    relocate(cohort_first_gpa, .after = pathway_history) %>% 
    select(study_id, cohort_first_gpa) # retain only the columns needed for the join
  
  sankey_structure_pathways_undeclared <- 
    sankey_structure_pathways_undeclared %>% 
    left_join(.,gpa_summary_joined, by = "study_id") %>% 
    relocate(cohort_first_gpa, .after = step4) # relocate to make examination easier
   
  sankey_long_format <- # convert to ggsankey format using make_long
    sankey_structure_pathways_undeclared %>%
    make_long(step1, step2, step3, step4, value = cohort_first_gpa) %>% # reformat and keep the desired mapping fill paramter
    filter(!is.na(node), node !="") %>%   # get rid of all NA nodes due to those who do not have all four steps
    rename(mean_first_gpa = value)  # rename the column so no information is lost
 
  # sankey_pal <- colorRampPalette(c("yellow","darkgreen"))
  # sankey_cols <- sankey_pal(nlevels(factor(sankey_long_format$node)))
    
  plot_sankey_pathways_undeclared <-
    sankey_long_format %>% 
    ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node))) +
    geom_sankey(flow.alpha = .8, node.color = 'gray90', show.legend = FALSE) +
    # scale_fill_manual(values = sankey_cols) +
    geom_sankey_label(aes(label = node), size = 3, color = 'black', fill = 'gray90') +
    theme_minimal() +
    theme(legend.position = 'none') +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),  
          panel.grid = element_blank()) +
    scale_x_discrete(labels = c("first semester","second","third","final outcome")) +
    labs(x = "Semester of Study at ISU (based on semester when began in Engineering Undeclared)")

  
  # print(plot_sankey_pathways_undeclared)
  
  # column visualization for comparison of outcome by semester CoE major was declared
  declaration_time_outcome <- 
    pathways_undeclared %>% 
    mutate(grad_status_dataset = factor(grad_status_dataset)) %>% 
    separate(col = pathway_history, into = c("step1", "step2", "step3", "step4"), sep = ":") %>%   # prep for network visualization
    mutate(step2 = str_c(step1, step2, sep = ":")) %>% 
    mutate(step3 = str_c(step2, step3, sep = ":")) %>% 
    mutate(step4 = str_c(step3, step4, sep = ":")) %>%
    mutate(step3 = if_else(
      (str_detect(step2,'D')),NA,step3)
    ) %>% 
    mutate(step4 = if_else(
      (str_detect(step3,'D') | is.na(step3)),NA,step4)
    ) %>% 
    mutate(undeclared_semesters = if_else( # indicate number of semesters spent in undeclared
      !str_detect(step2,"S2U"),1, #true
      (if_else( # false
          !str_detect(step3,"S3U"),2, #true
          3 #false
    )))) %>% 
    relocate(undeclared_semesters, .after = study_id) %>% # put in convenient location for viewing
    group_by(undeclared_semesters, grad_status_dataset) %>% 
    count() %>%
    ungroup() %>% 
    group_by(undeclared_semesters) %>% 
    mutate(proportion = n/sum(n)) %>% 
    ungroup() %>% 
    mutate(grad_status_dataset = fct_relevel(grad_status_dataset,
                                             c("Engineering Degree","Non-Engineering Degree","No Degree"))) 
  
  plot_declaration_time_outcome <- 
    declaration_time_outcome %>%
    ggplot(aes(x = undeclared_semesters, y = n, fill = factor(grad_status_dataset))) +
    geom_col(position = "dodge") +
    labs(y = "Number of students in cohort") +
    labs(x = "Number of semesters in Undeclared Engineering") +
    labs(fill = "Degree Outcome") +
    theme_minimal()
  
  # print(plot_declaration_time_outcome)
  
  # analysis of CoE majors and degree outcomes plotted by duration in undeclared
  declaration_time_pathway <- # identify pathways steps for undeclared starts
    pathways_undeclared %>% 
    separate(col = pathway_history, into = c("step1", "step2", "step3", "step4"), sep = ":") %>%   # prep for network visualization
    mutate(step2 = str_c(step1, step2, sep = ":")) %>% 
    mutate(step3 = str_c(step2, step3, sep = ":")) %>% 
    mutate(step4 = str_c(step3, step4, sep = ":")) %>%
    mutate(step3 = if_else(
      (str_detect(step2,'D')),NA,step3)
    ) %>% 
    mutate(step4 = if_else(
      (str_detect(step3,'D') | is.na(step3)),NA,step4)
    ) %>% 
    mutate(undeclared_semesters = if_else( # indicate number of semesters spent in undeclared
      !str_detect(step2,"S2U"),1, #true
      (if_else( # false
        !str_detect(step3,"S3U"),2, #true
        3 #false
      )))) %>% 
    relocate(undeclared_semesters, .after = study_id)
    
  undeclareds_one_semester <- # select students who spent one semester as undeclared
    declaration_time_pathway %>% 
    filter(undeclared_semesters == 1)
  
  sankey_undeclareds_one <- # structure data for sankey
    undeclareds_one_semester %>%
    make_long(major_first, sem_2_major, grad_status_dataset) %>% # reformat and keep the desired mapping fill paramter
    mutate(node = factor(node, level = rev(levels(factor(node)))),
           next_node = factor(next_node, level = rev(levels(factor(next_node)))))  # reverse factor order to get sankey in alphabetical order top to bottom
  
  # randomize the colors to make sankey more legible
  n_nodes_one <- nlevels(factor(sankey_undeclareds_one$node))
  set.seed(123)
  palette_random_one <-  sample(scales::hue_pal()(n_nodes_one))
    
  plot_sankey_undeclareds_one <-
    sankey_undeclareds_one %>% 
    ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node))) +
    geom_sankey(flow.alpha = .8, node.color = 'gray90', show.legend = FALSE) +
    geom_sankey_label(aes(label = node), size = 3, color = 'black', fill = 'gray90') +
    theme_minimal() +
    theme(legend.position = 'none') +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),  
          panel.grid = element_blank()) +
    scale_fill_manual(values = palette_random_one) + # this uses randomized color order
    scale_x_discrete(labels = c("","major declared","final outcome")) +
    labs(x = "Pathways of Students who spent ONE semester in Undeclared Engineering")
  
  print(plot_sankey_undeclareds_one)
  
  undeclareds_two_semester <- 
    declaration_time_pathway %>% 
    filter(undeclared_semesters == 2)
  
  sankey_undeclareds_two <- # structure data for sankey
    undeclareds_two_semester %>%
    make_long(major_first, sem_3_major, grad_status_dataset) %>% # reformat and keep the desired mapping fill paramter
    mutate(node = factor(node, level = rev(levels(factor(node)))),
           next_node = factor(next_node, level = rev(levels(factor(next_node)))))  # reverse factor order to get sankey in alphabetical order top to bottom
  
  # randomize the colors to make sankey more legible
  n_nodes_two <- nlevels(factor(sankey_undeclareds_two$node))
  set.seed(123)
  palette_random_two <-  sample(scales::hue_pal()(n_nodes_two))
  
  plot_sankey_undeclareds_two <-
    sankey_undeclareds_two %>% 
    ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node))) +
    geom_sankey(flow.alpha = .8, node.color = 'gray90', show.legend = FALSE) +
    geom_sankey_label(aes(label = node), size = 3, color = 'black', fill = 'gray90') +
    theme_minimal() +
    theme(legend.position = 'none') +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),  
          panel.grid = element_blank()) +
    scale_fill_manual(values = palette_random_two) + # this uses randomized color order
    scale_x_discrete(labels = c("","major declared","final outcome")) +
    labs(x = "Pathways of Students who spent TWO semesters in Undeclared Engineering")
  
  print(plot_sankey_undeclareds_two)
  
  # print(plot_sankey_undeclareds_one + plot_sankey_undeclareds_two + # combine into single figure
  #         plot_layout(axes = "collect", guides = "collect") +
  #         plot_annotation(title = "Pathways of Undeclared students based on number of undeclared semesters"))
  
  }

# STUDY OF THE GENERAL PATHWAYS SUMMARIES BY MAJOR ####
general_pathway_summary_analysis = TRUE # set switch to run analysis
if(general_pathway_summary_analysis == TRUE){
  # set up a loop to create summary table major-by-major
  outcome_resolved_first_major <- # process the data to get ready for summaries by major
    outcome_resolved %>% 
    mutate(coe_program = if_else( # get assigned to program if started undeclared
      (undeclared_start == 0),major_first,major_second
    )) %>% 
    relocate(coe_program, .before = major_first) %>% 
    mutate(major_first = if_else(
      (undeclared_start == 0),major_first, major_second
    )) %>% 
    mutate(major_second = if_else(
      (undeclared_start == 0), major_second, major_third
    )) %>% 
    mutate(major_third = if_else(
      (major_second == major_third),NA, major_third
    ))

  program_pathway_summary <- # create a tibble template for summaries
    tibble(program_name = character(),
           program_size = integer(),
           undeclared_start_prop = numeric(),
           first_maj_completion_prop = numeric(),
           other_coe_completion_prop = numeric(),
           non_coe_completion_prop = numeric(),
           no_degree_prop = numeric(),
           mean_semesters = numeric(),
           mean_first_gpa = numeric(),
           early_departure_prop = numeric(),
           departure_gpa = numeric(),
           other_origin_grad_prop = numeric()
           )
  
  program_list <-  c("Aerospace Engineering", "Agricultural Engineering", "Biological Systems Engineering",
                     "Chemical Engineering", "Civil Engineering", "Computer Engineering",
                     "Construction Engineering", "Cyber Security Engineering", "Electrical Engineering",
                     "Environmental Engineering", "Industrial Engineering", "Materials Engineering",
                     "Mechanical Engineering", "Software Engineering")
  for (program_name in program_list){ # build a row of data for each program
    program_dataset <- # select only students who chose program as first major
      outcome_resolved_first_major %>% 
      filter(major_first == program_name)
    
    program_size <- # how many declared as first CoE major
      program_dataset %>% 
      count() %>% 
      pull(n)
    
    program_undeclared_start <- # how many started in undeclared
      program_dataset %>% 
      filter(undeclared_start == 1) %>% 
      count() %>% 
      mutate(undeclared_start_prop = n/program_size) %>% 
      select(undeclared_start_prop)
      
    
    program_completion_in_first_major <- # how many completed the degree in their first major
      program_dataset %>% 
      filter(graduated_program == program_name) %>% 
      count() %>% 
      mutate(first_maj_completion_prop = n/program_size) %>%
      select(first_maj_completion_prop)
    
    program_completion_in_other_coe_major <- # how many transferred to another CoE major and graduated
      program_dataset %>% 
      filter(graduated_college == 'Engineering' & graduated_program != program_name) %>% 
      count() %>% 
      mutate(other_coe_completion_prop = n/program_size) %>%
      select(other_coe_completion_prop)
    
    program_completion_non_coe <- # how many left CoE but still got ISU degree
      program_dataset %>% 
      filter(grad_status_dataset == 'Non-Engineering Degree') %>% 
      count() %>% 
      mutate(non_coe_completion_prop = n/program_size) %>%
      select(non_coe_completion_prop)
    
    program_no_degree <- # how many left ISU with no degree
      program_dataset %>% 
      filter(grad_status_dataset == 'No Degree') %>% 
      count() %>% 
      mutate(no_degree_prop = n/program_size) %>%
      select(no_degree_prop)

    program_coe_duration <- # what is the mean number of semesters in CoE of all who declared this as first major
      program_dataset %>% 
      summarize(mean_semesters = mean(coe_duration))
  
    program_first_gpa <- # what is the mean first semester GPA
      program_dataset %>% 
      summarize(mean_first_gpa = mean(first_sem_gpa, na.rm = TRUE))
    
    program_isu_departure_early <- # count students who departed ISU after staying in CoE 2 semesters or less
      program_dataset %>% 
      filter(grad_status_dataset == 'No Degree') %>% 
      filter(coe_duration <= 2) %>% 
      summarize(count = n(), mean_gpa = mean(first_sem_gpa, na.rm = TRUE)) %>% 
      mutate(early_departure_prop = count/program_size, departure_gpa = mean_gpa) %>% 
      select(early_departure_prop, departure_gpa)
    
    program_all_grads <- # count total number of program graduates regardless of their first CoE major
      outcome_resolved_first_major %>% 
      filter(graduated_program == program_name) %>%
      count() %>% 
      pull(n)
      
    program_entry <- # count number of grads who transferred from other CoE majors
      outcome_resolved_first_major %>% 
      filter(graduated_program == program_name) %>% 
      filter(major_first != program_name & (major_second == program_name | major_third == program_name)) %>% 
      count() %>% 
      mutate(other_origin_grad_prop = n/program_all_grads) %>% 
      select(other_origin_grad_prop)
    
    program_row = tibble(program_name,program_size,program_undeclared_start, program_completion_in_first_major, # assemble the program's row
                         program_completion_in_other_coe_major, program_completion_non_coe, program_no_degree,
                         program_coe_duration, program_first_gpa, program_isu_departure_early,
                         program_entry)
    
    program_pathway_summary <- # append to the summary tibble
      program_pathway_summary %>% 
      add_row(program_row)
  } 
  
}
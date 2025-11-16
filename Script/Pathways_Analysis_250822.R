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
# if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey") # install sankey package

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

# STUDY OF COE DURATION AND DEGREE OUTCOME FOR UNDECLARED STARTS ####

outcomes_duration_normalized <- # structure data to plot results vs. number of semesters in CoE
  outcome_resolved %>% 
  mutate(coe_duration = factor(coe_duration), undeclared_start = factor(undeclared_start),
         major_first = factor(major_first), start_status_isu = factor(start_status_isu), admission_type = factor(admission_type),
         grad_status_dataset = factor(grad_status_dataset), degree_outcome = factor(degree_outcome))

# Reset default color options
options(ggplot2.discrete.fill = c("#1f78ff", "#E69F00", "#33A02C"))
options(ggplot2.discrete.color = c("#1f78ff", "#E69F00", "#33A02C"))

# Added functionality to plot outcomes by major or other defined group

cohort_outcomes_study = 1 # set the trigger to analyze by specified cohort
if (cohort_outcomes_study == 1) { # modify the tibble by filtering if necessary
  outcomes_duration_normalized <- 
  outcomes_duration_normalized %>%
  mutate(coe_program = if_else( # get assigned to program if started undeclared
    (undeclared_start == 0),major_first,major_second
  )) %>% 
  filter(coe_program == 'Aerospace Engineering') # define the cohort
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


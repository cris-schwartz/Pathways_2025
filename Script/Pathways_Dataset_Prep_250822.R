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
  read_excel("./Data/CoE_curriculum_request_072825_edited.xlsx", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
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
    (year_current == '2024' & semester_current == 'Spring') ~ 27,
    (year_current == '2024' & semester_current == 'Fall') ~ 29,
    (year_current == '2025' & semester_current == 'Spring') ~ 30,
    (year_current == '2025' & semester_current == 'Summer') ~ 31
    
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
    (admsn_term == 'S24') ~ 27,
    (admsn_term == '124') ~ 28,
    (admsn_term == 'F24') ~ 29,
    (admsn_term == 'S25') ~ 30,
    (admsn_term == '125') ~ 31
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
    (degree_year == '2022' & degree_term == 'Summer') ~ 22,
    (degree_year == '2022' & degree_term == 'Fall') ~ 23,
    (degree_year == '2023' & degree_term == 'Spring') ~ 24,
    (degree_year == '2023' & degree_term == 'Summer') ~ 25,
    (degree_year == '2023' & degree_term == 'Fall') ~ 26,
    (degree_year == '2024' & degree_term == 'Winter') ~26, # group winter with previous fall
    (degree_year == '2024' & degree_term == 'Spring') ~ 27,
    (degree_year == '2024' & degree_term == 'Summer') ~ 28,
    (degree_year == '2024' & degree_term == 'Fall') ~ 29,
    (degree_year == '2025' & degree_term == 'Spring') ~ 30,
    (degree_year == '2025' & degree_term == 'Summer') ~ 31
  )) %>% 
  group_by(study_id) %>% # group by student to continue coding
  arrange(study_id, sem_sequence_id) %>%  # arrange in chronological order
  mutate(coe_sem_start = first(sem_sequence_id)) %>% # get first semester in CoE program
  mutate(first_college = first(college_prevsem)) %>%  # assign first college at ISU start
  mutate(start_status_isu = if_else( # determine whether first ISU semester was in CoE
   (first_college == 'New' | first_college == 'Not Enrolled'),'CoE', 'non-CoE')
  ) %>% 
  mutate(major_current = if_else( # relabel undeclared
    major_current == 'College of Engineering Undergraduate Undeclared Major','Undeclared Engineering',major_current
  )) %>% 
  mutate(major_first = first(major_current)) %>%
  mutate(undeclared_start = ifelse(major_first == 'Undeclared Engineering',1,0)) %>%  # determine undeclared status
  mutate(grad_status_dataset = case_when(
    (grad_sem_id < coe_sem_start) ~ 'No Degree', # this eliminates students who completed non-CoE degrees before starting in CoE
    (graduated_college == 'Engineering') ~ 'Engineering Degree',
    (graduated_college != 'Engineering' & !is.na(graduated_college)) ~ 'Non-Engineering Degree',
    (is.na(graduated_college)) ~ 'No Degree'
  )) %>% 
  ungroup
  

transfer_details <- # pull out transfer history data
  processed_data %>% 
  group_by(study_id, major_current) %>%
  distinct(study_id, major_current) %>% # delete rows where these two fields are the same
  ungroup %>% 
  group_by(study_id) %>% # regroup to build vectors of major_current by student
  mutate(major_second = nth(major_current,2), major_third = nth(major_current,3)) %>% # select appropriate vector location
  ungroup %>% 
  select(study_id, major_second, major_third) %>% # remove unwanted columns
  group_by(study_id) %>% 
  distinct(study_id, .keep_all = TRUE) %>% # remove all but first row for each study_id
  ungroup

# SUMMARY OF DATA BY STUDENT -----------------------
pathway_summary <- # summarize records to single row per student
  processed_data %>% 
  group_by(study_id) %>% 
  arrange(study_id, sem_sequence_id) %>% # arrange the data chronologically
  summarize(admsn_sem_id = first(admsn_sem_id), coe_sem_start = first(coe_sem_start),
            coe_sem_final = last(sem_sequence_id), coe_duration = n(),
            # major_first = first(major_first), major_second = first(major_second),
            major_first = first(major_first),
            major_changes = n_distinct(major_current) - 1,
            graduated_program = first(graduated_program), graduated_college = first(graduated_college),
            grad_sem_id = first(grad_sem_id), first_college = first(first_college),
            start_status_isu = first(start_status_isu), undeclared_start = first(undeclared_start),
            grad_status_dataset = first(grad_status_dataset), warning_instances = sum(academic_standing == 'Warning'), probation_instances = sum(academic_standing == 'Probation'),
            dismissal_instances = sum(academic_standing == 'Dismissal'),
            sex = first(sex), ethnicity = first(ethnicity), first_generation = first(first_generation),
            veteran = first(veteran), residency = first(residency), admission_type = first(admission_type),
            hs_code = first(hs_code),
            first_sem_gpa = nth(sem_gpa_current,2),final_coe_gpa = last(cmltv_gpa_current)
            ) %>%
            mutate(degree_duration = case_when( # determine the number of semesters either to degree or since ISU start
              (grad_status_dataset != 'No Degree') ~ grad_sem_id - admsn_sem_id + 1,
              (grad_status_dataset == 'No Degree') ~ 31 - admsn_sem_id + 1 # 31 is the semester ID of Summer 2025
            )) %>% 
            mutate(degree_outcome = case_when( # determine the ultimate outcome for students with no degree listed
              (!is.na(graduated_program)) ~ 'Degree',
              (is.na(graduated_program) ~ (if_else(degree_duration <= 14, 'Undetermined','No Degree')))
            )) %>% 
  left_join(.,transfer_details) %>% # insert the transfer history info
  relocate(c(major_second, major_third), .after = major_first) %>% # move columns to better location
  mutate(major_changes = if_else( # do not count declaring major as a major change
    (undeclared_start == 1 & !is.na(major_second)), major_changes - 1, major_changes
  ))
  

# OUTPUT OF DATASET WITH ONLY DEGREE OUTCOME DETERMINED STUDENTS -------
resolved_students <- 
  pathway_summary %>% 
  filter(degree_outcome != 'Undetermined') # filter out undetermined degree status students

# GENERATE CSV OUTPUT TO EXPLORE DATASET
# write_csv(resolved_students,'./Data/Degree_Outcome_Resolved_Students_250901.csv')
  
# CALCULATE DISTRIBUTION OF SEMESTERS TO COMPLETE DEGREE ------
# variables below have already been calculated and results used to determine degree_outcome status of students/
# in the dataset with no degree listed

# graduates_HSdirect_coe_degree <- 
#   pathway_summary %>%
#   filter(grad_status_dataset != 'No Degree') %>% 
#   filter(admission_type == 'Direct from HS' & graduated_college == 'Engineering') %>% 
#   mutate(degree_duration = grad_sem_id - admsn_sem_id + 1)
# 
# graduates_HSdirect_noncoe_degree <- 
#   pathway_summary %>% 
#   filter(grad_status_dataset != 'No Degree') %>% 
#   filter(admission_type == 'Direct from HS' & graduated_college != 'Engineering') %>% 
#   mutate(degree_duration = grad_sem_id - admsn_sem_id + 1)
# 
# graduates_transfer_coe_degree <- 
#   pathway_summary %>% 
#   filter(grad_status_dataset != 'No Degree') %>% 
#   filter(admission_type == 'Transfer' & graduated_college == 'Engineering') %>% 
#   mutate(degree_duration = grad_sem_id - admsn_sem_id + 1)
# 
# graduates_transfer_noncoe_degree <- 
#   pathway_summary %>% 
#   filter(grad_status_dataset != 'No Degree') %>% 
#   filter(admission_type == 'Transfer' & graduated_college != 'Engineering') %>% 
#   mutate(degree_duration = grad_sem_id - admsn_sem_id + 1)

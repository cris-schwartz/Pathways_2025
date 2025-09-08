# Title:    Analysis of CoE Students who transferred to Computer Science
# File:     Pathways_Analysis_COMS_250902.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(readxl)
library(ggsankey)
if (!require("ggsankey")) devtools::install_github("davidsjoberg/ggsankey") # install sankey package

# LOAD AND PREPARE DATA ------------------------------------
pathway_summary <- # import the raw csv file of the student summary data
  read_csv("./Data/Student_Pathway_Summary_250902.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
  as_tibble()

semester_based_data <- # load the semester-by-semester data
  read_csv("./Data/Processed_Student_Dataset_250902.csv", guess_max = 1000) %>% 
  as_tibble()

coms_transfer_students <- 
  semester_based_data %>% 
  filter(major_nextsem == 'Computer Science') %>% 
  select(study_id,sem_sequence_id, academic_standing) %>%  # select fields 
  rename(transfer_sem_id = sem_sequence_id, academic_standing_at_transfer = academic_standing) %>% 
  left_join(.,pathway_summary,by = "study_id") # match up summaries for each student

node_fields <- # structure the data for a Sankey plot
  coms_transfer_students %>% 
  select(study_id, major_first, graduated_program, graduated_college) %>% 
  mutate(degree_field = case_when(
    (graduated_program == 'Computer Science') ~ "2. COMS Degree",
    (graduated_program != 'Computer Science' & !is.na(graduated_program)) ~ "other",
    (is.na(graduated_program)) ~ "1. No Degree"
  )) %>% 
  mutate(degree_field = if_else(
    degree_field != "other",degree_field, case_when(
      (graduated_college == "LAS") ~ "4. LAS Degree (other than COMS)",
      (graduated_college == "Business") ~ "3. BUS Degree",
      (graduated_college == "HHS") ~ "6. HHS Degree",
      (graduated_college == "Engineering") ~ "5. Engineering Degree"
    )
  ))

# VISUALIZE THE DATA ------------------------------------

coms_pathways_sankey_format <- 
  node_fields %>% 
  make_long(major_first, degree_field)

print(
  ggplot(coms_pathways_sankey_format %>% 
           mutate(node = fct_rev(as.factor(node)),
                  next_node = fct_rev(as.factor(next_node))
                  ),
         aes(x = x,
                                          next_x = next_x,
                                          node = node,
                                          next_node = next_node,
                                          fill = factor(node))) +
    geom_sankey(flow.alpha = 0.8, node.color = 'gray90', show.legend = FALSE) +
    geom_sankey_label(aes(label = node), size = 3, color = 'black', fill = 'gray90') +
    theme_bw() +
    theme(legend.position = 'none') +
    theme(axis.title = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks = element_blank()  
          , panel.grid = element_blank()) 
  
)

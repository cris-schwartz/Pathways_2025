# Title:    Analysis of Intra-CoE Major Transfers
# File:     Pathways_Transfers_260422.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse,ggforce)
library(readxl)
library(cobalt)
library(broom)
library(patchwork)
library(glue)
library(tidygraph)
library(ggraph)

# LOAD AND PREPARE DATA ------------------------------------
pathway_summary <- # import the previously prepared pathway_summary csv file
  read_csv("./Data/Resolved_Students_Major_Pathways_260422.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
  as_tibble()


# CALCULATE TRANSFERS IN AND OUT BY EACH MAJOR --------------
graduates_with_transfers <- # select only students who earned CoE degree and changed CoE major
  pathway_summary %>% 
  filter(major_changes != 0, grad_status_dataset == 'Engineering Degree')

transfer_flows <- # determine the numbers of transfers along each major combination
  graduates_with_transfers %>% 
  group_by(major_first, major_second) %>% 
  summarize(totals = n())

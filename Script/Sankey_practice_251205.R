# Title:    Practice with ggsankifier
# File:     Sankey_practice_251205.R
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
library(ggsankeyfier)

# LOAD AND PREPARE DATA ------------------------------------


## INITIAL GGSANKEYFIER WORK ####
# ## first get a data set with a wide format:
# data(ecosystem_services)
# 
# ## pivot to long format for plotting:
# es_long <-
#   pivot_stages_longer(
#     ## the data.frame we wish to pivot:
#     data        = ecosystem_services,
#     ## the columns that represent the stages:
#     stages_from = c("activity_type", "pressure_cat",
#                     "biotic_realm", "service_division"),
#     ## the column that represents the size of the flows:
#     values_from = "RCSES"
#   )
# 
# 
# library(ggplot2)
# theme_set(theme_light())
# 
# 
# ## Let's subset the example data to create a less cluttered
# ## Sankey diagram
# es_sub <-
#   ecosystem_services |>
#   subset(RCSES > 0.005) |>
#   pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"),
#                       "RCSES", "service_section")
# print(
# ggplot(
#   data    = es_sub,
#   mapping = aes(x = stage, y = RCSES, group = node,
#                 edge_id = edge_id, connector = connector, colour = stage)) +
#   ## apply fill and alpha aesthetic only to edges (not the nodes)
#   geom_sankeyedge(aes(alpha = RCSES, fill = service_section)) +
#   geom_sankeynode() +
#   guides(fill   = guide_legend(ncol = 1),
#          alpha  = guide_legend(ncol = 1),
#          colour = guide_legend(ncol = 1)) +
#   theme(legend.position = "top")
# )

# UPDATED SANKEY WORK --------------------------------------
pathways_general_wide <- 
  read_csv("./Data/Pathways_wide.csv") %>% 
  as_tibble()

desired_sankey_order <- 
  c("Aerospace Engineering", "Agricultural Engineering",
    "Biological Systems Engineering", "Chemical Engineering",
    "Civil Engineering", "Computer Engineering",
    "Construction Engineering", "Cyber Security Engineering",
    "Electrical Engineering", "Environmental Engineering",
    "Industrial Engineering", "Materials Engineering",
    "Mechanical Engineering", "Software Engineering",
    "Non-Engineering Degree", "No Degree",
    "Never Declared", "Started in Major",
    "Started Undeclared", "Other ISU College",
    "Transfer", "Direct from High School")

pathways_long <- 
  pathways_general_wide %>% 
  mutate(student_tally = 1) %>% # need to make value of 1 to handle ggsankeyfier summation
  pivot_stages_longer(
    stages_from = c("student_origin", "undeclared_start", "major_first", "program_outcome"),
    values_from = "student_tally"
    # additional_aes_from = "first_generation"
  )

pos_sankey <- 
  position_sankey(align = "center", v_space = "auto")

plot_sankey <- 
  pathways_long %>% 
  ggplot(
    aes(x = stage, y = student_tally, group = node,
        edge_id = edge_id, connector = connector, fill = node)) +
  geom_sankeyedge(position = pos_sankey, alpha = 0.8) +
  geom_sankeynode(position = pos_sankey) +
  geom_text(aes(label = node), stat = "sankeynode", position = pos_sankey) +
  scale_fill_manual(values = c(
    "Aerospace Engineering" = "#4e79a7",
    "Agricultural Engineering"   = "#A0CBE8",
    "Biological Systems Engineering"  = "#F28E2B",
    "Chemical Engineering"  = "#59A14F",
    "Civil Engineering"   = "#B6992D",
    "Computer Engineering" = "#F1CE63",
    "Construction Engineering" = "#D37295",
    "Cyber Security Engineering" = "#86BCB6",
    "Electrical Engineering"   = "#E15759",
    "Environmental Engineering" = "#FF9D9A",
    "Industrial Engineering"   = "#79706E",
    "Materials Engineering" = "#499894",
    "Mechanical Engineering"   = "#B07AA1",
    "Software Engineering"   = "#9D7660",
    "Started in Major" = "#E69F00",
    "Started Undeclared" = "#1f78ff",
    "Other ISU College" = "#2F4F4F",
    "Transfer" = "#A45EE5" ,
    "Direct from High School" = "#6EDCC4"
  )) +
  theme_minimal() +
  theme(legend.position = 'none')
  
print(plot_sankey)

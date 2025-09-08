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

bar_n  <- 50        # how many cases the ruler should represent
x_pos  <- 0.6       # place near the left of the first stage (tweak as needed)
y0     <- 0
y1     <- bar_n

print(
  ggplot(
    coms_pathways_sankey_format,
    aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, value = 1)
  ) +
    geom_sankey(flow.alpha = 0.8, node.color = "gray90", show.legend = FALSE) +
    geom_sankey_label(aes(label = node), size = 3, color = "black", fill = "gray90") +
    
    # scale bar (doesn't inherit sankey aesthetics)
    geom_segment(aes(x = x_pos, xend = x_pos, y = y0, yend = y1),
                 inherit.aes = FALSE, linewidth = 0.6) +
    geom_text(aes(x = x_pos, y = y1, label = paste0(bar_n, " cases")),
              inherit.aes = FALSE, vjust = -0.4, size = 3) +
    
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      plot.margin = margin(5.5, 22, 5.5, 5.5) # a little room for the label
    )
)

library(scales)
print(
  ggplot(
    coms_pathways_sankey_format,
    aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, value = 1)
  ) +
    geom_sankey(flow.alpha = 0.8, node.color = "gray90", show.legend = FALSE) +
    geom_sankey_label(aes(label = node), size = 3, color = "black", fill = "gray90") +
    scale_y_continuous(name = "Count", breaks = pretty_breaks(n = 5),
                       expand = expansion(mult = c(0.02, 0.02))) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid   = element_blank()
    )
)
# Title:    Analysis of Intra-CoE Major Transfers
# File:     Pathways_Transfers_260422.R
# Project:  Pathways_2025

# CLEAN AND CLEAR THE ENVIRONMENT --------------------------
rm(list = ls())             ## Clear environment
# cat("\014")                 ## Clear console, ctrl+L

# INSTALL AND LOAD PACKAGES --------------------------------
pacman::p_load(magrittr, pacman, tidyverse, ggforce)
library(readxl)
library(cobalt)
library(broom)
library(patchwork)
library(glue)
library(tidygraph)
library(ggraph)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.lpsolve)


# LOAD AND PREPARE DATA ------------------------------------
pathway_summary <- # import the previously prepared pathway_summary csv file
  read_csv("./Data/Resolved_Students_Major_Pathways_260422.csv", guess_max = 1000) %>% # guess_max ensures empty rows not treated as logical values 
  as_tibble()


# CALCULATE TRANSFERS IN AND OUT BY EACH MAJOR --------------
graduates_with_transfers <- # select only students who earned CoE degree and changed CoE major
  pathway_summary %>% 
  filter(major_changes != 0, grad_status_dataset == 'Engineering Degree') %>% 
  mutate(major_second = if_else( # correct any intermediate transfers to undeclared
    (major_second == 'Undeclared Engineering'), major_third, major_second
  ))

transfer_flows <- # determine the numbers of transfers along each major combination
  graduates_with_transfers %>% 
  group_by(major_first, major_second) %>% 
  summarize(totals = n())



program_list <-  tibble(program_name = c("Aerospace Engineering", "Agricultural Engineering", "Biological Systems Engineering",
                                         "Chemical Engineering", "Civil Engineering", "Computer Engineering",
                                         "Construction Engineering", "Cyber Security Engineering", "Electrical Engineering",
                                         "Environmental Engineering", "Industrial Engineering", "Materials Engineering",
                                         "Mechanical Engineering", "Software Engineering"), 
                        program_abbreviation = c("AERE", "AE", "BSE", "CHE", "CE",
                                                 "CPRE", "CONE", "CYBE", "EE", "ENVE",
                                                 "IE", "MATE", "ME", "SE"),
                        program_plot_color = c(
                          "#4E79A7","#A0CBE8","#F28E2B","#59A14F",
                          "#B6992D","#F1CE63","#D37295","#86BCB6",
                          "#E15759","#FF9D9A","#79706E","#499894",
                          "#B07AA1","#9D7660") # based on Tableau 20 color palette
)

program_colors <- setNames(program_list$program_plot_color, program_list$program_abbreviation) # ensure programs
# get same color in each plot

transfer_flows <- 
  transfer_flows %>% 
  left_join(.,program_list, by = c('major_first' = 'program_name'), keep = FALSE) %>% 
  mutate(major_first = program_abbreviation) %>% 
  select(!(program_abbreviation)) %>% 
  left_join(.,program_list, by = c('major_second' = 'program_name'), keep = FALSE) %>% 
  mutate(major_second = program_abbreviation) %>% 
  select(!c(program_abbreviation,program_plot_color.y)) %>% 
  rename(program_plot_color = program_plot_color.x)
  
# K-CUT ANALYSIS --------------
# Most of this section was generated using Claude Sonnet 4.6
# Minimum K-cut optimization using Integer Linear Programming
# Partition N = 14 majors into K groups. Start with K = 4
# to see the best partitioning to minimize transfers in/out of each group
k_cut_analysis = FALSE
if (k_cut_analysis == TRUE) {
majors <- program_list$program_name # extract the vector of program names
N = length(majors) # total number of majors
K <- 6 # desired number of groups

# construct a 2 x 2 matrix of transfers for the ILP solver
transfer_matrix <- 
  transfer_flows %>% 
  pivot_wider(names_from = major_second, values_from = totals) %>% 
  select(order(colnames(.))) %>% 
  relocate(major_first) %>% 
  column_to_rownames("major_first") %>% 
  mutate(across(everything(), as.numeric))

W <- as.matrix(transfer_matrix)

W[is.na(W)] <- 0 # replace NA's with 0 to show no transfers
# W_sym = W + t(W) # will treat as directed transfers so do not use symmetric matrix
W <- W + t(W)

edge_list <- transfer_flows # build the edge list table
colnames(edge_list) <- c("first_major", "second_major", "total_transfers") 
mutate(edge_list, total_transfers = as.integer(total_transfers))

cat("Edge list preview (first 6 rows):\n")
print(head(edge_list))
cat(sprintf("\nTotal edges: %d | Total students tracked: %d\n\n",
            nrow(edge_list), sum(edge_list$total_transfers)))

# ILP FORMULATION
# Decision variables:
#   x[i, k] in {0,1}  -- node i is assigned to group k
#   z[i, j] in {0,1}  -- edge (i,j) crosses a group boundary (is "cut")
#
# Objective:
#   Minimize  SUM_{i < j} W[i,j] * z[i,j]
#
# Constraints:
#   (C1) Each node belongs to exactly one group:
#          SUM_k x[i,k] = 1   for all i
#   (C2) Cut linearization — z[i,j] must equal 1 if i and j are in different groups:
#          z[i,j] >= x[i,k] - x[j,k]   for all i < j, k
#          z[i,j] >= x[j,k] - x[i,k]   for all i < j, k
#   (C3) Each group must contain at least one node (non-empty groups):
#          SUM_i x[i,k] >= 1   for all k
#   (C4) Symmetry breaking — fix node 1 to group 1 to reduce equivalent solutions:
#          x[1,1] = 1


cat("Building ILP model...\n")

model <- MIPModel() %>% 
  # --- Variables ---
  add_variable(x[i, k], i = 1:N, k = 1:K, type = "binary") %>% 
  add_variable(z[i, j], i = 1:N, j = 1:N, i < j, type = "binary") %>% 
  
  # --- Objective ---
  set_objective(
    sum_over(W[i, j] * z[i, j], i = 1:N, j = 1:N, i < j),
    sense = "min"
  ) %>% 
  
  # --- C1: Each node in exactly one group ---
  add_constraint(sum_over(x[i, k], k = 1:K) == 1, i = 1:N) %>% 
  
  # --- C2: Cut linearization ---
  add_constraint(z[i, j] >= x[i, k] - x[j, k], i = 1:N, j = 1:N, k = 1:K, i < j) %>% 
  add_constraint(z[i, j] >= x[j, k] - x[i, k], i = 1:N, j = 1:N, k = 1:K, i < j) %>% 
  
  # --- C3: Non-empty groups ---
  add_constraint(sum_over(x[i, k], i = 1:N) >= 1, k = 1:K) %>% 
  
  # --- C4: Symmetry breaking ---
  add_constraint(x[1, 1] == 1)

cat(sprintf("Model built: %d variables, %d constraints\n\n",
            nrow(model$variables), nrow(model$constraints)))

#4. SOLVE
cat("Solving with lpsolve...\n\n")

result <- solve_model(
  model,
  with_ROI(solver = "lpsolve", verbose = TRUE)
)

cat(sprintf("\nSolver status: %s\n", solver_status(result)))

# 5. EXTRACT & DISPLAY RESULTS

# --- Node assignments ---
assignments <- get_solution(result, x[i, k]) |>
  filter(value > 0.5) |>
  transmute(
    major = majors[i],
    node_index = i,
    group = k
  ) |>
  arrange(group, major)

cat("\n=== MAJOR GROUP ASSIGNMENTS ===\n")
for (g in 1:K) {
  members <- assignments |> filter(group == g) |> pull(major)
  cat(sprintf("  Group %d: %s\n", g, paste(members, collapse = ", ")))
}

# --- Objective value ---
total_cut_flow <- objective_value(result)
total_flow     <- sum(W[upper.tri(W)])
pct_cut        <- round(100 * total_cut_flow / total_flow, 1)

cat(sprintf(
  "\n=== CUT STATISTICS ===\n  Inter-group student flow : %s\n  Total student flow       : %s\n  Percent of flow cut      : %s%%\n",
  format(total_cut_flow, big.mark = ","),
  format(total_flow,     big.mark = ","),
  pct_cut
))

# --- Per-group summary ---
cat("\n=== GROUP SUMMARY ===\n")
group_summary <- assignments |>
  group_by(group) |>
  summarise(
    n_majors = n(),
    majors   = paste(major, collapse = ", "),
    .groups  = "drop"
  )
print(group_summary)

# --- Inter-group flow matrix (between the 4 groups) ---
node_group <- assignments |>
  arrange(node_index) |>
  pull(group)

flow_between_groups <- matrix(0, K, K)
for (i in 1:N) {
  for (j in 1:N) {
    if (i != j) {
      gi <- node_group[i]
      gj <- node_group[j]
      flow_between_groups[gi, gj] <- flow_between_groups[gi, gj] + W[i, j]
    }
  }
}
rownames(flow_between_groups) <- paste0("Group ", 1:K)
colnames(flow_between_groups) <- paste0("Group ", 1:K)

cat("\n=== INTER-GROUP FLOW MATRIX (directed) ===\n")
print(flow_between_groups)
cat("(Diagonal = within-group flow; off-diagonal = cross-group flow)\n")
}


# VISUALIZE TRANSFER FLOWS -----------------------------------
tree_plot_visualization = TRUE
if (tree_plot_visualization == TRUE){
  tree_edges_graph <- 
    transfer_flows %>% 
    rename(from = major_first, to = major_second)
  
  tree_graph_transfers <- 
    tbl_graph(
      edges = tree_edges_graph,
      directed = TRUE)

  plot_tree_graph_transfers <- 
    tree_graph_transfers %>% 
    ggraph(layout = "linear", circular = TRUE) +
    geom_edge_diagonal(aes(edge_width = totals, color = program_plot_color),
                       arrow = arrow(length = unit(4, 'mm')),
                       end_cap = circle (2, 'mm'),
                       alpha = 0.8)+
    scale_edge_color_identity() +
    geom_node_label(aes(label = name)) +
    # theme_minimal() +
    # theme_no_axes() +
    theme_graph(base_family = "") +
    theme(plot.title = element_text(size = 16)) +
    scale_edge_width(guide = "none") +
    labs(title = "CoE Transfers Between Programs 2015 - 2024")
 
  
  print(plot_tree_graph_transfers)

  transfers_out <- 
    transfer_flows %>% 
    group_by(major_first) %>% 
    summarize(net_out = sum(totals))
  
  transfers_in <- 
    transfer_flows %>% 
    group_by(major_second) %>% 
    summarize(net_in = sum(totals))
  
  net_transfers <- 
    left_join(transfers_out, transfers_in, by = c('major_first' = 'major_second')) %>% 
    mutate(net_transfers_in = net_in - net_out)
  
}

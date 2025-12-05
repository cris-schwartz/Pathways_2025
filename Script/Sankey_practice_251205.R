# Title:    Practice with ggsankifier
# File:     Sankey_practice_251205.R
# Project:  Pathways_2025

library(ggsankeyfier)

## first get a data set with a wide format:
data(ecosystem_services)

## pivot to long format for plotting:
es_long <-
  pivot_stages_longer(
    ## the data.frame we wish to pivot:
    data        = ecosystem_services,
    ## the columns that represent the stages:
    stages_from = c("activity_type", "pressure_cat",
                    "biotic_realm", "service_division"),
    ## the column that represents the size of the flows:
    values_from = "RCSES"
  )


library(ggplot2)
theme_set(theme_light())


## Let's subset the example data to create a less cluttered
## Sankey diagram
es_sub <-
  ecosystem_services |>
  subset(RCSES > 0.005) |>
  pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"),
                      "RCSES", "service_section")
print(
ggplot(
  data    = es_sub,
  mapping = aes(x = stage, y = RCSES, group = node,
                edge_id = edge_id, connector = connector, colour = stage)) +
  ## apply fill and alpha aesthetic only to edges (not the nodes)
  geom_sankeyedge(aes(alpha = RCSES, fill = service_section)) +
  geom_sankeynode() +
  guides(fill   = guide_legend(ncol = 1),
         alpha  = guide_legend(ncol = 1),
         colour = guide_legend(ncol = 1)) +
  theme(legend.position = "top")
)
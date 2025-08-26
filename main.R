# Robustness Analysis ----------

#Takes a few hours to generate the replicate robustness data

library(tidyverse)
library(patchwork)
library(igraph)

source("lib/robustness.R")

# set seed
set.seed(66)

# Data sources ----

edgelist <- read.csv("data/edgelist.csv")
extinction_order = read.csv("data/extinction_order.csv")

## SECONDARY EXTINCTION ----
graph <- graph_from_edgelist(as.matrix(edgelist))

# define spread of losses
spread <- seq(from=1,to=99,by=5)

# empty df
results <- data.frame(robustness = numeric(),
                      threshold = numeric(),
                      rep = numeric())

for (i in 1:ncol(extinction_order)) {
  
  rob <- robustness_gradient(graph,
                             spread = spread,
                             ext_order = extinction_order[,i])
  
  results <-
    rbind(results,
          data.frame(robustness = rob[,1],
                     threshold = rob[,2],
                     rep = rep(i, length(rob[,1]))))
  
}

write_csv(results,
          "data/robustness_r.csv")

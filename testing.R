library(tidyverse)

# set seed
set.seed(66)

results_r <- read.csv("data/robustness_r.csv") %>%
  mutate(robustness = signif(robustness, 2)) %>%
  group_by(rep) %>%
  arrange(threshold)
results_julia <- read.csv("data/robustness_julia.csv") %>%
  mutate(robustness = signif(robustness, 2)) %>%
  group_by(rep) %>%
  arrange(threshold)

results_r$robustness - results_julia$robustness

anti_join(results_r, results_julia) %>% view()

anti_join(results_r, results_julia) %>%
  group_by(threshold) %>%
  tally()

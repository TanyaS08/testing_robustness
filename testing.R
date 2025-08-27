library(tidyverse)

# set seed
set.seed(66)

results_r <- read.csv("data/robustness_r.csv") %>%
  mutate(robustness = signif(robustness, 2)) %>%
  group_by(rep) %>%
  arrange(threshold) %>%
  ungroup()
results_julia <- read.csv("data/robustness_julia.csv") %>%
  mutate(robustness = signif(robustness, 2)) %>%
  group_by(rep) %>%
  arrange(threshold) %>%
  ungroup()

results_r$robustness - results_julia$robustness

anti_join(results_r, results_julia)

anti_join(results_r, results_julia) %>%
  group_by(threshold) %>%
  tally()

df_combined <- 
  rbind(results_r %>%
          mutate(source = "R"),
        results_julia %>%
          mutate(source = "julia"))


ggplot(df_combined %>%
         group_by(source, threshold, rep) %>%
         summarise(robustness = mean(robustness))) +
  geom_abline(intercept = 100,
              slope = -1,
              colour = "grey85") +
  #geom_line(aes(x = threshold,
  #              y = robustness,
  #              colour = source)) +
  geom_smooth(data = df_combined,
              aes(x = threshold,
                  y = robustness*100,
                  colour = source),
              se = FALSE) +
  ylim(0,100)



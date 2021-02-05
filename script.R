# Fetch data from Github
load(url("https://github.com/dgromer/labmeeting_20210205/blob/main/data.RData?raw=true"))

# install.packages("tidyverse")
library(tidyverse)

l <-
  logfiles %>%
  map(filter, !is.na(exp_slider.response)) %>%
  map(select, exp_slider.response, cs_type, group) %>%
  map(split, c(rep("cond", 36), rep("ext", 36), rep("reinst", 3))) %>%
  modify_depth(2, ~ split(., .$cs_type)) %>%
  modify_depth(3, mutate, trial = 1:n()) %>%
  modify_depth(2, bind_rows) %>%
  map(bind_rows, .id = "phase") %>%
  bind_rows(.id = "id") %>%
  rename(exp = exp_slider.response) %>%
  select(id, group, phase, cs_type, trial, exp)

# Plot
l %>%
  group_by(group, phase, cs_type, trial) %>%
  summarize(mean_exp = mean(exp), se = sd(exp) / sqrt(n())) %>%
  ggplot(aes(x = trial, y = mean_exp, color = cs_type)) +
  geom_point() +
  geom_path() +
  geom_errorbar(aes(ymin = mean_exp - se, ymax = mean_exp + se), width = .4) +
  facet_grid(group ~ phase)

# Same as above, but with grouped operations instead of map/modify_depth
logfiles %>%
  bind_rows(.id = "id") %>%
  filter(!is.na(exp_slider.response)) %>%
  select(id, group, cs_type, exp_slider.response) %>%
  group_by(id) %>%
  mutate(phase = c(rep("cond", 36), rep("ext", 36), rep("reinst", 3))) %>%
  group_by(id, phase, cs_type) %>%
  mutate(trial = 1:n()) %>%
  rename(exp = exp_slider.response) %>%
  select(id, group, phase, cs_type, trial, exp) %>%
  arrange(id, phase, cs_type, trial)

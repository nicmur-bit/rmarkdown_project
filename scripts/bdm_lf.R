library(tidyverse)

bdm_lf <- read_csv("data/bdm_length_frequency.csv") %>% 
filter(!is.na(length), !is.na(weight))

bdm_lf %>% 
  group_by(species)

bdm_echinites <- filter(bdm_lf, species == "Actinopyga echinites") %>% 
  group_by(year)

bdm_plot1 <- bdm_echinites %>% 
  ggplot(aes(x = length)) +
  geom_bar(group = year)



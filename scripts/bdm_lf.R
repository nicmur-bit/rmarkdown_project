library(tidyverse)

bdm_lf <- read_csv("data/bdm_length_frequency.csv") 

bdm_echinites <- bdm_lf %>% 
  filter(str_detect(species, 'echinites|echin|tes')) +
  
  
bdm_plot1 <- bdm_echinites %>% 
  ggplot(aes(x = length)) +
  geom_bar()+
  facet_wrap(~year)
 

#filter(!is.na(length), !is.na(weight))
#filter(bdm_lf, species == "Actinopyga echinites") 
#group_by(year)

bdm_density <- read_csv("data/bdm_density_average_sea cucumber_only.csv")

bdm_whitmaei <- bdm_density %>% 
  select('holothuria_whitmaei', 'zone', 'samocc', 'strata')  %>% 
  group_by(zone)
  
  
ggplot(bdm_whitmaei, aes(x= samocc)) +
  geom_density() +
  facet_wrap(~ zone)

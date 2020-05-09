library(tidyverse)

bdm_lf <- read_csv("data/bdm_length_frequency.csv") 

bdm_echinites <- bdm_lf %>% 
  filter(str_detect(species, 'echinites|echin|tes')) 

bdm_echinites %>% 
  ggplot(aes(x = length)) +
  geom_bar(binwidth = 5)+
  facet_wrap(~year)

bdm_whit <-  bdm_lf %>% 
  filter(species == "Holothuria whitmaei")
  
#group_by(year)
  
bdm_whit %>% 
  ggplot(aes(x = length)) +
  geom_histogram(binwidth = 10)+
  facet_wrap(~year)

bdm_whit$year <- as.character(bdm_whit$year)

bdm_whit %>% 
  ggplot(aes(x = length, group = year, fill = year)) +
  geom_histogram(aes(y = stat(count) / sum(count) * 100), 
                 bins = 45, position = "dodge") 
  
#guides(fill = guide_legend(byrow = "TRUE"))


#guides(colour = guide_colorbar(order = 1),
#alpha = guide_legend(order = 2))
  
  
#scale_x_discrete(limits = c("1995", "2005", "2009", "2019")) +
#scale_fill_discrete(name = "Year", labels = c("1995", "2005", "2009", "2019"))

#filter(!is.na(length), !is.na(weight))
#filter(bdm_lf, species == "Actinopyga echinites") 
#group_by(year)

bdm_density <- read_csv("data/bdm_density_average_sea cucumber_only.csv")

bdm_whitmaei <- bdm_density %>% 
  select('holothuria_whitmaei', 'zone', 'samocc', 'strata')

whitmaei_reeftop <- filter(bdm_whitmaei, strata == "Reef top") %>% 
  group_by(zone)

whit_fourzone <- subset(whitmaei_reeftop, zone == "Don Cay")

ggplot(whitmaei_reeftop, aes(x= zone, colour = zone)) +
  geom_density() +
  facet_wrap(~ samocc)

whitmaei_reeftop %>% 
  ggplot(aes(zone)) +
  geom_density()
  


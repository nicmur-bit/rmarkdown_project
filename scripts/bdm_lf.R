
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

bdm_density <- read_csv("data/bdm_density_average_sea cucumber_only_edit.csv")

bdm_whitmaei <- bdm_density %>% 
  select('holothuria_whitmaei', 'zone', 'samocc', 'strata')

whitmaei_reeftop <- filter(bdm_whitmaei, strata == "Reef top") 
group_by(zone)

whit_fourzone <- filter(whitmaei_reeftop, zone == "Don Cay"| zone == "Cumberland" | zone == "Barrier" | zone == "Darnley" | zone == "Seven Reefs" | zone == "Great North East Channel") 

arrange(whit_fourzone, desc(samocc))

ggplot(whit_fourzone, aes(x= samocc, colour = samocc)) +
  geom_density() +
  facet_wrap(~ zone)

whit_fourzone %>% 
  ggplot(aes(samocc)) +
  geom_density()
  
#level_order <- c("RR195", "ETS02", "ETS05","ETS09", "ETS19")  
#fct_relevel(samocc,"RR195", "ETS02", "ETS05","ETS09", "ETS19"))

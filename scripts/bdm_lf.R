
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

bdm_density <- read_csv("data/bdm_density_average_sea cucumber_edit.csv")

bdm_whitmaei <- bdm_density %>% 
  select('holothuria_whitmaei', 'zone', 'samocc', 'strata')

whitmaei_reeftop_buff <- filter(bdm_whitmaei, strata == "Reef top buffer (200 m)") 

whit_fourzone <- filter(whitmaei_reeftop_buff, zone == "Barrier" | zone == "Great North East Channel" | zone == "Don Cay"| zone == "Cumberland" | zone == "Darnley" | zone == "Seven Reefs") 

ggplot(whit_fourzone, aes(x= samocc, colour = samocc)) +
  geom_density() +
  facet_wrap(~ zone) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019))

ggplot(whit_fourzone, aes(y= holothuria_whitmaei, x = samocc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ zone) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019))

#include all strata and use colour to separate them

ggplot(whit_fourzone, aes(y= holothuria_whitmaei, x = samocc, colour = zone)) +
  geom_line() +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019))

ggplot(whit_fourzone, aes(y= holothuria_whitmaei, x = zone, colour = samocc)) +
  geom_line() 
  

# i don't need density, can't use for this data

whit_fourzone %>% 
  ggplot(aes(samocc)) +
  geom_density() +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019))

whit_fourzone %>% 
  ggplot() +
  geom_density(aes(x = samocc, group = zone, fill = zone), alpha = 0.2, adjust = 1) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019))

whit_fourzone %>% 
  ggplot(aes(samocc, colour = zone)) +
  geom_density()
  
whit_fourzone %>% 
  ggplot(aes(x = samocc, y = holothuria_whitmaei, colour = zone)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019))

#include an average/summarise of data
  
#group_by(zone)
#arrange(whit_fourzone, desc(samocc))
#level_order <- c("RR195", "ETS02", "ETS05","ETS09", "ETS19")  
#fct_relevel(samocc,"RR195", "ETS02", "ETS05","ETS09", "ETS19"))

#include all strata and use colour to separate them

bdm_whitmaei <- bdm_density %>% 
  select('holothuria_whitmaei', 'zone', 'samocc', 'strata')

whit_all_strata <- filter(bdm_whitmaei, zone == "Barrier" | zone == "Great North East Channel" | zone == "Don Cay"| zone == "Cumberland" | zone == "Darnley" | zone == "Seven Reefs") 

whit_no_deep <- filter(whit_all_strata, strata == "Reef top buffer (200 m)" | strata == "Reef top" | strata == "Reef edge")

ggplot(whit_no_deep, aes(y= holothuria_whitmaei, x = samocc, colour = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ zone) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

ggplot(whit_no_deep, aes(y= holothuria_whitmaei, x = samocc, colour = zone)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ strata) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 





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

#this is just for reef top buff, produce one for other two strata and put on average/mean trend line

#include an average/summarise of data
  
#group_by(zone)
#arrange(whit_fourzone, desc(samocc))
#level_order <- c("RR195", "ETS02", "ETS05","ETS09", "ETS19")  
#fct_relevel(samocc,"RR195", "ETS02", "ETS05","ETS09", "ETS19"))
#ggplot(whit_no_deep, aes(y= holothuria_whitmaei, x = samocc, colour = zone)) +
#geom_point() +
#geom_line() +
#facet_wrap(~ strata) +
#scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
#theme(axis.text.x=element_text(angle=45,hjust=1)) 

#include all strata and use colour to separate them

#holothuria_whitmaei
bdm_whitmaei <- bdm_density %>% 
  select('holothuria_whitmaei', 'zone', 'samocc', 'strata')

whit_all_strata <- filter(bdm_whitmaei, zone == "Barrier" | zone == "Great North East Channel" | zone == "Don Cay"| zone == "Cumberland" | zone == "Darnley" | zone == "Seven Reefs") 

whit_no_deep <- filter(whit_all_strata, strata == "Reef top buffer (200 m)" | strata == "Reef top" | strata == "Reef edge")

zone_names <- as_labeller(c("Barrier" = "Barrier", "Cumberland" = "Cumberland", "Darnley" = "Darnely", "Don Cay" = "Don Cay", "Great North East Channel" = "Great NE Channel", "Seven Reefs" = "Seven Reefs"))

ggplot(whit_no_deep, aes(y= holothuria_whitmaei, x = samocc, colour = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ zone, labeller = zone_names) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#stichopus_herrmanni

bdm_herrmanni <- bdm_density %>% 
  select('stichopus_herrmanni', 'zone', 'samocc', 'strata')

herm_all_strata <- filter(bdm_herrmanni, zone == "Barrier" | zone == "Great North East Channel" | zone == "Don Cay"| zone == "Cumberland" | zone == "Darnley" | zone == "Seven Reefs") 

herm_no_deep <- filter(herm_all_strata, strata == "Reef top buffer (200 m)" | strata == "Reef top" | strata == "Reef edge")

zone_names <- as_labeller(c("Barrier" = "Barrier", "Cumberland" = "Cumberland", "Darnley" = "Darnely", "Don Cay" = "Don Cay", "Great North East Channel" = "Great NE Channel", "Seven Reefs" = "Seven Reefs"))

ggplot(herm_no_deep, aes(y= stichopus_herrmanni, x = samocc, colour = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ zone, labeller = zone_names) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#holothuria_ananas

bdm_ananas <- bdm_density %>% 
  select('holothuria_ananas', 'zone', 'samocc', 'strata')

ananas_all_strata <- filter(bdm_ananas, zone == "Barrier" | zone == "Great North East Channel" | zone == "Don Cay"| zone == "Cumberland" | zone == "Darnley" | zone == "Seven Reefs") 

ananas_no_deep <- filter(ananas_all_strata, strata == "Reef top buffer (200 m)" | strata == "Reef top" | strata == "Reef edge")

zone_names <- as_labeller(c("Barrier" = "Barrier", "Cumberland" = "Cumberland", "Darnley" = "Darnely", "Don Cay" = "Don Cay", "Great North East Channel" = "Great NE Channel", "Seven Reefs" = "Seven Reefs"))

ggplot(ananas_no_deep, aes(y= holothuria_ananas, x = samocc, colour = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ zone, labeller = zone_names) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#holothuria_fuscogilva

bdm_fusco <- bdm_density %>% 
  select('holothuria_fuscogilva', 'zone', 'samocc', 'strata')

fusco_all_strata <- filter(bdm_fusco, zone == "Barrier" | zone == "Great North East Channel" | zone == "Don Cay"| zone == "Cumberland" | zone == "Darnley" | zone == "Seven Reefs") 

fusco_no_deep <- filter(fusco_all_strata, strata == "Reef top buffer (200 m)" | strata == "Reef top" | strata == "Reef edge") 

zone_names <- as_labeller(c("Barrier" = "Barrier", "Cumberland" = "Cumberland", "Darnley" = "Darnely", "Don Cay" = "Don Cay", "Great North East Channel" = "Great NE Channel", "Seven Reefs" = "Seven Reefs"))

  ggplot(fusco_no_deep, aes(y= holothuria_fuscogilva, x = samocc, colour = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ zone, labeller = zone_names) +
  scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#need to put on an average trend line for strata and zone


  plot_fuscog <- ggplot(fusco_no_deep, aes(y= holothuria_fuscogilva, x = samocc, colour = strata)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ zone, labeller = zone_names) +
    labs(y= "Holothuria fuscogilva", x = "Survey year", colour = "Strata:") +
    geom_smooth(se = FALSE, colour = "black") +
    scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
    theme(axis.text.x=element_blank(),
          axis.title.y = element_text(face = "italic"),
          axis.title.x = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.key=element_blank())
  
  plot_whit <- ggplot(whit_no_deep, aes(y= holothuria_whitmaei, x = samocc, colour = strata)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ zone, labeller = zone_names) +
    labs(y= "Holothuria whitamaei", x = "Survey year", colour = "Strata:") +
    geom_smooth(se = FALSE, colour = "black") +
    scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
    theme(axis.text.x=element_blank(),
          axis.title.y = element_text(face = "italic"),
          axis.title.x = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.key=element_blank())
  
  plot_herm <- ggplot(herm_no_deep, aes(y= stichopus_herrmanni, x = samocc, colour = strata)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ zone, labeller = zone_names) +
    labs(y= "Stichopus herrmannii", x = "Survey year", colour = "Strata:") +
    geom_smooth(se = FALSE, colour = "black") +
    scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
    theme(axis.text.x=element_text(angle=50, hjust=1),
          axis.title.y = element_text(face = "italic"),
          strip.text = element_text(face = "bold"),
          legend.key=element_blank())
  
  plot_anan <- ggplot(ananas_no_deep, aes(y= holothuria_ananas, x = samocc, colour = strata)) +
    geom_point() +
    geom_line () +
    facet_wrap(~ zone, labeller = zone_names) +
    labs(y= "Thelenota ananas", x = "Survey year", colour = "Strata:") +
    geom_smooth(se = FALSE, colour = "black") +
    scale_x_continuous(breaks = c(1995, 2002, 2005, 2009, 2019)) +
    theme(axis.text.x=element_text(angle=50,hjust=1),
          axis.title.y = element_text(face = "italic"),
          strip.text = element_text(face = "bold"),
          legend.key=element_blank())
  
  #stat_smooth(method = 'lm', formula = y ~ x, se = FALSE) +

library(ggplot2)
library("ggpubr")
  
figure <- ggarrange(plot_fuscog, plot_whit, plot_herm, plot_anan,
          labels = c("A" , "B", "C", "D", element_text(size = 8)),
          ncol = 2, nrow = 2,
          common.legend = TRUE, legend = "bottom") +
          labs(title = "Average density of sea cucumber species in Torres Strait zones\nwithin habitat strata, across survey years") +
          theme(plot.title = element_text(size = 15, face = "bold",
                margin = margin(10, 0, 10, 0)),
                axis.title.x = element_text(vjust = -0.35),
                axis.title.y = element_text(vjust = 0.35),
                plot.background=element_rect(fill="lightblue"),
                plot.margin = unit(c(1, 1, 1, 1), "cm")) #top, right, bottom, left

ggsave("plots/figure.png", height = 20, width = 25, units="cm", dpi=200)

ggexport(figure, filename = "figure1.pdf")        



#ggexport

library(cowplot)

all_plot_bdm <- plot_grid(plot_fuscog, plot_whit, plot_herm, plot_anan) 
    
ggsave("plots/all_plot_bdm.png", height = 15, width = 20, units="cm", dpi=200)    

#theme(plot_fusco, (legend.position = 'hidden'), 
        #plot_whit, (legend.position = 'hidden'), 
        #plot_herm, (legend.position = 'hidden'), 
        #plot_anan, (legend.postion = 'right'))







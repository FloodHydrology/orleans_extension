#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Circle plots
#Date: 6/7/2022
#Coder: Nate Jones (cnjones7@ua.edu)
#Purpose: Create initial plots for manuscript in prep
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#load libraries of interest
library('tidyverse')
library('readxl')
library('patchwork')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Tidy Data -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2016 data
df1 <- read_xlsx(
  'data/database.xlsx', 
  sheet = '2016_Perception', 
  skip =1) %>% 
  select(ID = '...1',Taste, Odor, Color, Stain, Particles) %>% 
  pivot_longer(-ID) %>% 
  mutate(year = 2016)

#2021 data
df2 <- read_xlsx(
  'data/database.xlsx', 
  sheet = '2021_Perception', 
  skip =1) %>% 
  select(ID = '...1',Taste, Odor, Color, Stain, Particles) %>% 
  pivot_longer(-ID) %>% 
  mutate(year = 2021)

#Group
group <- read_xlsx('data/database.xlsx', sheet = '2021 ID') 

#Combine
df <- bind_rows(df1, df2) %>% left_join(., group)

#Estimate Proportion of yes by observation type
df<-df %>% 
  filter(Municpal=='Yes') %>% 
  mutate(value = if_else(value == 'Yes', 1, 0)) %>% 
  group_by(year, name) %>% 
  summarise(total = n(), 
            yes = sum(value)) %>% 
  mutate(prop = yes/total)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Plots! --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_2016<-df %>% 
  filter(year == 2016) %>% 
  arrange(prop) %>%  
  mutate(name = factor(name, name)) %>% 
  ggplot(aes(x=name, y=prop)) +
  geom_segment(
    aes(x=name, xend = name, y=0, yend=prop), 
    col="grey", 
    size = 1.3) +
  geom_point(
    size=4, 
    col = "#E57200"
  ) +
  ylim(0, 0.65) +
  coord_flip() + 
  theme_bw() + 
  ggtitle("Pre-Municipal Extension [2016]") +
  ylab("Proportion Reported") +
  theme(
    plot.title = element_text(size = 14), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
    ) 

p_2021<-df %>% 
  filter(year == 2021) %>% 
  arrange(prop) %>%  
  mutate(name = factor(name, c('Particles', 'Color', 'Odor', 'Taste', 'Stain'))) %>% 
  ggplot(aes(x=name, y=prop)) +
  geom_segment(
    aes(x=name, xend = name, y=0, yend=prop), 
    col="grey", 
    size = 1.3) +
  geom_point(
    size=4, 
    col = "#232D4B"
  ) +
  ylim(0, 0.65) +
  coord_flip() + 
  theme_bw() + 
  ggtitle("Post-Municipal Extension [2021]") +
  ylab("Proportion Reported") +
  theme(
    plot.title = element_text(size = 14), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
  ) 

#Create plot
p_2016 + p_2021 + plot_layout(ncol=2)
ggsave('docs/circleplot.png', width = 7, height = 4, units="in", dpi=300)
#https://floodhydrology.github.io/orleans_extension/boxplot.png

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Fancier Plot! -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1 <- df %>% filter(year == 2016)

df %>% 
  mutate(
    x = if_else(name == 'Stain', 5, 0), 
    x = if_else(name == 'Taste', 4, x), 
    x = if_else(name == 'Odor',  3, x), 
    x = if_else(name == 'Color', 2, x), 
    x = if_else(name == 'Particles', 1, x), 
    x = if_else(year == 2016, x+0.1, x-0.1)
  ) %>% 
  ggplot() +
  geom_segment(
    aes(x=x, xend = x, y=0, yend=prop), 
    col="grey", 
    size = 1.3) +
  geom_point(
    aes(x=x, y=prop, col=year),
    size=4, 
    col = c("#E57200", "#232D4B","#E57200", "#232D4B","#E57200", "#232D4B","#E57200", "#232D4B","#E57200", "#232D4B")
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels=c('Particles', 'Color', 'Odor', 'Taste', 'Stain'), 
    ) +
  coord_flip() + 
  theme_bw() + 
  ggtitle("Aesthetic Concerns") +
  ylab("Proportion Reported") +
  theme(
    plot.title = element_text(size = 14), 
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
  ) 

ggsave('docs/circleplot2.png', width = 3.5, height = 4, units="in", dpi=300)

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
  select(ID = '...1',Corrosion, Taste, Odor, Color, Stain, Particles) %>% 
  pivot_longer(-ID) %>% 
  mutate(year = 2016)

#2021 data
df2 <- read_xlsx(
  'data/database.xlsx', 
  sheet = '2021_Perception', 
  skip =1) %>% 
  select(ID = '...1',Corrosion, Taste, Odor, Color, Stain, Particles) %>% 
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
#Step 3: Lolli pop plot! -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rank
rank<- tibble(
  name = c('Stain', 'Corrosion', 'Taste', 'Odor', 'Color', 'Particles'),
  x    = seq(6,1)
)

#Isolate 2016 data
df1 <- df %>% filter(year == 2016)

#Join rank
df<-df %>% 
  left_join(., rank) %>% 
  mutate(
    x = if_else(year == 2016, x+0.1, x-0.1),
    period = if_else(year == 2016, "Pre-extension", "Post-extension")) 

#Prep labels for groups
df$period <- factor(df$period, levels = c("Pre-extension", "Post-extension"))

#Start ggplot
ggplot(df) + 
  geom_segment(
    aes(x=x, xend = x, y=0, yend=prop), 
    col="grey", 
    size = 1.3) +
  geom_point(
    aes(x, prop, colour=factor(period)), 
    size = 4) + 
  scale_color_manual(values=c("#E57200", "#232D4B")) +  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6),
    labels=c('Particles', 'Color', 'Odor', 'Taste','Corrosion', 'Stain'), 
    ) +
  coord_flip() + 
  theme_bw() + 
  xlab("Aesthetic Concerns") +
  ylab("Proportion Reported") +
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
    legend.position="bottom", 
    legend.title = element_blank(),
    legend.text = element_text(size=10)
  ) 

ggsave('docs/circleplot.png', width = 4.25, height = 4, units="in", dpi=300)

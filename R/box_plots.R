#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Plots 
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
    sheet = '2016_Water quality', 
    skip =1) %>% 
  select(ID = '...1',NA_ppb = 'Na', Cl_ppm='Cl (ppm)') %>% 
  mutate(
    year= 2016,
    NA_ppm=NA_ppb/1000) %>% 
  select(-NA_ppb)
  
#2021 data
df2 <- read_xlsx(
  'data/database.xlsx', 
  sheet = '2021_Water quality', 
  skip =1) %>% 
  select(ID = '...1',NA_ppb = 'Na', Cl_ppm='Cl (ppm)') %>% 
  mutate(
    year = 2021,
    NA_ppm=NA_ppb/1000) %>% 
  select(-NA_ppb)

#Group
group<- read_xlsx('data/database.xlsx', sheet = '2021 ID') 

#Combine
df<-bind_rows(df1, df2) %>% left_join(., group)

#prep for plotting
df<-df %>% 
  filter(Municpal=='Yes') %>% 
  mutate(year = paste(year)) %>% 
  mutate(
    Groups = if_else(Groups=="Salt", "1_salt", Groups), 
    Groups = if_else(Groups=="Major", "2_major", Groups), 
    Groups = if_else(Groups=="Minor", "3_minor", Groups), 
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Plots plots plots ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#conduct wilcoxon test for funzies
kruskal.test(df$Cl_ppm, df$year)
kruskal.test(df$NA_ppm, df$year)

#Chloride
cl<-df %>% 
  ggplot(., aes(x=Groups, y=Cl_ppm, fill=year)) + 
  #Plot contents
  geom_boxplot() + 
  #Theme
  theme_bw()+
  #Color Options
  scale_fill_manual(values=c("#E57200", "#232D4B","#E57200", "#232D4B","#E57200", "#232D4B")) +
  #Axes
  coord_cartesian(ylim=c(10,1000)) +
  scale_y_log10(
    breaks = c(10,100,1000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  ylab(expression(Chloride~textstyle({'['})*mg%.%L^{-1}*textstyle({']'}))) +
  scale_x_discrete(labels=c("Salt Barn","Major Roads","Minor Roads")) +
  theme(
    axis.title.y = element_text(size = 12), 
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    legend.position = 'NONE') 
  
na<-df %>% 
  ggplot(., aes(x=Groups, y=NA_ppm, fill=year)) + 
  #Plot contents
  geom_boxplot() + 
  #Theme
  theme_bw()+
  #Color Options
  scale_fill_manual(values=c("#E57200", "#232D4B","#E57200", "#232D4B","#E57200", "#232D4B")) +
  #Axes
  coord_cartesian(ylim=c(10,1000)) +
  scale_y_log10(
    breaks = c(10,100,1000),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  ylab(expression(Sodium~textstyle({'['})*mg%.%L^{-1}*textstyle({']'}))) +
  scale_x_discrete(labels=c("Salt Barn","Major Roads","Minor Roads")) +
  theme(
    axis.title.y = element_text(size = 12), 
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
    legend.title = element_text(size = 10),
    legend.text = element_text(size=8),
    legend.position = "bottom"
  )+
  guides(fill=guide_legend(title = "Sampling Year"))


#Create plot
cl + na + plot_layout(ncol=1) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ".)") &
  theme(plot.tag = element_text(size = 12))
ggsave('docs/boxplot.png', width = 4, height = 5, units="in", dpi=300)
  #https://floodhydrology.github.io/orleans_extension/boxplot.png

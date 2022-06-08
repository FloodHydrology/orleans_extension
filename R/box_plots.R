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
  ggplot(., aes(x=Groups, y=Cl_ppm, fill=interaction(Groups, year))) + 
  #Plot contents
  geom_boxplot() + 
  #Theme
  theme_bw()+
  #Color Options
  scale_fill_manual(values=c("#e31a1c","#ff7f00", "#1f78b4", "#e31a1c80" ,"#ff7f0080",  "#1f78b480")) +
  #Axes
  scale_y_log10()+
  ylab("Cl [ppm]") +
  scale_x_discrete(labels=c("Salt Barn","Major Roads","Minor Roads")) +
  theme(
    axis.title.y = element_text(size = 14), 
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
    legend.position = "none"
  ) 

#NA
na<-df %>% 
  ggplot(., aes(x=Groups, y=NA_ppm, fill=interaction(Groups, year))) + 
  #Plot contents
  geom_boxplot() + 
  #Theme
  theme_bw()+
  #Color Options
  scale_fill_manual(values=c("#e31a1c","#ff7f00", "#1f78b4", "#e31a1c80" ,"#ff7f0080",  "#1f78b480")) +
  #Axes
  scale_y_log10()+
  ylab("Na [ppm]") +
  scale_x_discrete(labels=c("Salt Barn","Major Roads","Minor Roads")) +
  theme(
    axis.title.y = element_text(size = 14), 
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
    legend.position = "none"
  ) 

#Ratio
ratio<-df %>% 
  mutate(ratio = Cl_ppm/NA_ppm) %>% 
  ggplot(., aes(x=Groups, y=ratio, fill=interaction(Groups, year))) + 
  #Plot contents
  geom_boxplot() + 
  #Theme
  theme_bw()+
  #Color Options
  scale_fill_manual(values=c("#e31a1c","#ff7f00", "#1f78b4", "#e31a1c80" ,"#ff7f0080",  "#1f78b480")) +
  #Axes
  ylab("Cl/Na Ratio") +
  scale_x_discrete(labels=c("Salt Barn","Major Roads","Minor Roads")) +
  theme(
    axis.title.y = element_text(size = 14), 
    axis.title.x = element_blank(),
    axis.text.y  = element_text(size = 10),
    axis.text.x  = element_text(size = 10), 
    legend.position = "none"
  ) 

#Create plot
cl + na + ratio + plot_layout(ncol=1)
ggsave('docs/boxplot.png', width = 3.7, height = 7, units="in", dpi=300)
  #https://floodhydrology.github.io/orleans_extension/boxplot.png

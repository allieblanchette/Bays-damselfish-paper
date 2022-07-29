###################################################################
# Purpose: This code re-organizes the raw data to be loaded into the figures and analyses scripts for Blanchette et al. 2022 - Effects of onshore development and damselfish (Stegastes nigricans) on coral richness in Opunohu Bay and Cook's Bay in Moorea, French Polynesia 
# Last updated: July 29, 2022
# By: Allie Blanchette
# Raw data available at: https://data.mendeley.com/datasets/whxw97nw86/1
###################################################################

library(tidyverse) #for data organization
library(dplyr) #for rename function





# Data used for figures code:

## Available in Mendeley
### Quadrat_condensed.csv (for Fig3)
### Coral Data.csv (for Fig4, plus setting up PCA.csv)
### Quadrat Data.csv (for Fig4, plus setting up PCA.csv)
### Data_table1_quad.csv (for Fig6, plus setting up PCA.csv)
### ByCoral_long.csv (for Fig6)


## To be set up using data from Mendeley
### PCA.csv (for Fig5, FigS1) -- puts data into a form suitable for PCA
Data_table1<-read.csv("Data_table1_quad.csv")
Quadrat_Data<-read.csv("Quadrat Data.csv")
Coral_Data<-read.csv("Coral Data.csv")

Coral_cover_join<-Quadrat_Data %>% 
  select(Bay, Site.old, Quad.updated, Coral.Sum..PC._counts) %>% 
  mutate(Coral_per=(Coral.Sum..PC._counts/81)*100,
         Coral.counts=Coral.Sum..PC._counts) %>% 
  select(-Coral.Sum..PC._counts)

Abuns.Quad<-Coral_Data %>% 
  group_by(Bay, Site.old, Quad.updated, DFT.present, Coral) %>% 
  summarize(sum.Abun=length(Coral)) %>% 
  spread(Coral, sum.Abun, fill=0) 

PCA<-merge(Data_table1, Abuns.Quad) %>% 
  select(-Abun.JUV, -Rich.JUV, -Abun.JUV.NonPL, -Rich.JUV.NonPL, -Abun.ADT, -Rich.ADT) %>% 
  mutate(Opunohu=Bay,
         Cooks=Bay,
         Opunohu=recode(Opunohu, "Cook's"="0"),
         Opunohu=recode(Opunohu, "Opunohu"="1"),
         Cooks=recode(Cooks, "Cook's"="1"),
         Cooks=recode(Cooks, "Opunohu"="0"),
         Opunohu=as.numeric(as.character(Opunohu)),
         Cooks=as.numeric(as.character(Cooks))) %>% 
  left_join(Coral_cover_join)

write.csv(PCA, file="PCA.csv", row.names = F)











# Data loaded into analysis code:

## Available in Mendeley
### None


## To be set up using data from Mendeley
### PCA.csv (for GLM effect of Bay/Site; set-up code is above L21-L49) -- puts data into a form suitable for PCA


### Rich.TOT.csv (for GLM effect of visibility) -- small data frame of total coral taxanomic richness and average coral size per site
Richcount<-function(x){
  x1<-x[x!=0]
  x2<-unique(x1)
  length(x2)
}

Coral_Data<-read.csv("Coral Data.csv")

Rich.TOT<-Coral_Data %>% 
  filter(Coral!="n/a") %>% 
  mutate(Size..cm.=as.numeric(as.character(Size..cm.))) %>% 
  group_by(Bay, Site.updated) %>% 
  summarize(t.rich=Richcount(Coral),
            m.size.2=mean(Size..cm., na.rm=T))

write.csv(Rich.TOT, file = "Rich.TOT.csv", row.names = F)


### PCA_data.csv (for GLM effect of visibility; for GLMEM effect of S. nigricans on coral community) -- adds zcores for PCA.csv and removes some non-useful data columns
PCA_data<-read.csv("PCA.csv") %>% 
  select(-Abun.NonPL, -Rich.NonPL, -Abun.PL, -Size.NonPL, -Size.PL) %>% 
  mutate(Rich.z = (Rich.ALL - mean(Rich.ALL))/sd(Rich.ALL),
         CCA.z = (CCA.per - mean(CCA.per))/sd(CCA.per),
         Macro.z = (Macro.per - mean(Macro.per))/sd(Macro.per),
         TurfHeight.z = (Average.Turf.Height - mean(Average.Turf.Height))/sd(Average.Turf.Height),
         Turf.z = (Turf.per - mean(Turf.per))/sd(Turf.per)) %>% 
  rename("Porites lobata"=Porites.lobata,
         "Cyphastrea sp."=Cyphastrea.sp.,
         "Fungia fungites"=Fungia.fungites,
         "Gardineroseris planulata"=Gardineroseris.planulata,
         "Herpolitha limax"=Herpolitha.limax,
         "Leptastrea sp."=Leptastrea.sp,
         "Leptoseris sp."=Leptoseris.sp,
         "Astrea curta"=Montastrea.curta,
         "Montipora sp."=Montipora.sp,
         "Pavona cactus"=Pavona.cactus,
         "Pocillopora sp."=Pocillopora.sp,
         "Porites rus"=Porites.rus,
         "Psammocora contigua"=Psammocora.obtusangula,
         "Psammocora profundacella"=Psammocora.profundacella,
         "Stylocoeniella armata"=Stylocoeniella.armata,
         "CCA"=CCA.per,
         "Macroalgae"=Macro.per,
         "Turf"=Turf.per,
         #"Damselfish Territory"=DFT.present,
         "Turf Height"=Average.Turf.Height,
         "Visibility"=secchi.avg..3m.)

write.csv(PCA_data, file="PCA_data.csv", row.names=F)









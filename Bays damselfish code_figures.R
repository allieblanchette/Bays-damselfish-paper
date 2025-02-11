###################################################################
# Purpose: This code creates the FIGURES in Blanchette et al. 2022 - Effects of onshore development and damselfish (Stegastes nigricans) on coral richness in Opunohu Bay and Cook's Bay in Moorea, French Polynesia 
# Last updated: June 22, 2022
# By: Allie Blanchette
# Raw data available at: https://data.mendeley.com/datasets/whxw97nw86/1
###################################################################

#R Studio version 1.2.1335
#R version 3.6.1

# Note - Some of the .csv data is available directly on Mendeley and some requires further set-up. For code for the data set-up and explanation, see 'Bays damselfish code_data set-up.R'



library(tidyverse) #for data organization
library(dplyr) #for rename function
# library(reshape) #for melt function (masks reshape, expand, smiths, stamp functions) 
library(viridisLite) #for colorblind color palette
library(viridis) #for colorblind color palette
library(cowplot) #for draw_plot function
library(ggplot2)
library(ggfortify) #for plotting PCA
library(ggrepel) #for plotting PCA
library(scales) #for scaling axis labels
theme_set(theme_bw(15))

setwd("C:\\Users\\rassweiler\\Documents\\Other stuff\\MBQ\\Jacobs\\For Mendeley and github")


#FIGURE 3 - Quadrat point count
# library(reshape) #for melt function #masks rename function, so use as needed
Quad_Scatter1<-read.csv("Quadrat_condensed.csv")

QS2<-melt(Quad_Scatter1, id=c("Bay", "Site.old", "Site.updated", "Quad.updated", "DFT.present", "Total.sum")) %>% 
  mutate(variable=recode(variable, Crustose.coralline.algae="CCA"),
         variable=recode(variable, Sandy.sediment="Sediment"),
         variable=recode(variable, Hard.Coral="Hard coral"),
         variable=recode(variable, Hard.substrate="Rock"),
         Site.duplicated=Site.updated,
         Site.updated=as.character(Site.updated),
         Site.updated=recode(Site.updated, "1" ="O1"),
         Site.updated=recode(Site.updated, "2" ="O2"),
         Site.updated=recode(Site.updated, "3" ="O3"),
         Site.updated=recode(Site.updated, "4" ="O4"),
         Site.updated=recode(Site.updated, "5" ="O5"),
         Site.updated=recode(Site.updated, "6" ="C6"),
         Site.updated=recode(Site.updated, "7" ="C7"),
         Site.updated=recode(Site.updated, "8" ="C8"),
         Site.updated=recode(Site.updated, "9" ="C9"),
         Site.updated=recode(Site.updated, "10" ="C10"),
         DFT.present=as.character(DFT.present),
         Site.old=as.character((Site.old)),
         percent=value/.81)

QS3<-QS2 %>% 
  group_by(Bay, Site.old, Site.updated,Site.duplicated,variable) %>% 
  summarize(mean_counts=mean(value),
            mean_percent=mean(percent),
            sd_c=sd(value),
            n_c=length(value),
            sd_p=sd(percent),
            n_p=length(percent)) %>% 
  mutate(se_c=sd_c/sqrt(n_c),
         se_p=sd_p/sqrt(n_p))

AvgCoral.Bay<-QS2 %>% 
  group_by(Bay, variable) %>% 
  summarize(mean_counts=mean(value),
            mean_percent=mean(percent),
            sd_c=sd(value),
            n_c=length(value),
            sd_p=sd(percent),
            n_p=length(percent)) %>% 
  mutate(se_c=sd_c/sqrt(n_c),
         se_p=sd_p/sqrt(n_p))

Avg.Coral<-QS2 %>% 
  group_by(variable) %>% 
  summarize(mean_counts=mean(value),
            mean_percent=mean(percent),
            sd_c=sd(value),
            n_c=length(value),
            sd_p=sd(percent),
            n_p=length(percent)) %>% 
  mutate(se_c=sd_c/sqrt(n_c),
         se_p=sd_p/sqrt(n_p))

Fig3<-
  ggplot(data=QS3, aes(x=reorder(Site.updated, Site.duplicated), y=mean_percent, fill=variable))+
  geom_bar(color="black", width = 0.75,stat="identity")+
    scale_fill_manual(values=c("#bdb0a4","#fac228","#fcffa4",
                               "#1357d1","#81baeb","#65156e",
                               "#d61c5a","#f57d15"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15,angle = 45, hjust = 1), 
        axis.text.y = element_text(color="black",size=15))+
  xlab("Site")+
  ylab("Average Percent Cover")+
  geom_hline(yintercept = 5.3, linetype="dashed", size = 1.25,color="gray60")+
  geom_hline(yintercept =4.2, linetype="dashed", size=1.25)







#FIGURE 4 - DFT and coral species abundances per site
Coral_Data<-read.csv("Coral Data.csv")
QuadData<-read.csv("Quadrat Data.csv")

Abuns.DFT<-QuadData %>% 
  select(Bay, Site.old, Site.updated, DFT.present) %>% 
  mutate(across(Bay, factor, levels=c("Opunohu","Cook's"))) %>% 
  group_by(Bay, Site.updated, Site.old) %>% 
  summarize(DFTsums=sum(DFT.present))

Abuns.Corals<-Coral_Data %>% 
  filter(Coral!="n/a") %>% 
  mutate(across(Bay, factor, levels=c("Opunohu","Cook's")),
         across(DFT.present, factor, levels=c("1","0"))) %>% 
  group_by(Bay, Site.updated, Site.old, Coral, DFT.present) %>% 
  summarize(Coral.Abun=length(Coral)) %>% 
  mutate(Coral=as.factor(recode(Coral, Cyphastrea.sp.="Cyphastrea sp.")),
         Coral=as.factor(recode(Coral, Fungia.fungites="Fungia fungites")),
         Coral=as.factor(recode(Coral, Gardineroseris.planulata="Gardineroseris planulata")),
         Coral=as.factor(recode(Coral, Herpolitha.limax="Herpolitha limax")),
         Coral=as.factor(recode(Coral, Leptastrea.sp="Leptastrea sp.")),
         Coral=as.factor(recode(Coral, Leptoseris.sp="Leptoseris sp.")),
         Coral=as.factor(recode(Coral, Montastrea.curta="Montastrea curta")),
         Coral=as.factor(recode(Coral, Montipora.sp="Montipora sp.")),
         Coral=as.factor(recode(Coral, Pavona.cactus="Pavona cactus")),
         Coral=as.factor(recode(Coral, Pocillopora.sp="Pocillopora sp.")),
         Coral=as.factor(recode(Coral, Porites.lobata="Porites lobata")),
         Coral=as.factor(recode(Coral, Porites.rus="Porites rus")),
         Coral=as.factor(recode(Coral, Psammocora.obtusangula="Psammocora obtusangula")),
         Coral=as.factor(recode(Coral, Psammocora.profundacella="Psammocora profundacella")),
         Coral=as.factor(recode(Coral, Stylocoeniella.armata="Stylocoeniella armata")))

Abuns.Corals$Coral<-factor(Abuns.Corals$Coral, levels = c("Cyphastrea sp.",
                                                          "Fungia fungites",
                                                          "Gardineroseris planulata",
                                                          "Herpolitha limax", 
                                                          "Leptastrea sp.", 
                                                          "Leptoseris sp.", 
                                                          "Montastrea curta",
                                                          "Montipora sp.",
                                                          "Pavona cactus",
                                                          "Pocillopora sp.",
                                                          "Porites lobata", 
                                                          "Porites rus",
                                                          "Psammocora obtusangula",
                                                          "Psammocora profundacella",
                                                          "Stylocoeniella armata"))



Fig4ab<-
ggplot(Abuns.DFT, aes(x=Site.old, y=DFTsums))+
  geom_bar(stat="identity", width=0.75, fill="black")+
  facet_wrap(~ Bay,
             scales = 'fixed',
             labeller = labeller(Bay = 
                                   c("Opunohu" = "Opunohu Bay",
                                     "Cook's" = "Cook's Bay")))+
  ylim(0,15)+
  ylab(expression(paste("Sum of ", italic(" S. nigricans"), " territories")))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color="black",size=12),
        axis.title.y=element_text(color="black", size=12),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing=unit(2,"line"))


Fig4cdef<-
ggplot(Abuns.Corals, aes(x=Site.old, y=Coral.Abun, fill=Coral))+
  geom_bar(color="black", width = 0.75,stat="identity")+
  facet_wrap(~ DFT.present+Bay,
             scales = 'fixed')+
  ylim(0,230)+
  ylab("Total Coral Abundance")+
  scale_fill_manual(values=c("#000000","#004799","#009292",
                             "deeppink2","#fad2e6","#52178f",
                             "dodgerblue","#b66dff","cyan",
                             "lightblue","#920000","#924900",
                             "#db6d00","#24ff24","#ffff6d"),
                    labels = c(
                      expression(paste(italic("Cyphastrea sp."))),
                      expression(paste(italic("Fungia fungites"))),
                      expression(paste(italic("Gardineroseris planulata"))),
                      expression(paste(italic("Herpolitha limax"))),
                      expression(paste(italic("Leptastrea sp."))),
                      expression(paste(italic("Leptoseris sp."))),
                      expression(paste(italic("Astrea curta"))),
                      expression(paste(italic("Montipora sp."))),
                      expression(paste(italic("Pavona cactus"))),
                      expression(paste(italic("Pocillopora sp."))),
                      expression(paste(italic("Porites lobata"))),
                      expression(paste(italic("Porites rus"))),
                      expression(paste(italic("Psammocora contigua"))),
                      expression(paste(italic("Psammocora profundacella"))),
                      expression(paste(italic("Stylocoeniella armata")))))+
  theme(
    axis.text.x = element_text(color="black",size=12), 
    axis.title.x = element_blank(), 
    axis.text.y=element_text(color="black", size=12),
    axis.title.y=element_text(color="black", size=12),
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title=element_blank(),
    legend.text=element_text(size=10),
    panel.grid=element_blank(),
    strip.text.x = element_blank(),
    panel.spacing=unit(2,"line"))+
  guides(fill=guide_legend(ncol=3))








#FIGURE 5 - PCA

#prepping PCA data
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

PCA_8sites<-PCA_data %>%
  filter(Site.old!="5") %>% 
  select(-n.a, Visibility, secchi.avg..surf.) %>% 
  mutate(Damselfish.Territory.f=as.character(DFT.present),
         Damselfish.Territory.f=recode(Damselfish.Territory.f, "1"="Present"),
         Damselfish.Territory.f=recode(Damselfish.Territory.f, "0"="Absent"),
         "Coral Richness"=Rich.z,
         "CCA"=CCA.z,
         "Macroalgae"=Macro.z,
         "Turf"=Turf.z,
         "Turf Height"=TurfHeight.z) %>% 
  rename("Pl"="Porites lobata",
         "Cs"="Cyphastrea sp.",
         "Ff"="Fungia fungites",
         "Gp"= "Gardineroseris planulata",
         "Hl"="Herpolitha limax",
         "La"="Leptastrea sp.",
         "Lo"="Leptoseris sp.",
         "Ac"="Astrea curta",
         "Ms"="Montipora sp.",
         "Pc"="Pavona cactus",
         "Ps"="Pocillopora sp.",
         "Pr"="Porites rus",
         "Pm"="Psammocora contigua",
         "Pp"="Psammocora profundacella",
         "Sa"="Stylocoeniella armata")

corals2<-prcomp(PCA_8sites[,c("Cs",
                              "Ff",
                              "Gp",
                              "Hl",
                              "La",
                              "Lo",
                              "Ac",
                              "Ms",
                              "Pc",
                              "Ps",
                              "Pl",
                              "Pr",
                              "Pm",
                              "Pp",
                              "Sa")],
                center=TRUE, scale.=TRUE)

benthic3<-prcomp(PCA_8sites[,c("Coral Richness",
                               "CCA",
                               "Macroalgae",
                               "Turf",
                               "Turf Height")],
                 center=TRUE, scale.=TRUE)

#plotting PCAs
corals<-
  autoplot(corals2, 
           data=PCA_8sites,
           colour="Damselfish.Territory.f",
           shape="Damselfish.Territory.f",
           size=1.25,
           loadings=TRUE, 
           loadings.label=TRUE, 
           loadings.colour="black", 
           loadings.label.colour="black",
           loadings.label.repel=T,
           loadings.label.size=3)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray")+
  geom_vline(xintercept=0, linetype="dashed", color = "gray")+
  scale_shape_manual(expression(paste(italic("S. nigricans "),"Territory")),
                     values= c("circle","triangle"),
                     labels= c(Absent="Absent",Present="Present"))+
  scale_color_manual(expression(paste(italic("S. nigricans "),"Territory")),
                     values=c("lightblue","orange"),
                     labels= c(Absent="Absent",Present="Present"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=12), 
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        axis.title.y=element_text(color="black", size=12),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))

benthic<-
  autoplot(benthic3,
           data=PCA_8sites,
           colour="Damselfish.Territory.f",
           shape="Damselfish.Territory.f",
           size=1.25,
           loadings=TRUE, 
           loadings.label=TRUE, 
           loadings.colour="black",
           loadings.label.colour="black",
           loadings.label.repel=T,
           loadings.label.size=3)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray")+
  geom_vline(xintercept=0, linetype="dashed", color = "gray")+
  scale_shape_manual(expression(paste(italic("S. nigricans ")," Territory")),
                     values= c("circle","triangle"),
                     labels= c(Absent="Absent",Present="Present"))+
  scale_color_manual(expression(paste(italic("S. nigricans ")," Territory")),
                     values=c("lightblue","orange"),
                     labels= c(Absent="Absent",Present="Present"))+
  guides(color=guide_legend(title="Damselfish\nTerritory"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",size=12), 
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        axis.title.y=element_text(color="black", size=12),
        legend.position="none")

Fig5<-
  ggdraw()+
  draw_plot(benthic, x=0.03, y=.5, width=.37, height=.48)+
  draw_plot(corals, x=.44, y=.5, width=.57, height=.48)+
  draw_plot_label(label= c("a","b"), size=13,
                  x= c(0.01, 0.47), y = c(1, 1))


# TABLES S3 and S4
summary(corals2)
summary(benthic3)


#FIGURE S2 - scree plots from PCA
var_explained_df_C2 <- data.frame(PC= paste0("PC",1:15),
                                  PC_num=(1:15),
                                  var_explained=(corals2$sdev)^2/sum((corals2$sdev)^2))

var_explained_df_B3 <- data.frame(PC= paste0("PC",1:5),
                                  PC_num= (1:5),
                                  var_explained=(benthic3$sdev)^2/sum((benthic3$sdev)^2))

benthic.scree<-
  var_explained_df_B3 %>%
  ggplot(aes(x=reorder(PC, PC_num),y=var_explained, group=1))+
  geom_point(size=2.5)+
  geom_line()+
  ylab("Variance explained for benthic variables")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(color="black",size=7,angle = 45, hjust = 1),
        axis.text.y = element_text(color="black",size=10),
        axis.title.y=element_text(color="black", size=10))

corals.scree<-
  var_explained_df_C2 %>%
  ggplot(aes(x=reorder(PC, PC_num),y=var_explained, group=1))+
  geom_point(size=2.5)+
  geom_line()+
  ylab("Variance explained for coral taxa")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(color="black",size=10),
        axis.title.y=element_text(color="black", size=10),
        axis.text.x = element_text(color="black",size=7,angle = 45, hjust = 1))

FigS2<-
  ggdraw()+
  draw_plot(benthic.scree, x=0.04, y=.47, width=.43, height=.5)+
  draw_plot(corals.scree, x=.51, y=.47, width=.49, height=.5)+
  draw_plot_label(label= c("a","b"), size=13,
                  x= c(0.01, 0.48), y = c(1, 1))









#Figure 6 - CORAL RICHNESS IN/OUT OF DFT
Data_table1<-read.csv("Data_table1_quad.csv")

ByCoral_long<-read.csv("ByCoral_long.csv") #pretty sure it's excluding sites 5 and 10, but it's confusing to backtrack bc i made the csv in excel (not R) and renamed it *eye roll*

ST1_DFT_filt<-Data_table1 %>% 
  filter(Site.old!="5") %>% 
  group_by(DFT.present) %>% 
  summarize(m.DFT=(mean(DFT.present)*100),
            sd.DFT=sd(DFT.present),
            n.DFT=length(DFT.present),
            sum.DFT=sum(DFT.present),
            m.Abun.ALL=mean(Abun.ALL),
            sd.Abun.ALL=sd(Abun.ALL),
            n.Abun.ALL=length(Abun.ALL),
            m.Rich.ALL=mean(Rich.ALL),
            sd.Rich.ALL=sd(Rich.ALL),
            n.Rich.ALL=length(Rich.ALL),
            m.size=mean(avg_size, na.rm=T),
            sd.size=sd(avg_size, na.rm=T),
            n.size=length(avg_size),
            m.TrfHgt=mean(Average.Turf.Height),
            sd.TrfHgt=sd(Average.Turf.Height),
            n.TrfHgt=length(Average.Turf.Height),
            m.CCA=mean(CCA.per),
            sd.CCA=sd(CCA.per),
            n.CCA=length(CCA.per),
            m.Macro=mean(Macro.per),
            sd.Macro=sd(Macro.per),
            n.Macro=length(Macro.per),
            m.Turf=mean(Turf.per),
            sd.Turf=sd(Turf.per),
            n.Turf=length(Turf.per)) %>% 
  mutate(se.DFT=sd.DFT/sqrt(n.DFT),
         se.TrfHgt=sd.TrfHgt/sqrt(n.TrfHgt),
         se.CCA=sd.CCA/sqrt(n.CCA),
         se.Macro=sd.Macro/sqrt(n.Macro),
         se.Turf=sd.Turf/sqrt(n.Turf),
         se.Abun.ALL=sd.Abun.ALL/sqrt(n.Abun.ALL),
         se.Rich.ALL=sd.Rich.ALL/sqrt(n.Rich.ALL),
         se.size=sd.size/sqrt(n.size),
         DFT.present=as.character(DFT.present),
         DFT.present=recode(DFT.present, "0"="Absent"),
         DFT.present=recode(DFT.present, "1"="Present"))

avgrich<-
  ggplot(data = ST1_DFT_filt, aes(x=reorder(DFT.present, n.Abun.ALL), y = m.Rich.ALL))+
  geom_point(color="black", size=3)+
  geom_errorbar(aes(ymin=m.Rich.ALL-se.Rich.ALL, ymax=m.Rich.ALL+se.Rich.ALL), width=0)+
  ylab("Coral Richness")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=12),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(color="black",size=12))

bycoral<-
  ggplot(data = ByCoral_long, aes(x=reorder(DFT.present, m.abun), y = m.abun, group=Coral))+
  geom_point(size=2, aes(color=Coral))+
  geom_errorbar(aes(ymin=m.abun-se, ymax=m.abun+se, color=Coral), width=0)+
  geom_smooth(method="lm", se=F, aes(color=Coral), size=0.75)+
  ylab("Coral Abundance")+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  scale_color_manual(values=c("#000000",
                              "#004949",
                              "#009292",
                              "#ff6db6",
                              "#ffb6db",
                              "#490092",
                              "#006ddb",
                              "#b66dff",
                              "#6db6ff",
                              "#b6dbff",
                              "#920000",
                              "#924900",
                              "#db6d00",
                              "#24ff24",
                              "#ffff6d"),
                     labels = c(
                       expression(paste(italic("Cyphastrea sp."))),
                       expression(paste(italic("Fungia fungites"))),
                       expression(paste(italic("Gardineroseris planulata"))),
                       expression(paste(italic("Herpolitha limax"))),
                       expression(paste(italic("Leptastrea sp."))),
                       expression(paste(italic("Leptoseris sp."))),
                       expression(paste(italic("Astrea curta"))),
                       expression(paste(italic("Montipora sp."))),
                       expression(paste(italic("Pavona cactus"))),
                       expression(paste(italic("Pocillopora sp."))),
                       expression(paste(italic("Porites lobata"))),
                       expression(paste(italic("Porites rus"))),
                       expression(paste(italic("Psammocora contigua"))),
                       expression(paste(italic("Psammocora profundacella"))),
                       expression(paste(italic("Stylocoeniella armata")))))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color="black", size=12),
        axis.text.x = element_text(color="black",size=12), 
        axis.text.y = element_text(color="black",size=12),
        legend.position = "bottom",
        legend.text.align = 0,
        legend.title=element_blank(),
        legend.text=element_text(size=10))+
  guides(color=guide_legend(ncol=2))

Fig6<-
  ggdraw()+
  draw_plot(avgrich, x=0.07, y=.65, width=.59, height=.31)+
  draw_plot(bycoral, x=0.07, y=0, width=.57, height=.66)+
  draw_plot_label(label= c("a","b"), size=15,
                  x= c(0.001, 0.001), y = c(1, 0.67))











#FIGURE S1 - Visibility by sites
PCA<-read.csv("PCA.csv")

PCA_BS<-PCA %>% 
  select(Bay, Site.old, Site.updated, Quad.updated, Coral_per, Coral.counts, Rich.ALL, avg_size, secchi.avg..3m.) %>% 
  rename(vis=secchi.avg..3m.)

BS<-PCA_BS %>% 
  group_by(Bay, Site.old, vis) %>% 
  summarise(m.cover=mean(Coral_per),
            m.counts=mean(Coral.counts),
            m.rich=mean(Rich.ALL),
            m.size=mean(avg_size, na.rm=T)) %>% 
  mutate(m.cover.log=log(m.cover),
         m.counts.log=log(m.counts),
         m.rich.log=log(m.rich),
         m.size.log=log(m.size),
         vis.log=log(vis))

site.labels <- c("1 (6)", "2 (7)", "3 (8)", "4 (9)", "5 (10)")
plot<-BS %>% 
  mutate(Site.old=as.character(Site.old))

FigS1<-
  ggplot(data = plot, aes(x=Site.old, y = vis, shape=Bay))+
  geom_point(size = 3)+
  ylab("Water Clarity (m)")+
  scale_x_discrete(labels= site.labels)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=15), 
        axis.title.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black",size=15), 
        axis.text.y = element_text(color="black",size=15),
        legend.text=element_text(color="black", size=12))+
  xlab("Site")

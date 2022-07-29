###################################################################
# Purpose: This code runs the ANALYSES in Blanchette et al. 2022 - Effects of onshore development and damselfish (Stegastes nigricans) on coral richness in Opunohu Bay and Cook's Bay in Moorea, French Polynesia 
# Last updated: July 29, 2022
# By: Allie Blanchette
# Raw data available at: https://data.mendeley.com/datasets/whxw97nw86/1
###################################################################

#R Studio version 1.2.1335
#R version 3.6.1

# Note - Some of the .csv data is available directly on Mendeley and some requires further set-up. For code and explanation of the further data set-up, see 'Bays damselfish code_data set-up.R'



library(tidyverse) #for data organization
library(dplyr) #for editing dataframe
library(lme4) #for running GLMEM
library(car)
library(DHARMa) #for diagnostics




#GLM - EFFECT OF BAY AND SITE

#preparing bay/site data
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





#GLM - effect of bay and site on CORAL COVER (using raw point counts for cover values)
mc_BS<- glm(m.counts ~ Site.old*Bay, family = "gaussian", data=BS)
Anova(mc_BS, type = 'III')

## checking for normality in coral cover (counts)
par(mfrow=c(2,2)) 
plot(mc_BS)
shapiro.test(BS$m.counts)
hist(BS$m.counts, breaks=10) 





#GLM - effect of bay and site on CORAL RICHNESS
mr_BS<- glm(m.rich ~ Site.old*Bay, family = "gaussian", data=BS)
Anova(mr_BS, type = 'III')

## checking for normality in coral richness
par(mfrow=c(2,2)) 
plot(mr_BS)
shapiro.test(BS$m.rich)
hist(BS$m.rich, breaks=10)





# #GLM - effect of bay and site on CORAL SIZE
ms_BS<- glm(m.size.log ~ Site.old*Bay, family = "gaussian", data=BS)
Anova(ms_BS, type = 'III')

## checking for normality in coral size
par(mfrow=c(2,2)) 
plot(ms_BS)
shapiro.test(BS$m.size.log) #Note - log-transform gets us closer to normality
hist(BS$m.size.log, breaks=10)





#GLM - effect of bay and site on visibility (all 10 sites)
mv_BS<- glm(vis ~ Site.old*Bay, family = "gaussian", data=BS)
Anova(mv_BS, type = 'III')

## checking for normality in visibility (10 sites)
par(mfrow=c(2,2))
plot(mv_BS)
shapiro.test(BS$vis)
hist(BS$vis, breaks=5)





#GLM - effect of bay and site on visibility (excluding sites 5 and 10)
BS_8<-BS %>% 
  filter(Site.old!= "5")

mv8_BS<- glm(vis ~ Site.old*Bay, family = "gaussian", data=BS_8)
Anova(mv8_BS, type = 'III')

## checking for normality in visibility (8 sites)
par(mfrow=c(2,2))
plot(mv8_BS)
shapiro.test(BS_8$vis)
hist(BS_8$vis, breaks=10)





#multiple testing adjustments - Site effect
GLM_S_pvals1<-read.csv("pvalues_GLM BaySite_anova3.csv") %>% 
  filter(Predictor=="Site") %>% 
  select(Response.variable, GLM.p.raw)

GLM_S_pvals2=GLM_S_pvals1[order(GLM_S_pvals1$GLM.p.raw),]

GLM_S_pvals3<-GLM_S_pvals2 %>% 
  select(Response.variable, GLM.p.raw) %>% 
  mutate(Bonferroni=p.adjust(GLM.p.raw,
                             method = "bonferroni"),
         Holm=p.adjust(GLM.p.raw,
                       method = "holm"),
         Hochberg=p.adjust(GLM.p.raw,
                           method = "hochberg"),
         Hommel=p.adjust(GLM.p.raw,
                         method = "hommel"),
         BH_fdr=p.adjust(GLM.p.raw,
                         method = "BH"),
         BY=p.adjust(GLM.p.raw,
                     method = "BY"))





#multiple testing adjustments - Bay effect
GLM_B_pvals1<-read.csv("pvalues_GLM BaySite_anova3.csv") %>% 
  filter(Predictor=="Bay") %>% 
  select(Response.variable, GLM.p.raw)

GLM_B_pvals2=GLM_B_pvals1[order(GLM_B_pvals1$GLM.p.raw),]

GLM_B_pvals3<-GLM_B_pvals2 %>% 
  select(Response.variable, GLM.p.raw) %>% 
  mutate(Bonferroni=p.adjust(GLM.p.raw,
                             method = "bonferroni"),
         Holm=p.adjust(GLM.p.raw,
                       method = "holm"),
         Hochberg=p.adjust(GLM.p.raw,
                           method = "hochberg"),
         Hommel=p.adjust(GLM.p.raw,
                         method = "hommel"),
         BH_fdr=p.adjust(GLM.p.raw,
                         method = "BH"),
         BY=p.adjust(GLM.p.raw,
                     method = "BY"))





#multiple testing adjustments - Interaction
GLM_I_pvals1<-read.csv("pvalues_GLM BaySite_anova3.csv") %>% 
  filter(Predictor=="Interaction") %>% 
  select(Response.variable, GLM.p.raw)

GLM_I_pvals2=GLM_I_pvals1[order(GLM_I_pvals1$GLM.p.raw),]

GLM_I_pvals3<-GLM_I_pvals2 %>% 
  select(Response.variable, GLM.p.raw) %>% 
  mutate(Bonferroni=p.adjust(GLM.p.raw,
                             method = "bonferroni"),
         Holm=p.adjust(GLM.p.raw,
                       method = "holm"),
         Hochberg=p.adjust(GLM.p.raw,
                           method = "hochberg"),
         Hommel=p.adjust(GLM.p.raw,
                         method = "hommel"),
         BH_fdr=p.adjust(GLM.p.raw,
                         method = "BH"),
         BY=p.adjust(GLM.p.raw,
                     method = "BY"))










#GLM - EFFECT OF VISIBILITY

#preparing visibility data
Rich.TOT<-read.csv("Rich.TOT.csv") %>% 
  mutate(Site.updated=as.factor(Site.updated))

site_dat<-read.csv("PCA_data.csv") %>%
  mutate(Site.updated=as.factor(Site.updated),
         Coral_per=as.numeric(as.character(Coral_per))) %>%
  group_by(Bay, Site.old, Site.updated, Visibility) %>%
  summarise(m.cover=mean(Coral_per),
            m.counts=mean(Coral.counts),
            m.rich=mean(Rich.ALL),
            m.size=mean(avg_size)) %>%
  mutate(log.cover=log(m.cover),
         log10.cover=log10(m.cover),
         sqrt.cover=sqrt(m.cover),
         cbrt.cover=(m.cover)^(1/3),
         sq.cover=(m.cover)^2) %>% 
  left_join(Rich.TOT)

site_dat_8<-site_dat %>% 
  filter(Site.old!="5")





#GLM - effect of visibility on CORAL COVER (using raw point counts for cover values)
mc_V <- glm(m.counts ~ Visibility, family = "gaussian", data=site_dat_8)
mc<-glm(m.counts ~ 1, family = "gaussian", data=site_dat_8)
anova(mc_V, mc, test="Chisq")

#checking for normality in coral cover
par(mfrow=c(2,2))
plot(mc_V)
shapiro.test(site_dat_8$m.counts)
hist(site_dat_8$m.counts, breaks=5)





#GLM - effect of visibility on CORAL RICHNESS
mr_V <- glm(t.rich~ Visibility,  family = "gaussian", data=site_dat_8)
mr<-glm(t.rich ~ 1,  family = "gaussian", data=site_dat_8)
anova(mr_V, mr, test="Chisq")

#checking for normality in coral richness
par(mfrow=c(2,2))
plot(mr_V)
shapiro.test(site_dat_8$m.rich)
hist(site_dat_8$m.rich, breaks=5)





#GLM - effect of visibility on CORAL SIZE
ms_V <- glm(m.size.2~ Visibility, family = "gaussian", data=site_dat_8)
ms<-glm(m.size.2 ~ 1, family = "gaussian", data=site_dat_8)
anova(ms_V, ms, test="Chisq")

#checking for normality in coral size
par(mfrow=c(2,2))
plot(ms_V)
shapiro.test(site_dat_8$m.size.2)
hist(site_dat_8$m.size.2, breaks=5)





#multiple testing adjustments for visibility GLMs
GLM_Vis_pvals1<-read.csv("pvalues_GLM Vis.csv")

GLM_Vis_pvals2=GLM_Vis_pvals1[order(GLM_Vis_pvals1$GLM.p.raw),]

GLM_Vis_pvals3<-GLM_Vis_pvals2 %>% 
  select(Response.variable, GLM.p.raw) %>% 
  mutate(Bonferroni=p.adjust(GLM.p.raw,
                             method = "bonferroni"),
         Holm=p.adjust(GLM.p.raw,
                       method = "holm"),
         Hochberg=p.adjust(GLM.p.raw,
                           method = "hochberg"),
         Hommel=p.adjust(GLM.p.raw,
                         method = "hommel"),
         BH_fdr=p.adjust(GLM.p.raw,
                         method = "BH"),
         BY=p.adjust(GLM.p.raw,
                     method = "BY"))









#GLMEM - EFFECT OF S. NIGRICANS

#preparing damselfish data
dat<-read.csv("PCA_data.csv") %>% 
  mutate(Site.updated=as.factor(Site.updated),
         Coral_per=as.numeric(as.character(Coral_per)),
         Damselfish.Territory=as.character(DFT.present)) %>% 
  filter(Site.old!="5")

transformations<-dat %>% 
  select(avg_size, Damselfish.Territory, Turf, Turf.Height, Coral_per, Site.updated, Visibility) %>%
  mutate(avg_size_sqrt=sqrt(avg_size),
         avg_size_log=log(avg_size),
         avg_size_inv=1/avg_size,
         Turf.Height_sqrt=sqrt(Turf.Height),
         Turf.Height_log=log(Turf.Height),
         Turf.Height_inv=1/Turf.Height)





#GLMEM on effect of S. nigricans on CORAL COVER (coral counts)
m1_C_DFT <- glmer.nb(Coral.counts ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_C<-glmer.nb(Coral.counts ~ 1 + (1|Site.updated), data=dat)
anova(m1_C_DFT, m2_C, test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_C_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput) 
plot(simulationOutput)







#GLMEM on effect of S. nigricans on CORAL RICHNESS
m1_R_DFT <- glmer.nb(Rich.ALL ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_R<-glmer.nb(Rich.ALL ~ 1 + (1|Site.updated), data=dat)
anova(m1_R_DFT,m2_R,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_R_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput) 
plot(simulationOutput)





#GLMEM on effect of S. nigricans on CORAL SIZE
m1_S_DFT<-lmer(avg_size_log ~ Damselfish.Territory + (1|Site.updated), data=transformations)
m2_S<-lmer(avg_size_log ~ 1 + (1|Site.updated), data=transformations)
anova(m1_S_DFT,m2_S,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_S_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#GLMEM on effect of S. nigricans on TURF COVER
m1_TC_DFT <- glmer.nb(Turf.counts ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_TC<-glmer.nb(Turf.counts ~ 1 + (1|Site.updated), data=dat)
anova(m1_TC_DFT,m2_TC,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_TC_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput) 
plot(simulationOutput)





#GLMEM on effect of S. nigricans on TURF HEIGHT
m1_TH_DFT<-lmer(Turf.Height_log ~ Damselfish.Territory + (1|Site.updated), data=transformations)
m2_TH<-lmer(Turf.Height_log ~ 1 + (1|Site.updated), data=transformations)
anova(m1_TH_DFT,m2_TH,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_TH_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#multiple testing adjustments for S. nigricans GLMEMs
GLMEM_pvals1<-read.csv("pvalues_GLMEM.csv")

GLMEM_pvals2=GLMEM_pvals1[order(GLMEM_pvals1$GLMEM.p.raw),]

GLMEM_pvals3<-GLMEM_pvals2 %>% 
  select(Coral.Metric, GLMEM.p.raw) %>% 
  mutate(Bonferroni=p.adjust(GLMEM.p.raw,
                             method = "bonferroni"),
         Holm=p.adjust(GLMEM.p.raw,
                       method = "holm"),
         Hochberg=p.adjust(GLMEM.p.raw,
                           method = "hochberg"),
         Hommel=p.adjust(GLMEM.p.raw,
                         method = "hommel"),
         BH_fdr=p.adjust(GLMEM.p.raw,
                         method = "BH"),
         BY=p.adjust(GLMEM.p.raw,
                     method = "BY"))











#GLMEM on effect of S. nigricans on CORAL INDIVIDUAL SPECIES
## Note - P-values were calculated only when there was sufficient data for each coral taxon (i.e., taxon present in > 15% of quadrats and present at Sites 1-4 and 6-9). 

#Cyphastrea.sp.
m1_CS_DFT <- glmer.nb(Cyphastrea.sp. ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_CS<-glmer.nb(Cyphastrea.sp. ~ 1 + (1|Site.updated), data=dat)
anova(m1_CS_DFT,m2_CS,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_CS_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#Gardineroseris.planulata
m1_GP_DFT <- glmer.nb(Gardineroseris.planulata ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_GP<-glmer.nb(Gardineroseris.planulata ~ 1 + (1|Site.updated), data=dat)
anova(m1_GP_DFT,m2_GP,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_GP_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#Leptastrea.sp.
m1_LA_DFT <- glmer.nb(Leptastrea.sp. ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_LA<-glmer.nb(Leptastrea.sp. ~ 1 + (1|Site.updated), data=dat)
anova(m1_LA_DFT,m2_LA,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_LA_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#Porites.lobata
m1_PL_DFT <- glmer.nb(Porites.lobata ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_PL<-glmer.nb(Porites.lobata ~ 1 + (1|Site.updated), data=dat)
anova(m1_PL_DFT,m2_PL,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_PL_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#Porites.rus
m1_PR_DFT <- glmer.nb(Porites.rus ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_PR<-glmer.nb(Porites.rus ~ 1 + (1|Site.updated), data=dat)
anova(m1_PR_DFT,m2_PR,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_PR_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#Psammocora.contigua (previously Psammocora obtusangula)
m1_PMc_DFT <- glmer.nb(Psammocora.contigua ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_PMc<-glmer.nb(Psammocora.contigua ~ 1 + (1|Site.updated), data=dat)
anova(m1_PMc_DFT,m2_PMc,test="Chisq")

##DHARMa diagnostics
simulationOutput <- simulateResiduals(fittedModel = m1_PMc_DFT, n = 250)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
plot(simulationOutput)





#multiple testing adjustments for S. nigricans GLMEMs on CORAL INDIVIDUAL SPECIES
GLMEM_CoralSp_pvals1<-read.csv("pvalues_GLMEM_CoralSpecies.csv")

GLMEM_CoralSp_pvals2=GLMEM_CoralSp_pvals1[order(GLMEM_CoralSp_pvals1$GLMEM.p.raw),]

GLMEM_CoralSp_pvals3<-GLMEM_CoralSp_pvals2 %>% 
  select(Coral.Species, GLMEM.p.raw) %>% 
  mutate(Bonferroni=p.adjust(GLMEM.p.raw,
                             method = "bonferroni"),
         Holm=p.adjust(GLMEM.p.raw,
                       method = "holm"),
         Hochberg=p.adjust(GLMEM.p.raw,
                           method = "hochberg"),
         Hommel=p.adjust(GLMEM.p.raw,
                         method = "hommel"),
         BH_fdr=p.adjust(GLMEM.p.raw,
                         method = "BH"),
         BY=p.adjust(GLMEM.p.raw,
                     method = "BY"))

library(tidyverse) #for data organization
library(dplyr) #for editing dataframe
library(lme4) #for running GLMEM

setwd("C:\\Users\\rassweiler\\Documents\\MBQ - clean out folder one day\\Jacobs\\Coral Reefs submission\\Code tables and figures\\R")




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
mc_S <- glm(m.counts ~ Site.old, family = "gaussian", data=BS)
mc_B <- glm(m.counts ~ Bay, family = "gaussian", data=BS)
mc<-glm(m.counts ~ 1, family = "gaussian", data=BS)

anova(mc_S, mc, test="Chisq")
anova(mc_B, mc, test="Chisq")

#checking for normality in coral cover (counts)
par(mfrow=c(2,2)) 
plot(mc_S)
plot(mc_B)
shapiro.test(BS$m.counts) #normal enough
hist(BS$m.counts, breaks=10)


#GLM - effect of bay and site on CORAL RICHNESS
mr_S <- glm(m.rich ~ Site.old, family = "gaussian", data=BS)
mr_B <- glm(m.rich ~ factor(Bay), family = "gaussian", data=BS)
mr<-glm(m.rich ~ 1, family = "gaussian", data=BS)

anova(mr_S, mr, test="Chisq")
anova(mr_B, mr, test="Chisq")

#checking for normality in coral richness
par(mfrow=c(2,2)) 
plot(mr_S)
plot(mr_B)
shapiro.test(BS$m.rich) #normal enough
hist(BS$m.rich, breaks=10)


#GLM - effect of bay and site on CORAL SIZE
ms_S <- glm(m.size.log ~ Site.old, family = "gaussian", data=BS)
ms_B <- glm(m.size.log ~ factor(Bay), family = "gaussian", data=BS)
ms<-glm(m.size.log ~ 1, family = "gaussian", data=BS)

anova(ms_S, ms, test="Chisq")
anova(ms_B, ms, test="Chisq")

#checking for normality in coral size
par(mfrow=c(2,2)) 
plot(ms_S)
plot(ms_B)
shapiro.test(BS$m.size.log) #log-transofrmed to get closer to normality
hist(BS$m.size.log, breaks=10)


#GLM - effect of bay and site on visibility (all 10 sites)
mv_S <- glm(vis ~ Site.old, family = "gaussian", data=BS)
mv_B <- glm(vis ~ factor(Bay), family = "gaussian", data=BS)
mv<-glm(vis ~ 1, family = "gaussian", data=BS)

anova(mv_S, mv, test="Chisq")
anova(mv_B, mv, test="Chisq")

#checking for normality in visibility (10 sites)
par(mfrow=c(2,2))
plot(mv_S)
plot(mv_B)
shapiro.test(BS$vis) #normal enough
hist(BS$vis, breaks=5)


#GLM - effect of bay and site on visibility (excluding sites 5 and 10)
BS_8<-BS %>% 
  filter(Site.old!= "5")

mv8_S <- glm(vis ~ Site.old, family = "gaussian", data=BS_8)
mv8_B <- glm(vis ~ factor(Bay), family = "gaussian", data=BS_8)
mv8<-glm(vis ~ 1, family = "gaussian", data=BS_8)

anova(mv8_S, mv8, test="Chisq")
anova(mv8_B, mv8, test="Chisq")

#checking for normality in visibility (8 sites)
par(mfrow=c(2,2))
plot(mv8_S)
plot(mv8_B)
shapiro.test(BS_8$vis) #normal enough
hist(BS_8$vis, breaks=10)


#multiple testing adjustments - Site
GLM_S_pvals1<-read.csv("pvalues_GLM Site.csv")

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


#multiple testing adjustments - Bay
GLM_B_pvals1<-read.csv("pvalues_GLM Bay.csv")

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

par(mfrow=c(2,2))
plot(mr_V)
shapiro.test(site_dat_8$m.rich)
hist(site_dat_8$m.rich, breaks=5)


#GLM - effect of visibility on CORAL SIZE
ms_V <- glm(m.size.2~ Visibility, family = "gaussian", data=site_dat_8)
ms<-glm(m.size.2 ~ 1, family = "gaussian", data=site_dat_8)

anova(ms_V, ms, test="Chisq")

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
hist(dat$Coral.counts, breaks=25) #poisson distributed

m1_C_DFT <- glmer.nb(Coral.counts ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_C<-glmer.nb(Coral.counts ~ 1 + (1|Site.updated), data=dat)

anova(m1_C_DFT,m2_C,test="Chisq")


#GLMEM on effect of S. nigricans on CORAL RICHNESS
hist(dat$Rich.ALL, breaks=5) #poisson distributed

m1_R_DFT <- glmer.nb(Rich.ALL ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_R<-glmer.nb(Rich.ALL ~ 1 + (1|Site.updated), data=dat)

anova(m1_R_DFT,m2_R,test="Chisq")


#GLMEM on effect of S. nigricans on CORAL SIZE
hist(transformations$avg_size_log, breaks=5)
shapiro.test(transformations$avg_size_log) #log transform for normal distribution

par(mfrow=c(2,2))
m_norm_log<-lm(avg_size_log ~ Damselfish.Territory, data = transformations)
plot(m_norm_log)

m1_S_DFT<-lmer(avg_size_log ~ Damselfish.Territory + (1|Site.updated), data=transformations)
m2_S<-lmer(avg_size_log ~ 1 + (1|Site.updated), data=transformations)

anova(m1_S_DFT,m2_S,test="Chisq")


#GLMEM on effect of S. nigricans on TURF COVER
hist(dat$Turf.counts, breaks=5) #slightly poisson
shapiro.test(dat$Turf.counts) #not normal

m1_TC_DFT <- glmer.nb(Turf.counts ~ Damselfish.Territory + (1|Site.updated), data=dat)
m2_TC<-glmer.nb(Turf.counts ~ 1 + (1|Site.updated), data=dat)

anova(m1_TC_DFT,m2_TC,test="Chisq")


#GLMEM on effect of S. nigricans on TURF HEIGHT
hist(transformations$Turf.Height_log, breaks=25) 
shapiro.test(transformations$Turf.Height_log)

m1_TH_DFT<-lmer(Turf.Height_log ~ Damselfish.Territory + (1|Site.updated), data=transformations)
m2_TH<-lmer(Turf.Height_log ~ 1 + (1|Site.updated), data=transformations)

anova(m1_TH_DFT,m2_TH,test="Chisq")

par(mfrow=c(2,2))
m_norm_log<-lm(Turf.Height_log ~ Damselfish.Territory, data = transformations)
plot(m_norm_log)


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



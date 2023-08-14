#Loading data in to R
Pots1<-read.csv("Pots1.csv", fileEncoding="UTF-8-BOM")
View(Pots1)

#Loading libraries
library(lme4)
library(nlme)
library(lmerTest)
library(doBy)
library(ggplot2)
library(plotrix)
library(car)
library(afex)
library(onewaytests)
library(multcomp)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)

#Check for missing values in a specific field
missing <- colSums(is.na(Pots1[,]))
print(missing)

#Change columns in a dataframe to factors/categorical values, useful for treatments and soils, str displays 
#the structure of R objects or the contents of a list
#treatment order specifies the porder in which treatments will appear 
Trt_order <- c("Control1", "Control2", "CanolaMeal_50kg_ha", "CanolaHull_50kg_ha", "Manure_50kg_ha", "Willow_50kg_ha",
               "CanolaMeal_10t_ha", "CanolaHull_10t_ha", "Manure_10t_ha", "Willow_10t_ha",
               "CanolaHull_10t_ha_TSP", "CanolaMeal_10t_ha_TSP", "Manure_10t_ha_TSP", "Willow_10t_ha_TSP", 
               "Triple_Super_Phosphate")
Soil_order <- c("Haverhill", "Oxbow")
Pots1$Treatment <- factor(Pots1$Treatment, levels = Trt_order)
Pots1$Soil <- factor(Pots1$Soil, levels = Soil_order)
summary(Pots1)
str(Pots1)
View(Pots1)

# Summary data (means, SE, etc.) for each treatment and variable
VarMean <- summary_by(.~Soil+Treatment, data=Pots1, FUN=mean)
VarSD <- summary_by(.~Soil+Treatment, data=Pots1, FUN=sd)

#Modelling data using simple linear model for CRD, no random intercept possible for my project & mixed model thus not possible
#look at glmr - equivalent of GLMMIX
#ANOVA model - this describes the response variable is influenced by the explanatory variables

####P-VALUE INTERPRETATION: p >0.05, no significant difference, fail to reject null hypothesis
#p-value for intercept <0.05

#Mod1 - Dry Weight
Mod1<-lm(DryWt~Treatment+Soil,data=Pots1) #using a linear model as the best fit
anova(Mod1) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod1)
#the residuals plot, qqnorm and shapiro.test all checks normality
shapiro.test(resid(Mod1))
#levene test to check for significant differences between varians (i.e. whether vriances are equal or not)
leveneTest(Drywt~Treatment, data = Pots1)

##S-W p value = 0.4382; Data is considered normal

#Two-way anova for Dry weight
Mod1a <- aov(Drywt~Treatment*Soil, data=Pots1)
anova(Mod1a)
summary(Mod1a)
shapiro.test(resid(Mod1a))
leveneTest(Drywt ~ Treatment, data = Pots1)

#The two-way anova appears to be a better fit for the dry weight data


# Mod2 - Total N - after numerous transformations and setting the distribution to gamma (instead of Gaussian),
# this data will not be analysed. It will be used to determine N uptake and recovery, which will be analysed separately
Mod2a <- lm(TotalN~Soil+Treatment, data=Pots1)
anova(Mod2a)
summary(Mod2a)
shapiro.test(resid(Mod2a))

Mod2<-lmer(TotalN~Treatment+(1|Soil),data=Pots1)
anova(Mod2)
summary(Mod2)
shapiro.test(resid(Mod2))
leveneTest(TotalN ~ Treatment, data = Pots1)

Mod2a <- aov(TotalN~Treatment*Soil, data=Pots1)
anova(Mod2a)
summary(Mod2a)
shapiro.test(resid(Mod2a))
leveneTest(TotalN ~ Treatment, data = Pots1)

##S-W p value <0.001; Data is considered non-normal and needs to be transformed
# Log-transforming Total N and checking normality of transformed data
ModNlog<-lmer(log(TotalN)~Treatment+(1|Soil),data=Pots1)
anova(ModNlog)
shapiro.test(resid(ModNlog))
leveneTest(log(TotalN) ~ Treatment, data = Pots1)

##S-W shows no sig dif, but Levene Test shows sig dif between variances for log transform
# Sqrt transformation
ModNsqrt <- lmer(sqrt(TotalN)~Treatment+(1|Soil),data=Pots1)
anova(ModNsqrt)
summary(ModNsqrt)
shapiro.test(resid(ModNsqrt))
leveneTest(sqrt(TotalN)~Treatment,data=Pots1)

##S-W shows no sig dif, but Levene Test shows sig dif between variances for sqrt transform
#using "gamma" distribution in glmer test
Mod2gl <- glmer(TotalN~Treatment+(1|Soil),data=Pots1,family=Gamma(link="log"))
anova(Mod2gl)
summary(Mod2gl)
shapiro.test(resid(Mod2gl))
bf.test(TotalN~Treatment, data=Pots1)


#Mod3 - N uptake - using original N data; not done for soil & crops - do for thesis
Mod3 <- lm(Nuptake~Soil+Treatment, data=Pots1)
anova(Mod3)
summary(Mod3)
shapiro.test(resid(Mod3))

##S-W p value <0.01; Data is considered non-normal and needs to be transformed
# Mod3a - log transform



#Mod4 - N recovery
###Nrecovery has missing values for Control1 - the subset removes these values
Nrec <- Pots1[complete.cases(Pots1$Nrecovery),] #set up a subset removing the missing data only from column Nrecovery
Mod4 <- lm(Nrecovery~Soil+Treatment, data=Nrec)
anova(Mod4)
summary(Mod4)
shapiro.test(resid(Mod4))

##S-W p value <0.005; Data is considered non-normal and needs to be transformed

Mod4a<-lmer(Nrecovery~Treatment+(1|Soil),data=Pots1)
anova(Mod4a)
summary(Mod4a)
shapiro.test(resid(Mod4a))
leveneTest(Nrecovery ~ Treatment, data = Pots1)

##S-W p value <0.005; Data is considered non-normal and needs to be transformed

#Mod4b - log and base log 10 transform data for N recovery
ModNreclog<-lmer(log(Nrecovery)~Treatment+(1|Soil),data=Pots1)
anova(ModNreclog)
shapiro.test(resid(ModNreclog))
leveneTest(log(Nrecovery) ~ Treatment, data = Pots1)

ModNreclog10<-lmer(log10(Nrecovery)~Treatment+(1|Soil),data=Pots1)
anova(ModNreclog10)
shapiro.test(resid(ModNreclog10))
leveneTest(log10(Nrecovery) ~ Treatment, data = Pots1)

##S-W p value <0.005; Data is considered non-normal with non-equal variance and needs to be transformed

#Mod4c - sqrt transformation
ModNrecsqrt <- lmer(sqrt(Nrecovery)~Treatment+(1|Soil),data=Pots1)
anova(ModNrecsqrt)
summary(ModNrecsqrt)
shapiro.test(resid(ModNrecsqrt))
leveneTest(sqrt(Nrecovery)~Treatment,data=Pots1)

#Mod4d using "gamma" distribution in glmer test
Mod4gl <- glmer(Nrecovery~Treatment+(1|Soil),data=Pots1,family=Gamma(link="log"))
anova(Mod4gl)
summary(Mod4gl)
shapiro.test(resid(Mod4gl))
bf.test(TotalN~Treatment, data=Pots1)

#issue with using the glmer model - use another model

#Mod5 - Total P1
Mod5 <- lm(TotalP1~Soil+Treatment, data=Pots1)
anova(Mod5)
summary(Mod5)
shapiro.test(resid(Mod5))


##S-W p value = 0.1874; Data is considered normal


#Mod6 - P uptake
Mod6 <- lm(Puptake~Soil+Treatment,data=Pots1)
anova(Mod6)
summary(Mod6)
shapiro.test(resid(Mod6))

##S-W p value = 0.3377; Data is considered normal


#Mod7 - P recovery
###Precovery has missing values for Control1 & Control2 - the subset removes these values
Prec <- Pots1[complete.cases(Pots1$Precovery),] #set up a subset removing the missing data only from column Precovery
Mod7 <- lm(Precovery~Soil+Treatment,data=Prec)
anova(Mod7)
summary(Mod7)
shapiro.test(resid(Mod7))

##S-W p value <0.001; Data is considered non-normal and needs to be transformed
#Mod7a - log transform
Mod7a <- lm(log(Precovery)~Soil+Treatment,data=Prec)
anova(Mod7a)
summary(Mod7a)
shapiro.test(resid(Mod7a))



#Plotting models
#Mod1 - Dry Weight
plot(fitted(Mod1),resid(Mod1),pch=16) #plot to be equally centered around 0, vertical lines are ok if cenetered around 0
#plots the residuals vs the fitted values from a linear regression model, i.e. Mod1
qqnorm(resid(Mod1))
qqline(resid(Mod1))

#Mod2 - Total N
plot(fitted(Mod2a),resid(Mod2a),pch=16)
qqnorm(resid(Mod2a))
qqline(resid(Mod2a))

#Mod3 - N uptake
plot(fitted(Mod2),resid(Mod2),pch=16)
qqnorm(resid(Mod2))
qqline(resid(Mod2))

#Mod4 - N recovery
plot(fitted(Mod3),resid(Mod3),pch=16)
qqnorm(resid(Mod3))
qqline(resid(Mod3))

#Mod5 - Total P
plot(fitted(Mod4a),resid(Mod4a),pch=16)
qqnorm(resid(Mod4a))
qqline(resid(Mod4a))

#Mod6 - P uptake
plot(fitted(Mod4),resid(Mod4),pch=16)
qqnorm(resid(Mod4))
qqline(resid(Mod4))

#Mod7 - P recovery
plot(fitted(Mod5),resid(Mod5),pch=16) 
qqnorm(resid(Mod5))
qqline(resid(Mod5))



##Tukey HSD tests to detremine significant differences between treatments
# Dry weight
TukeyHSD(Mod1a)
plot(TukeyHSD(Mod1a))

# N recovery


# P recovery



##Developing visualizations
#set up data frame with the means and SE of Dry weight
Pots1 <- read_csv("Pots1.csv")
Drywt_df <- Pots1 %>%
  select(Soil, Treatment, Drywt) %>% # select only relevant columns
  group_by(Soil, Treatment) %>% # group by soil and treatment
  summarise(Mean = mean(Drywt, na.rm=TRUE), # calculate the mean dry weight for each group
            SE = sd(Drywt, na.rm=TRUE)/sqrt(length(Drywt)), # calculate the standard error for each group
            .groups = 'drop')
levels(Drywt_df$Treatment)
View(Drywt_df)
write.csv(Drywt_df,"Drywt.csv")

# Plot means with error bars
#c(bottom, left, top, right) + 0.1 lines
par(mar=c(5,6,4,2)+0.1)
#plot all dry weights onto one graph
ggplot(Drywt_df, aes(x = Treatment, y = Mean, fill = Soil)) +
  geom_bar(stat = "identity", position=position_dodge2(padding=0.2)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = .2, position = position_dodge(width = 0.9)) +
  labs(title = "Dry Weight by Treatment and Soil", x = "Treatment", y = "Dry Weight (ug/g)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_manual(values = c("grey", "black"), 
                    labels = c("Haverhill","Oxbow"))
##select specific treatments and use those in the graph - repeat for all subsets
#Plotting dry weight using constant P and variable biochar rates
Drywt_trtVar <- c("Control1", "Control2","CanolaHull_50kg_ha","CanolaMeal_50kg_ha","Manure_50kg_ha","Willow_50kg_ha",
                  "Triple_Super_Phosphate")
Drywt_subVar <- Drywt_df %>%
  filter(Treatment %in% Drywt_trtVar)
ggplot(Drywt_subVar, aes(x = Treatment, y = Mean, fill = Soil)) +
  geom_bar(stat = "identity", position = position_dodge2(padding=0.2)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Dry Weight by Soil and Treatment", x = "Soil", y = "Dry Weight") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("grey", "black"), 
                    labels = c("Haverhill","Oxbow"))
#Plotting constant biochar rates with var P rates
Drywt_charCon <- c("Control1", "Control2","CanolaHull_10t_ha", "CanolaHull_10t_ha_TSP", "CanolaMeal_10t_ha", 
                   "CanolaMeal_10t_ha_TSP", "Manure_10t_ha", "Manure_10t_ha_TSP","Triple_Super_Phosphate", 
                   "Willow_10t_ha", "Willow_10t_ha_TSP")
Drywt_subCon <- Drywt_df %>%
  filter(Treatment %in% Drywt_charCon)
ggplot(Drywt_subCon, aes(x = Treatment, y = Mean, fill = Soil)) +
  geom_bar(stat = "identity", position = position_dodge2(padding=0.2)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Dry Weight by Soil and Treatment", x = "Soil", y = "Dry Weight") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("grey", "black"), 
                    labels = c("Haverhill","Oxbow"))






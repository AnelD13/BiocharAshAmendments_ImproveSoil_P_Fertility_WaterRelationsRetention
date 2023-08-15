#### Loading data in to R ####
Pots1<-read.csv("Pots1.csv", fileEncoding="UTF-8-BOM")
View(Pots1)
plot(Pots1$Yield)
PotsRaw<-read.csv("Pots1raw.csv", fileEncoding="UTF-8-BOM")

#Loading libraries
library(lme4)
library(nlme)
library(glmm)
library(glmmTMB)
library(lmerTest)
library(doBy)
library(ggplot2)
library(ggpattern)
library(ggforce)
library(ggrepel)
library(plotrix)
library(car)
library(afex)
library(onewaytests)
library(multcomp)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(broom)
library(multcomp)
library(multcompView)
library(emmeans)
library(lsmeans)
library(e1071)
library(rsq)
library(pgirmess)
library(car)
library(robustbase)
library(openxlsx)
library(ggcorrplot)
library(pheatmap)
library(MASS)
library(RColorBrewer)
library(reshape2)
library(metaSEM)
library(ggh4x)
library(gridExtra)
library(sjstats)
library(corrr)
library(FactoMineR)
library(factoextra)
library(reshape2)
library(writexl)
library(glmmTMB)


#### Summary and ordering of data   ####
#Check for missing values in a specific field
missing <- colSums(is.na(Pots1[,]))
print(missing)

#Change columns in a dataframe to factors/categorical values, useful for treatments and soils, str displays 
#the structure of R objects or the contents of a list
#treatment order specifies the order in which treatments will appear 
Pots1$Treatment <- factor(Pots1$Treatment, levels=c("Control1", "Control2", "CanolaMeal50kgha", "CanolaHull50kgha", 
                          "Manure50kgha", "Willow50kgha", "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", 
                          "Willow10tha", "CanolaMeal10thaTSP", "CanolaHull10thaTSP", "Manure10thaTSP", 
                          "Willow10thaTSP", "TripleSuperPhosphate"))
Pots1$Soil <- factor(Pots1$Soil, levels=c("Haverhill", "Oxbow"))
summary(Pots1)
str(Pots1) #displays the structure of the object
View(Pots1) #view the object in a separate window (e.g. as a table)

# Summary data (means, SD, etc.) for each treatment and variable
Pots1Mean <- summary_by(.~Soil+Treatment, data=Pots1, FUN=mean, na.rm=TRUE)
Pots1Mean <- rename(Pots1Mean, Yield=Yield.mean, Nuptake=Nuptake.mean, Nrecovery=Nrecovery.mean, Puptake=Puptake.mean, 
                    Precovery=Precovery.mean, NO3=NO3.mean, NH4=NH4.mean, PO4=PO4.mean, ResinP=ResinP.mean, 
                    WaterSolP=WaterSolP.mean, TotalP2=TotalP2.mean, pH=pH.mean, EC=EC.mean, OC=OC.mean)
View(Pots1Mean)
Pots1SD <- summary_by(.~Soil+Treatment, data=Pots1, FUN=sd)

####   Check for outliers   ####
##Yield
ggplot(PotsRaw, aes(x=Treatment, y=Yield, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Yield")
ggsave("OutliersYield.jpg", width=30, height=30, dpi=150)
##Total N
ggplot(PotsRaw, aes(x=Treatment, y=TotalN, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Total N")
ggsave("OutliersTotalN.jpg", width=30, height=30, dpi=150)
##N Uptake
ggplot(PotsRaw, aes(x=Treatment, y=Nuptake, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="N uptake")
ggsave("OutliersNuptake.jpg", width=30, height=30, dpi=150)
##N Recovery
ggplot(PotsRaw, aes(x=Treatment, y=Nrecovery, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="N Recovery")
ggsave("OutliersNrecovery.jpg", width=30, height=30, dpi=150)
##Total P1
ggplot(PotsRaw, aes(x=Treatment, y=TotalP1, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Total P Plants")
ggsave("OutliersTotalPplants.jpg", width=30, height=30, dpi=150)
##P Uptake
ggplot(PotsRaw, aes(x=Treatment, y=Puptake, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="P uptake")
ggsave("OutliersPuptake.jpg", width=30, height=30, dpi=150)
##P Recovery
ggplot(PotsRaw, aes(x=Treatment, y=Precovery, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="P recovery")
ggsave("OutliersPrecovery.jpg", width=30, height=30, dpi=150)
##NO3	
ggplot(PotsRaw, aes(x=Treatment, y=NO3, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="NO3")
ggsave("OutliersNO3.jpg", width=30, height=30, dpi=150)
##NH4	
ggplot(PotsRaw, aes(x=Treatment, y=NH4, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="NH4")
ggsave("OutliersNH4.jpg", width=30, height=30, dpi=150)
##PO4	
ggplot(PotsRaw, aes(x=Treatment, y=PO4, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="PO4")
ggsave("OutliersPO4.jpg", width=30, height=30, dpi=150)
## Resin P
ggplot(PotsRaw, aes(x=Treatment, y=ResinP, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="ResinP")
ggsave("OutliersResinP.jpg", width=30, height=30, dpi=150)
##WaterSolP	
ggplot(PotsRaw, aes(x=Treatment, y=WaterSolP, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Water Soluble P")
ggsave("OutliersWaterSolP.jpg", width=30, height=30, dpi=150)
##TotalP2	
ggplot(PotsRaw, aes(x=Treatment, y=TotalP2, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Total P Soil")
ggsave("OutliersTotalPSoil.jpg", width=30, height=30, dpi=150)
##pH	
ggplot(PotsRaw, aes(x=Treatment, y=pH, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="pH")
ggsave("OutlierspH.jpg", width=30, height=30, dpi=150)
##EC	
ggplot(PotsRaw, aes(x=Treatment, y=EC, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Electrical Conductivity")
ggsave("OutliersEC.jpg", width=30, height=30, dpi=150)
##OC
ggplot(PotsRaw, aes(x=Treatment, y=OC, fill=Soil)) +
  geom_boxplot(na.rm=TRUE) +
  facet_wrap(~ Soil+Treatment, scales="free") +
  labs(x="Treatment", y="Organic Carbon %")
ggsave("OutliersOC.jpg", width=30, height=30, dpi=150)





#Modelling data using simple linear model for CRD, no random intercept possible for my project & mixed model thus not possible
#look at glmr and glmm - equivalent of GLMMIX
#ANOVA model - this describes the response variable is influenced by the explanatory variables
####P-VALUE INTERPRETATION: p >0.05, no significant difference, fail to reject null hypothesis
#p-value for intercept <0.05
#Plots to check residuals to be equally centered around 0, vertical lines are ok if centered around 0


#### Yield ####
Pots1Yield_Mean <- summary_by(Yield~Treatment*Soil, data=Pots1, FUN=mean)
Pots1Yield_Mean <- as.numeric(Pots1Yield_Mean$Yield)
Pots1Yield_skew <- skewness(Pots1Yield_Mean,na.rm=TRUE)
Pots1Yield_kur <- kurtosis(Pots1Yield_Mean,na.rm=TRUE)
cat("Skewness:", Pots1Yield_skew, "\n") # -0.458 
cat("Kurtosis:", Pots1Yield_kur, "\n") # -0.655 
shapiro.test(Pots1$Yield) # p=0.001166
hist(Pots1$Yield) #  slight right skew
leveneTest(Yield~Treatment*Soil, data=Pots1)  # #check for equality of variance; p=0.01407, var is equal
ggplot(Pots1, aes(x = Treatment, y = Yield)) + #checking treatments in boxplots to see if variances = across trt
  geom_boxplot() + facet_wrap(~Soil) + labs(x = "Treatment", y = "Yield") + theme_bw()
# Transform data
shapiro.test(log(Pots1$Yield)) # p=4.984e-10
shapiro.test(log10(Pots1$Yield)) # p=4.984e-10
shapiro.test(sqrt(Pots1$Yield)) # p=1.949e-06
leveneTest(log(Yield) ~ Treatment*Soil, data=Pots1)  # much improved variance: 0.002803
leveneTest(log10(Yield) ~ Treatment*Soil, data=Pots1)  #  much improved variance: 0.002803
leveneTest(sqrt(Yield) ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 0.03423
hist((log(Pots1$Yield))) # severe right skew
hist((log10(Pots1$Yield))) # severe right skew
hist((sqrt(Pots1$Yield)))# severe right skew
#Running models - tried lm & aov but these do not allow for random effects which must be included
Mod1<-lm(Yield~Treatment*Soil,data=Pots1) #using a linear model as the best fit
anova(Mod1) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod1)
shapiro.test(resid(Mod1)) ##S-W p value=0.1965; Data is considered normal
#the residuals plot, qqnorm and shapiro.test all checks normality
plot(fitted(Mod1),resid(Mod1),pch=16, abline(h=0, lty=2)) #plots the residuals vs the fitted values 
#from a linear regression model, abline shows the linear horizontal line at 0
plot(Mod1, which=1) #plots the residuals vs the actual values, the line is the regression line
plot(Mod1, which=2) # different way to do the below qqnorm & qqline
qqnorm(resid(Mod1)) # tails at either end not too hectic
qqline(resid(Mod1))
rsq(Mod1) #r=0.8945
#Mod1a #The two-way anova appears to be a better fit for the dry weight data
Mod1a <- aov(Yield~Treatment*Soil, data=Pots1) #Two-way anova for Dry weight - another way to do the same
anova(Mod1a)
summary(Mod1a)
shapiro.test(resid(Mod1a)) #0.01965
Mod1a_tidy <- tidy(Mod1a)
Mod1a_tidy$sumsq[1] / (Mod1a_tidy$sumsq[1] + Mod1a_tidy$sumsq[2]) # 0.99
# Mod1b glmm
Mod1b <- glmmTMB(Yield~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod1b, type="III")# test.statistic ="F" is not currently available
summary(Mod1b)
performance::r2(Mod1b) # 0.895
# Mod1c Lmer
Mod1c<-lmer(Yield~Treatment*Soil+(1|Soil),data=Pots1) #model failed to converge
anova(Mod1c)
summary(Mod1c)
rsq(Mod1c) # adjusted R squared: 0.894
#Mod1d - lme
Mod1d<-lme(Yield~Treatment*Soil,random=~1|Soil, data=Pots1, na.action = na.exclude)
anova(Mod1d)
summary(Mod1d)
rsq(Mod1d) # issue with low degrees of freedom for random effect
#Mod1e - glmer
Mod1e <- glmer(Yield~Treatment*Soil+(1|Soil),data=Pots1,family=gaussian(link="log"))
anova(Mod1e)
summary(Mod1e)
shapiro.test(resid(Mod1e))  # p=6.486e-09
plot(fitted(Mod1e),resid(Mod1e),pch=16) # severe right cluster
qqnorm(resid(Mod1e)) # heavy tails
qqline(resid(Mod1e))
rsq(Mod1e)

#Compare models - doesn't work for certain types of models
anova(Mod1, Mod1a, Mod1b, Mod1c)

#extract the anova results in a tidy format
View(Mod1_tidy <- tidy(Mod1))
View(Mod1_tidy <- tidy(Mod1a))
View(Mod1_tidy <- tidy(Mod1b))  #No tidy method for objects of class glmmTMB
View(Mod1_tidy <- tidy(Mod1c))  # same for class lmerModLmerTest
View(Mod1_tidy <- tidy(Mod1d))  # same for class lme

#Run emmeans (the new lsmeans) - it creates a Tukey HSD pairwise comparison
#Mod1em <- emmeans(Mod1a,~Soil+Treatment) #a combined table by soil, but comparing all treatments per soil in one
#View(Mod1cld <- cld(Mod1em, Letters=trimws(letters))) #use Compact Letter Display (CLD) with a means test to show sig dif as letters

#emmeans for each soil separately - # cld use directly in ggplot - make sure labels are correct in ggplot
Mod1em_split <- emmeans(Mod1b,~Treatment|Soil, subset=(Pots1$Yield))
#MUST use trimws to trim the white spaces around the cld letters to ensure proper alignment
Mod1cld_split <- cld(Mod1em_split, Letters=trimws(letters), reversed=TRUE, by="Soil") #reversed = letters in correct order
View(Mod1cld_split)

##Developing visualizations
##select specific treatments and use those in the graph - repeat for all subsets
#Plotting dry weight using constant P and variable biochar rates
Yield_trtVar <- c("Control1", "Control2","CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha",
                  "TripleSuperPhosphate")
Yield_subVar <- Mod1cld_split %>%
  filter(Treatment %in% Yield_trtVar)
(Yield50kg <- ggplot(Yield_subVar, aes(x=Treatment, y=emmean, pattern=Soil))+
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  #geom_text(aes(label=.group, y=emmean+SE, fontface=ifelse(Soil == "Haverhill", "italic", "plain")),
           # size=6, position=position_dodge2(width=0.9), vjust=-0.5) + # change Oxbow to italics.
    #trim white spaces in Geom_text to align properly
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), 
                y = emmean + SE + 0.5), size = 6, position = position_dodge(width = 0.9))+
  labs(y="Biomass yield (g) for chars at 50kg P/ha")+
  scale_x_discrete(labels=c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                            "Phosphorus\nFertilizer"))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"), #hjust right aligns text
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Yield50kg, file="Pots1_Yield_50kgPha.jpg", width=12, height=8, dpi=150)
#Plotting constant biochar rates with var P rates
Yield_charCon <- c("Control1", "Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                   "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                   "Willow10tha", "Willow10thaTSP")
Yield_subCon <- Mod1cld_split %>%
  filter(Treatment %in% Yield_charCon)
(Yield10tha <- ggplot(Yield_subCon, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y = emmean + SE + 0.5),
              size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="Biomass yield (g) for chars at 10t/ha") +
  scale_x_discrete(labels=c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                            "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP", 
                            "Phosphorus\nFertilizer"))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Yield10tha, file="Pots1_Yield_10tha.jpg", width=12, height=8, dpi=150)



#### Total N (Mod2) - only used to calculate N uptake and recovery, don't analyse


#### N uptake ####
#check kurtosis and skewness - the data is not too moderate to highly skewed with moderate/high kurtosis
Nup_Mean <- summary_by(Nuptake~Soil+Treatment, data=Pots1, FUN=mean) # calculate means of N recovery
Nup_Mean <- as.numeric(Nup_Mean$Nuptake)
Nup_skew <- skewness(Nup_Mean,na.rm=TRUE)
Nup_kur <- kurtosis(Nup_Mean,na.rm=TRUE)
cat("Skewness:", Nup_skew, "\n") ## data is moderately skewed @ -1.31057
cat("Kurtosis:", Nup_kur, "\n") ## data has very high kurtosis @ 3.36125
#check distribution normality
shapiro.test(Pots1$Nuptake) #  p=3.599e-07
hist(Pots1$Nuptake) # slight right  skew
leveneTest(Nuptake ~ Treatment, data=Pots1)  # 0.0001608
qqnorm(Pots1$Nuptake) # very heavy left tail
qqline(Pots1$Nuptake)
# transform
shapiro.test(log(Pots1$Nuptake)) #p=2.008e-15
hist(log(Pots1$Nuptake))  # heavy right skew
leveneTest(log(Nuptake) ~ Treatment, data=Pots1) # p=0.0005804
shapiro.test(sqrt(Pots1$Nuptake)) #p=1.471e-11 
hist(sqrt(Pots1$Nuptake)) # heavy right skew
leveneTest(sqrt(Nuptake) ~ Treatment, data=Pots1) # p=0.0006209
# transformations did not improve normality or variance
# Mod3 - anova
Mod3 <- aov(Nuptake~Treatment*Soil, data=Pots1)
anova(Mod3)
summary(Mod3)
shapiro.test(resid(Mod3)) # p=0.0008813
hist(resid(Mod3)) # heavily flattened tails
qqnorm(resid(Mod3)) # heavy tails
qqline(resid(Mod3))
Mod3_tidy <- tidy(Mod3)
View(Mod3_tidy) #tidu output of the summary stas
Mod3sum_sq_reg <- Mod3_tidy$sumsq[1] #use summary stats to determine the sum squares regression
Mod3sum_sq_resid <- Mod3_tidy$sumsq[2]  # use the summary stats to determine the sum squares residuals
Mod3sum_sq_reg / (Mod3sum_sq_reg + Mod3sum_sq_resid) # 0.866
#Mode3a1
Mod3a1 <- lm(Nuptake~Treatment*Soil, data=Pots1)
rsq(Mod3a1) # r sq=0.750
summary(Mod3a1)$adj.r.squared # check adjusted R squared value: 0.639
summary(Mod3a1)
shapiro.test(resid(Mod3a1)) ##S-W p value 0.0007889; Data is considered non-normal and needs to be transformed
plot(fitted(Mod3a1),resid(Mod3a1),pch=16) # not normally distributed
qqnorm(resid(Mod3a1)) #not normally distributed
qqline(resid(Mod3a1)) 
#Mod3a - N uptake (outliers removed)
Mod3a <- lmer(Nuptake~Treatment*Soil+(1|Soil), data=Pots1, na.action=na.exclude,
              control=lmerControl(optCtrl=list(maxfun=1000000)))
rsq(Mod3a) # adjusted R squared: 0.762
anova(Mod3a)
summary(Mod3a)
shapiro.test(resid(Mod3a)) ##S-W p value 0.00544; Data is considered non-normal and needs to be transformed
plot(fitted(Mod3a),resid(Mod3a),pch=16) # cluster at right end is less severely compacted than for other models
qqnorm(resid(Mod3a)) #not normally distributed
qqline(resid(Mod3a)) 
# Log-transforming Total N and checking normality of transformed data
Mod3b<-lmer(log(Nuptake)~Treatment*Soil+(1|Soil), data=Pots1, na.action=na.exclude, 
            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
anova(Mod3b)
shapiro.test(resid(Mod3b)) # p=2.081e-08
plot(fitted(Mod3b),resid(Mod3b),pch=16) # clustered at the right end
qqnorm(resid(Mod3b)) # heavy tails
qqline(resid(Mod3b))
rsq(Mod3b) # 0.842
# Sqrt transformation
Mod3c<- lmer(sqrt(Nuptake)~Treatment*Soil+(1|Soil),data=Pots1, na.action=na.exclude)
rsq(Mod3c) # 0.8086
vif(Mod3c)
anova(Mod3c)
summary(Mod3c)
shapiro.test(resid(Mod3c)) # s-w p=5.286e-05, Sqrt transformation worsened normality
leveneTest(sqrt(Nuptake)~Treatment,data=Pots1) # Variances improved but still unequal at p=0.0007083
plot(fitted(Mod3c),resid(Mod3c),pch=16)
plot(Mod3c)
qqnorm(resid(Mod3c))
qqline(resid(Mod3c))
# Mod3d glm
Mod3d <- glm(Nuptake ~ Treatment*Soil, family=gaussian, data=Pots1)
rsq(Mod3d) #0.750
anova(Mod3d)
summary(Mod3d)
shapiro.test(resid(Mod3d))  # p=0.000881
plot(fitted(Mod3d),resid(Mod3d),pch=16) # cluster at right end is less severely compacted than for other models
qqnorm(resid(Mod3d))  #medium-heavy tails
qqline(resid(Mod3d))
#using "gamma" distribution in glmer test
Mod3e <- glmer(Nuptake~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log"))
#rsq(Mod3e)
anova(Mod3e)
summary(Mod3e)
shapiro.test(resid(Mod3e))  # p=6.486e-09
plot(fitted(Mod3e),resid(Mod3e),pch=16) # severe right cluster
qqnorm(resid(Mod3e)) # heavy tails
qqline(resid(Mod3e))
# Kruskal-Wallis test of medians
Mod3f <- kruskalmc(Nuptake~Treatment*Soil, data=Pots1)
# Transformation
Nuptake_YJ <- yjPower(Pots1$Nuptake, 0.5,jacobian.adjusted=TRUE)
Mod3g <- lmer(Nuptake_YJ ~ Treatment*Soil + (1|Soil), data=Pots1, control=lmerControl(optCtrl=list(maxfun=100000)))
rsq(Mod3g) # 0.807
anova(Mod3g)
summary(Mod3g)
leveneTest(Nuptake_YJ~Treatment*Soil, data=Pots1)  # 3.842e-09
shapiro.test(resid(Mod3g)) # p=4.088e-05
plot(fitted(Mod3g),resid(Mod3g),pch=16) # heavy mdeium-loose right cluster
qqnorm(resid(Mod3g)) # medium-heavy tails
qqline(resid(Mod3g))
# weighted lm model
Mod3var <- tapply(log(Pots1$Nuptake), Pots1$Treatment, var, na.rm=TRUE)
weightsP1Nuptake <- 1 / Mod3var
weightsP1Nuptake_full <- rep(weightsP1Nuptake, each=length(Pots1$Nuptake) / length(weightsP1Nuptake))
Mod3h <- lm(Nuptake ~ Treatment*Soil, data=Pots1, weights=weightsP1Nuptake_full) 
anova(Mod3h)
summary(Mod3h)
hist(resid(Mod3h)) # flattened on edges
shapiro.test(resid(Mod3h))  # 0.0008813
plot(fitted(Mod3h),resid(Mod3h),pch=16)   # clusters forming
qqnorm(resid(Mod3h)) # moderate tails
qqline(resid(Mod3h))
rsq(Mod3h) # 0.643
# glmm model - singularity issue
Mod3i <- glmmTMB(Nuptake~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod3i, type="III")
summary(Mod3i)
shapiro.test(resid(Mod3i)) # p=0.0008813
plot(fitted(Mod3i),resid(Mod3i),pch=16) # loose cluster at the right end
qqnorm(resid(Mod3i)) # moderate tails
qqline(resid(Mod3i))
performance::r2(Mod3i) # NA

#Comparing models using various options:
## AIC & BIC indicate that Mod3b is the best fit.
# Create a list of the models
N_modlist <- list(Mod3a, Mod3b, Mod3c, Mod3e, Mod3g, Mod3h, Mod3i)
AIC_values <- sapply(N_modlist, AIC)
BIC_values <- sapply(N_modlist, BIC)
N_AB <- data.frame(Model=c("Mod3a", "Mod3b", "Mod3c", "Mod3e", "Mod3g", "Mod3h", "Mod3i"),
                   AIC_values, BIC_values)
print(N_AB) # Other mods not appropriate for the data
#1 Mod3a   760.3687   848.4836
#2 Mod3b   119.6651   207.7800
#3 Mod3c   307.3099   395.4247
#4 Mod3e   971.5901  1059.7050
#5 Mod3g   768.5644   856.6793
#6 Mod3h   968.3044  1053.6657
#7 Mod3i   914.0315  1002.1463

# R squared values for Mod3a (lmer) =0.762; Mod3b (lmerlog)=0.841, Mod3c (lmersqrt)=0.8086; 
# Mod3g (glmr)=0.807, Mod3h (weightedlm)=0.672, Mod3i (glmm)=0.752

#Mod3b (lmer(log)) chosen as best fit
#emmeans 
Mod3em <- emmeans(Mod3i,~Treatment|Soil, data=Pots1)
Mod3cld <- cld(Mod3em, Letters=trimws(letters), reversed = TRUE, by="Soil", type="response") 
#Mod3cld <- Mod3cld %>% rename(emmean="response")
View(Mod3cld)

## Visualizations
Nuptake_trtVar <- c("Control1", "Control2","CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha",
                  "TripleSuperPhosphate")
Nuptake_subVar <- Mod3cld %>%
  filter(Treatment %in% Nuptake_trtVar)
(Nup50kg <- ggplot(Nuptake_subVar, aes(x=Treatment, y=emmean, pattern=Soil))+
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+2),
              size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="N uptake (ug/kg soil) for chars at 50kg p/ha")+
  scale_x_discrete(labels=c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Phosphorus\nFertilizer"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Nup50kg, file="Pots1_Nuptake_50kgPha.jpg", width=12, height=8, dpi=150)
#Plotting constant biochar rates with var P rates
Nuptake_charCon <- c("Control1", "Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                   "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                   "Willow10tha", "Willow10thaTSP")
Nuptake_subCon <- Mod3cld %>%
  filter(Treatment %in% Nuptake_charCon)
(Nup10tha <- ggplot(Nuptake_subCon, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+2),
              size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="N uptake (ug/kg soil) for chars at 10t/ha") +
  scale_x_discrete(labels=c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                            "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP",
                            "Phosphorus\nFertilizer"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Nup10tha, file="Pots1_Nuptake_10tha.jpg", width=12, height=8, dpi=150)



#### N recovery ####
Nrec_Mean <- summary_by(Nrecovery~Soil+Treatment, data=Pots1, FUN=mean) # calculate means of N recovery
Nrec_Mean <- as.numeric(Nrec_Mean$Nrecovery)
Nrec_skew <- skewness(Nrec_Mean,na.rm=TRUE)
Nrec_kur <- kurtosis(Nrec_Mean,na.rm=TRUE)
cat("Skewness:", Nrec_skew, "\n") ## data is mildly skewed @ 0.797
cat("Kurtosis:", Nrec_kur, "\n") ## data has very low kurtosis @ -0.231
hist(Pots1$Nrecovery)
leveneTest(Nrecovery ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 6.39e-12
### Nrecovery has missing values for Control1 - the subset removes these values for the glmer model
Nrec_out <- Pots1[complete.cases(Pots1$Nrecovery),] #set up a subset removing the missing data only from column Nrecovery
View(Nrec_out)
leveneTest(Nrecovery ~ Treatment*Soil, data=Nrec_out)  # 6.39e-12; results are the same as for the whole dataset  
# Mod4 ANOVA model - did not work ##S-W p value <0.005; Data is considered non-normal and needs to be transformed
Mod4 <- aov(Nrecovery~Treatment*Soil, data=Pots1)
anova(Mod4)
summary(Mod4)
shapiro.test(resid(Mod4)) #2.046e-06
plot(fitted(Mod4),resid(Mod4),pch=16) #looks relatively normal
qqnorm(resid(Mod4)) #long tails
qqline(resid(Mod4))
Mod4_tidy <- tidy(Mod4)
Mod4_tidy
View(Mod4_tidy) #tidu output of the summary stas
Mod4sum_sq_reg <- Mod4_tidy$sumsq[1] #use summary stats to determine the sum squares regression
Mod4sum_sq_resid <- Mod4_tidy$sumsq[2]  # use the summary stats to determine the sum squares residuals
Mod4sum_sq_reg / (Mod4sum_sq_reg + Mod4sum_sq_resid) #calculate the R squared value: 0.753
#trying out log transformation in aov - S-W result is much worse than for normal aov
Mod4_aovlog <- aov(log(Nrecovery)~Treatment*Soil, data=Pots1)
shapiro.test(resid(Mod4_aovlog)) # highly uneuqla variance p=1.297e-14
leveneTest(log(Nrecovery) ~ Treatment*Soil, data=Nrec_out) #log transformation improved variance: p=1.507e-06
leveneTest(log10(Nrecovery) ~ Treatment*Soil, data=Nrec_out) # variances improved compared to raw data p=1.507e-06
leveneTest(sqrt(Nrecovery) ~ Treatment*Soil, data=Nrec_out) # not much improvement on variances p=8.751e-12
summary((Mod4_aovlog))
# linear model - did not work ##S-W p value <0.005; Data is considered non-normal and needs to be transformed
Mod4a <- lm(Nrecovery~Soil*Treatment, data=Nrec)
summary(Mod4a)$adj.r.squared # check adjusted R squared value: 0.682
anova(Mod4a)
summary(Mod4a)
shapiro.test(resid(Mod4a)) # p=2.046e-06
plot(fitted(Mod4a),resid(Mod4a),pch=16) #looks relatively normal
qqnorm(resid(Mod4a)) #long tails
qqline(resid(Mod4a))
# lmer model - did not work  ##S-W p value <0.005; Data is considered non-normal and needs to be transformed
Mod4b<-lmer(Nrecovery~Treatment*Soil+(1|Soil),data=Pots1) #model fitting process didn't reach stable solution
# estimated parameter values may not be reliable
anova(Mod4b)
summary(Mod4b)
shapiro.test(resid(Mod4b))  # p=2.046e-06
plot(fitted(Mod4b),resid(Mod4b),pch=16) #looks relatively normal
qqnorm(resid(Mod4b)) #long tails
qqline(resid(Mod4b))
#Mod4c - log and base log 10 transform data for N recovery
Mod4c<-lmer(log(Nrecovery)~Treatment*Soil+(1|Soil),data=Pots1) # model didn't converge properly
anova(Mod4c)
summary(Mod4c)
shapiro.test(resid(Mod4c))  # p=1.297e-14
plot(fitted(Mod4c),resid(Mod4c),pch=16)  # pretty normal slight upward shift
qqnorm(resid(Mod4c)) #most data fit better but tails are exaggerated
qqline(resid(Mod4c))
rsq(Mod4c) # adjusted R squared: 0.631
#Mod4d 
Mod4d<-lmer(log10(Nrecovery)~Treatment*Soil+(1|Soil),data=Pots1)
anova(Mod4d)
summary(Mod4d)
shapiro.test(resid(Mod4d)) # p= 1.297e-14
plot(fitted(Mod4d),resid(Mod4d),pch=16)  # pretty normal but shifted up
qqnorm(resid(Mod4d)) #heavy tails & shifted up
qqline(resid(Mod4d))
ref.grid(Mod4d)
Mod4doptim <- lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=1e9))
Mod4dOp <- update(Mod4d, control=Mod4doptim)
rsq(Mod4d) #0.59
#Mod4e - sqrt transformation - did not work, data non-normally distributed and has unequal variances
Mod4e <- lmer(sqrt(Nrecovery)~Treatment*Soil+(1|Soil),data=Pots1) # model didn't converge properly
anova(Mod4e)
summary(Mod4e)
shapiro.test(resid(Mod4e)) #=8.829e-09
plot(fitted(Mod4e),resid(Mod4e),pch=16)  # pretty normal
qqnorm(resid(Mod4e)) #heavy tails
qqline(resid(Mod4e))
rsq(Mod4e)  # r=0.73
#Mod4f using "gamma" distribution in glmer test - did not work, data less normally distributed with unequal variance
Mod4f <- glmer(Nrecovery~Treatment*Soil+(1|Soil),data=Nrec_out,family=Gamma(link="log"))
anova(Mod4f)
summary(Mod4f)
shapiro.test(resid(Mod4f)) # p=2.471e-13
bf.test(Nrecovery~Treatment, data=Pots1) # variances are significantly different 
plot(fitted(Mod4f),resid(Mod4f),pch=16)  # data clustering at the top and slightly to left of the plot
qqnorm(resid(Mod4f)) #data clustering at the top of the plot, moderate tails
qqline(resid(Mod4f))
# Mod 4g glmm
Mod4g <- glmmTMB(Nrecovery~Treatment*Soil+(1|Soil), data=Nrec_out, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod4g, type="III")
summary(Mod4g)
performance::r2(Mod4g) # 0.764
shapiro.test(resid(Mod4g)) # p=2.046e-06
plot(fitted(Mod4g),resid(Mod4g),pch=16) # slight cluster to left
qqnorm(resid(Mod4g)) # moderate tails
qqline(resid(Mod4g))

##since the S-W values for all models show non-normally distributed data, look at other options:
#Shapiro-Wilk values for the various models showed Mod4 to have the most normal data
#Levene test showed Mod4c&d to have the most equal variances of all the models, but still unequal p=1.507e-06
#AIC and BIC values - this indicated that Mod4d (lmer with log10 transform) was the best fit
# Create a list of the models
Nrec_modlist <- list(Mod4c, Mod4d, Mod4e, Mod4f, Mod4g)
AIC_Nrec <- sapply(Nrec_modlist, AIC)
BIC_Nrec <- sapply(Nrec_modlist, BIC)
NrecAB <- data.frame(Model=c("Mod4c", "Mod4d", "Mod4e", "Mod4f", "Mod4g"),
                     AIC_Nrec, BIC_Nrec)
print(NrecAB)
# Model AIC_Nrec BIC_Nrec
#1 Mod4c 176.8876 257.3515
#2 Mod4d  43.4424 123.9063
#3 Mod4e 239.5915 320.0554
#4 Mod4f 701.5753 782.0392
#5 Mod4g 658.4141 738.8780


# R squared values - these were check for Mod4c=0.63, Mod4d=0.59, Mod4e=0.73, Mod4f=, Mod4g=0.76
# considering that none of the models fit very well, Mod4g was chosen as it had decent AIC, high rsq and 
# the residuals were the least abnormal

#run emmeans on Mod4g - most suitable
Mod4em <- emmeans(Mod4g,~Treatment|Soil, subset=(Nrec_out$Nrecovery))
Mod4em_cld <- cld(Mod4em, Letters=trimws(letters), reversed = TRUE, by="Soil")
View(Mod4em_cld)

## Visualizations
##select specific treatments and use those in the graph - repeat for all subsets
#Plotting dry weight using constant P and variable biochar rates
Nrec_trtVar <- c("Control2","CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha", 
                 "TripleSuperPhosphate")
Nrec_subVar <- Mod4em_cld %>%
  filter(Treatment %in% Nrec_trtVar)
(Nrec50kg <- ggplot(Nrec_subVar, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+2),
            size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="Nitrogen Recovery (%)") +
  scale_x_discrete(labels=c("Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow", 
                            "Phosphorus\nFertilizer"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Nrec50kg, file="Pots1_Nrec_50kg_ha.jpg", width=12, height=8, dpi=150)
#Plotting constant biochar rates with var P rates
Nrec_charCon <- c("Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                   "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                   "Willow10tha", "Willow10thaTSP")
Nrec_subCon <- Mod4em_cld %>%
  filter(Treatment %in% Nrec_charCon)
(Nrec10tha <- ggplot(Nrec_subCon, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+2),
              size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="Nitrogen Recovery (%)") +
  scale_x_discrete(labels=c("Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                            "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP",
                            "Phosphorus\nFertilizer"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Nrec10tha, file="Pots1_Nrec_10t_ha.jpg", width=12, height=8, dpi=150)





#### Total P ####  No need to analyze - only used for P uptake & P recovery




#### P uptake ####
## Issues with identical SE values - investigate
Pup_Mean <- summary_by(Puptake~Soil+Treatment, data=Pots1, FUN=mean) 
Pup_Mean <- as.numeric(Pup_Mean$Puptake)
Pup_skew <- skewness(Pup_Mean,na.rm=TRUE)
Pup_kur <- kurtosis(Pup_Mean,na.rm=TRUE)
cat("Skewness:", Pup_skew, "\n") ## data is not skewed @ 0.035
cat("Kurtosis:", Pup_kur, "\n") ## data has low/moderate kurtosis @ -1.115
shapiro.test(Pots1$Puptake)  # p=0.001144
hist(Pots1$Puptake) # left skewed
leveneTest(Puptake ~ Treatment*Soil, data=Pots1) # variances are equal; p=0.1601
#Mod5 - P uptake
Mod5 <- lm(Puptake~Soil*Treatment,data=Pots1) 
anova(Mod5)
summary(Mod5)
shapiro.test(resid(Mod5))  ##S-W p value=0.1046; Data is considered normal
plot(fitted(Mod4),resid(Mod4),pch=16) # slightly clustered to the left
qqnorm(resid(Mod4)) #long tails
qqline(resid(Mod4))
rsq(Mod5) # 0.9428224
## Run aov to compare
Mod5a <- aov(Puptake~Treatment*Soil, data=Pots1)
anova(Mod5a)
summary(Mod5a)
shapiro.test(resid(Mod5a)) #residuals are normally distributed: p=0.6237
plot(fitted(Mod5a),resid(Mod5a),pch=16) # residuals normal distribution
qqnorm(resid(Mod5a)) 
qqline(resid(Mod5a))
Mod5a_tidy <- tidy(Mod5a) 
Mod5asum_sq_reg <- Mod5a_tidy$sumsq[1] 
Mod5asum_sq_resid <- Mod5a_tidy$sumsq[2]  
Mod5asum_sq_reg / (Mod5asum_sq_reg + Mod5asum_sq_resid) # 0.9950879
# robust anova
Mod5b <- lmrob(Puptake ~ Treatment * Soil, data=Pots1, method="MM")
Mod5b_N <- length(residuals(Mod5b))
Mod5b_P <- length(coefficients(Mod5b)) - 1  # Exclude the intercept
(adj_rsq <- 1 - (1 - summary(Mod5b)$r.squared) * ((Mod5b_N - 1) / (Mod5b_N - Mod5b_P - 1))) # r=0.929
# Mod5c glmm
Mod5c <- glmmTMB(Puptake~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod5c, type="III")
summary(Mod5c)
performance::r2(Mod5c) # 0.938
shapiro.test(resid(Mod5c)) # p=0.6237
plot(fitted(Mod5c),resid(Mod5c),pch=16) # normal
qqnorm(resid(Mod5c)) # almost no tails
qqline(resid(Mod5c))

#AIC and BIC values - the models were equal
Pup_modlist <- list(Mod5c)
AIC_values <- sapply(Pup_modlist, AIC)
BIC_values <- sapply(Pup_modlist, BIC)
PupAB <- data.frame(Model=c("Mod5c"), AIC_values, BIC_values)
print(PupAB)
#Model AIC_values BIC_values
#1  Mod5   527.3229   613.2141
#2  Mod5a   527.3229   613.2141
#3  Mod5c   529.3229   617.9848
## rsq values: Mod5=0.94 , Mod5a= 0.99, Mod5b = 0.93, Mod5c= .094

#Mod5c chosen as it is a mixed model with high rsq
Mod5em <- emmeans(Mod5c,~Treatment|Soil)
Mod5em_cld <- cld(Mod5em, Letters=trimws(letters), reversed = TRUE, by="Soil", type="response")
View(Mod5em_cld)


## Visualizations
Pup_trtVar <- c("Control1", "Control2","CanolaMeal50kgha","CanolaHull50kgha","Manure50kgha","Willow50kgha",
                "TripleSuperPhosphate")
Pup_subVar <- Mod5em_cld %>% filter(Treatment %in% Pup_trtVar)
(Pup50kgha <- ggplot(Pup_subVar, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+0.8),
            size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="P uptake (ug/kg soil) for 50kg P/ha treatments") +
  scale_x_discrete(labels=c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow", 
                            "Phosphorus\nFertilizer"))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pup50kgha, file="Pots1_Puptake_50kg_ha.jpg", width=12, height=8, dpi=150)
#Plotting constant biochar rates with var P rates
Pup_charCon <- c("Control1", "Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                  "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                  "Willow10tha", "Willow10thaTSP")
Pup_subCon <- Mod5em_cld %>% filter(Treatment %in% Pup_charCon)
(Pup10tha <- ggplot(Pup_subCon, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+1),
            size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="P uptake (ug/kg soil) for 10t/ha treatments") +
  scale_x_discrete(labels=c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP",
                            "Phosphorus\nFertilizer"))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pup10tha, file="Pots1_Puptake_10t_ha.jpg", width=12, height=8, dpi=150)



#### P Recovery ####
Prec_Mean <- summary_by(Precovery~Soil+Treatment, data=Pots1, FUN=mean) 
Prec_Mean <- as.numeric(Prec_Mean$Precovery)
Prec_skew <- skewness(Prec_Mean,na.rm=TRUE)
Prec_kur <- kurtosis(Prec_Mean,na.rm=TRUE)
cat("Skewness:", Prec_skew, "\n") ## data is mildly skewed @ 1.12
cat("Kurtosis:", Prec_kur, "\n") ## data has low kurtosis @ 1.0798
hist(Pots1$Precovery) # moderately left skewed
leveneTest(Precovery ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 1.631e-06
## cannot log or sqrt transform as there are negative values in the dataset
#cannot use glmer with gamma distribution due to negative values
###Precovery has missing values for Control1 & Control2 - the subset removes these values
Prec <- Pots1[complete.cases(Pots1$Precovery),] #set up a subset removing the missing data only from column Precovery
View(Prec)
#Mod6 - ANOVA model - did not work Data is considered non-normal and needs to be transformed
Mod6 <- aov(Precovery~Treatment*Soil, data=Pots1)
anova(Mod6)
summary(Mod6)
shapiro.test(resid(Mod6)) #data is not normally distributed: p=3.472e-08
Mod6_tidy <- tidy(Mod6) #checking R squared value
Mod6_tidy
View(Mod6_tidy) #tidu output of the summary stas
Mod6sum_sq_reg <- Mod6_tidy$sumsq[1] #use summary stats to deteremine the sum squares regression
Mod6sum_sq_resid <- Mod6_tidy$sumsq[2]  # use the summary stats to determine the sum squares residuals
Mod6sum_sq_reg / (Mod6sum_sq_reg + Mod6sum_sq_resid) #calculate the R squared value: 0.9196
plot(fitted(Mod6),resid(Mod6),pch=16) # moderately normal distribution
qqnorm(resid(Mod6)) # tails severely off the line
qqline(resid(Mod6))
# Mod6a simple linear model
Mod6a <- lm(Precovery~Soil*Treatment,data=Prec)
summary(Mod6a)$adj.r.squared # check adjusted R squared value: 0.829
anova(Mod6a)
summary(Mod6a)
shapiro.test(resid(Mod6a)) #data is not normally distributed: 3.472e-08
plot(fitted(Mod6a),resid(Mod6a),pch=16) # left skewed but even distribution along the centre
qqnorm(resid(Mod6a)) # heavy tails
qqline(resid(Mod6a))
#Mod6b - applying Yeo-Johnson transformation
Prec_YJ <- yjPower(Pots1$Precovery, 0.5,jacobian.adjusted=TRUE)
Mod6b <- lmer(Prec_YJ ~ Treatment*Soil + (1|Soil), data=Pots1) # model failed to converge
rsq(Mod6b) # adjusted R squared: 0.8369
anova(Mod6b)
summary(Mod6b)
leveneTest(Prec_YJ~Treatment*Soil, data=Pots1)  #data has slightly less unequal variances 2.875e-08
shapiro.test(resid(Mod6b)) #data is not normally distributed: 3.853e-08
plot(fitted(Mod6b),resid(Mod6b),pch=16) # moderately clustered in the middel fo the graph and slightly towards the top
qqnorm(resid(Mod6b)) # heavy tails
qqline(resid(Mod6b))
# Mod 6c lmer model
Mod6c <- lme(Precovery ~ Treatment*Soil, random=~1|Soil, data=Prec)
summary(Mod6c)
anova(Mod6c)
shapiro.test(resid(Mod6c)) # p= 3.472e-08
plot(fitted(Mod6c),resid(Mod6c),pch=16) # clustered to left, equal around 0
qqnorm(resid(Mod6c)) # heavy  tails
qqline(resid(Mod6c))
rsq(Mod6c) # 0.88
plot(ranef(Mod6c))
# Mod6d glmm
Mod6d <- glmmTMB(Precovery~Treatment*Soil+(1|Soil), data=Prec, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod6d, type="III")
summary(Mod6d)
performance::r2(Mod6d) # 0.874
shapiro.test(resid(Mod6d)) # p= 3.472e-08
plot(fitted(Mod6d),resid(Mod6d),pch=16) # clustered to left, equal around 0
qqnorm(resid(Mod6d)) # heavy  tails
qqline(resid(Mod6d))

#AIC and BIC values
Prec_modlist <- list(Mod6b, Mod6c, Mod6d)
AIC_values <- sapply(Prec_modlist, AIC)
BIC_values <- sapply(Prec_modlist, BIC)
PrecAB <- data.frame(Model=c("Mod6b","Mod6c", "Mod6d"), AIC_values, BIC_values)
print(PrecAB)
#Model AIC_values BIC_values
#1 Mod6b   628.1842   701.1290
#2 Mod6c   627.4533   691.9672
#3 Mod6d   750.9711   823.9159

#rsq values: Mod6b=0.84, Mod6c=0.88, Mod6d=0.87 (Mod6b failed to converge)

#run emmeans on Mod6d - issues with Mods 6b & 6c
Mod6Em<- emmeans(Mod6d,~Treatment|Soil, subset=(Prec$Precovery), type="response")
Mod6Em_cld <- cld(Mod6Em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
#Mod6c resulted in Se values of 0 with df=0 and NaN for the upper and lower CIL
View(Mod6Em_cld)
print(Mod6Em_cld)

##Developing visualizations
Prec_trtVar <- c("CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha", "TripleSuperPhosphate")
Prec_subVar <- Mod6Em_cld %>%
  filter(Treatment %in% Prec_trtVar)
(Prec50kg <- ggplot(Prec_subVar, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  scale_y_continuous(limits=c(-10, 70))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+2.5),
              size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="P Recovery (%) for 50kg P/ha treatments") +
  scale_x_discrete(labels=c("Canola Meal", "Canola Hull", "Manure", "Willow", "Phosphorus\nFertilizer"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Prec50kg, file="Pots1_Prec_50kg_ha.jpg", width=12, height=8, dpi=150)
#Plotting constant biochar rates with var P rates
Prec_charCon <- c("CanolaMeal10tha", "CanolaHull10tha", "Manure10tha",  "Willow10tha", "CanolaMeal10thaTSP", 
                  "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP","TripleSuperPhosphate")
Prec_subCon <- Mod6Em_cld %>%
  filter(Treatment %in% Prec_charCon)
(Prec10tha <- ggplot(Prec_subCon, aes(x=Treatment, y=emmean, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+ 
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label = ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+2.5),
              size = 6, position = position_dodge(width = 0.9))+
  labs(x="Treatments", y="P Recovery (%) in 10t/ha treatments") +
  scale_x_discrete(labels=c("Canola Meal", "Canola Hull", "Manure", "Willow", "Canola Meal\n& TSP", 
                            "Canola Hull\n& TSP","Manure\n& TSP", "Willow\n& TSP", "Phosphorus\nFertilizer"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Prec10tha, file="Pots1_Prec_10t_ha.jpg", width=12, height=8, dpi=150)



####   NO3   ####
NO3_Mean <- summary_by(NO3~Soil+Treatment, data=Pots1, FUN=mean) 
NO3_Mean <- as.numeric(NO3_Mean$NO3)
NO3_skew <- skewness(NO3_Mean,na.rm=TRUE)
NO3_kur <- kurtosis(NO3_Mean,na.rm=TRUE)
cat("Skewness:", NO3_skew, "\n") ## data is mildly skewed @ 1.29
cat("Kurtosis:", NO3_kur, "\n") ## data has very low kurtosis @ 0.2929
hist(Pots1$NO3) #  left skewed
leveneTest(NO3 ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 2.878e-11
# Transform data
leveneTest(log(NO3) ~ Treatment*Soil, data=Pots1)  # much improved variance: 0.003112
leveneTest(log10(NO3) ~ Treatment*Soil, data=Pots1)  #  much improved variance: 0.003112
leveneTest(sqrt(NO3) ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 6.088e-07
hist((log(Pots1$NO3)))
hist((log10(Pots1$NO3)))
hist((sqrt(Pots1$NO3)))
#Mod 7
Mod7 <- aov(log10(NO3)~Treatment*Soil, data=Pots1)
anova(Mod7)
summary(Mod7)
shapiro.test(resid(Mod7)) #not normally distributed: 0.04706
plot(fitted(Mod7),resid(Mod7),pch=16) #slightly clustered to the left
qqnorm(resid(Mod7)) # slim tails
qqline(resid(Mod7))
Mod7_tidy <- tidy(Mod7) # checking R squared value
Mod7_tidy
View(Mod7_tidy) 
Mod7sum_sq_reg <- Mod7_tidy$sumsq[1] 
Mod7sum_sq_resid <- Mod7_tidy$sumsq[2]  
Mod7sum_sq_reg / (Mod7sum_sq_reg + Mod7sum_sq_resid) # 0.9784
# Mod7a lm
Mod7a <- lm(log10(NO3)~Treatment*Soil,data=Pots1)
anova(Mod7a) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod7a)
summary(Mod7a)$adj.r.squared  # 0.6804
shapiro.test(resid(Mod7a)) # p=0.04706
plot(fitted(Mod7a),resid(Mod7a),pch=16) # somewhat clustered to the left
qqnorm(resid(Mod7a)) # medium tails
qqline(resid(Mod7a))
# Mod7b glmm
Mod7b <- glmmTMB(NO3~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod7b, type="III")
summary(Mod7b)
performance::r2(Mod7b) # 0.767
shapiro.test(resid(Mod7b)) # p= 1.408e-09
plot(fitted(Mod7b),resid(Mod7b),pch=16) # clustered to left, equal around 0
qqnorm(resid(Mod7b)) # heavy  tails
qqline(resid(Mod7b))

#AIC and BIC values - this indicated that Mod7 was the best fit
NO3_modlist <- list(Mod7, Mod7a, Mod7b)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod7", "Mod7a", "Mod7b"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod7  -85.68761  -1.972715
#2 Mod7a  -85.68761  -1.972715
#3 Mod7b  575.15162 661.566995

#run emmeans on Mod7b - aov and lm doesn't work
Mod7em<- emmeans(Mod7b,~Treatment|Soil, subset=(Pots1$NO3), type="response")
Mod7em_cld <- cld(Mod7em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod7em_cld)
write.csv(Mod7em_cld, file="Pots1_NO3.csv")




####   NH4    ####
NH4_Mean <- summary_by(NH4~Soil+Treatment, data=Pots1, FUN=mean) 
NH4_Mean <- as.numeric(NH4_Mean$NH4)
NH4_skew <- skewness(NH4_Mean,na.rm=TRUE)
NH4_kur <- kurtosis(NH4_Mean,na.rm=TRUE)
cat("Skewness:", NH4_skew, "\n") ## data is mildly skewed @ 0.8415
cat("Kurtosis:", NH4_kur, "\n") ## data has low kurtosis @ 0.8102
shapiro.test(Pots1$NH4) # p=3.14e-05
hist(Pots1$NH4) #  left skewed
leveneTest(NH4 ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 2.37e-07
# Transform data
leveneTest(log(NH4) ~ Treatment*Soil, data=Pots1)  # improved variance: 5.441e-05
leveneTest(log10(NH4) ~ Treatment*Soil, data=Pots1)  #  improved variance: 5.441e-05
leveneTest(sqrt(NH4) ~ Treatment*Soil, data=Pots1)  # slightly improved variance: 2.975e-06
shapiro.test(log(Pots1$NH4)) # p=0.0004557
shapiro.test(log10(Pots1$NH4)) # p=0.0004557
shapiro.test(sqrt(Pots1$NH4)) # p=0.01667 data normally distributed
hist((log(Pots1$NH4)))
hist((log10(Pots1$NH4)))
hist((sqrt(Pots1$NH4)))
# Mod 8
Mod8 <- aov(sqrt(NH4)~Treatment*Soil, data=Pots1)
anova(Mod8)
summary(Mod8)
shapiro.test(resid(Mod8)) # p=0.002862
plot(fitted(Mod8),resid(Mod8),pch=16) # looks normal
qqnorm(resid(Mod8)) # medium tails
qqline(resid(Mod8))
Mod8_tidy <- tidy(Mod8) # checking R squared value
Mod8sum_sq_reg <- Mod8_tidy$sumsq[1] 
Mod8sum_sq_resid <- Mod8_tidy$sumsq[2]  
Mod8sum_sq_reg / (Mod8sum_sq_reg + Mod8sum_sq_resid) # 0.997
# Mod8a lm
Mod8a <- lm(sqrt(NH4)~Treatment*Soil,data=Pots1)
anova(Mod8a) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod8a)
summary(Mod8a)$adj.r.squared  # 0.8599
shapiro.test(resid(Mod8a)) # p=0.002862
plot(fitted(Mod8a),resid(Mod8a),pch=16) # looks normal, starting to cluster in vertical lines
qqnorm(resid(Mod8a)) # medium tails
qqline(resid(Mod8a))
# Mod8b glmm
Mod8b <- glmmTMB(NH4~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod8b, type="III")
summary(Mod8b)
performance::r2(Mod8b) # 0.864
shapiro.test(resid(Mod8b)) # p= 1.249e-06
plot(fitted(Mod8b),resid(Mod8b),pch=16) # clustered to left, equal around 0
qqnorm(resid(Mod8b)) # moderate-heavy  tails
qqline(resid(Mod8b))

#AIC and BIC values - this indicated that Mod8 was the best fit
NO3_modlist <- list(Mod8, Mod8a, Mod8b)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod8", "Mod8a", "Mod8b"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod8    39.4422   123.1571
#2  Mod8a   39.4422   123.1571
#3  Mod8b   473.6601  560.0755

#run emmeans on Mod8b - aov and lm shouldn't be used
Mod8em<- emmeans(Mod8b,~Treatment|Soil, subset=(Pots1$NH4), type="response")
Mod8em_cld <- cld(Mod8em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod8em_cld)
write.csv(Mod8em_cld, file="Pots1_NH4.csv")




#### PO4  ####
PO4_Mean <- summary_by(PO4~Soil+Treatment, data=Pots1, FUN=mean) 
PO4_Mean <- as.numeric(PO4_Mean$PO4)
PO4_skew <- skewness(PO4_Mean,na.rm=TRUE)
PO4_kur <- kurtosis(PO4_Mean,na.rm=TRUE)
cat("Skewness:", PO4_skew, "\n") ## data is moderately skewed @ 0.769
cat("Kurtosis:", PO4_kur, "\n") ## data has very low kurtosis @ -0.1055
shapiro.test(Pots1$PO4) # p=1.214e-08
hist(Pots1$PO4) #  left skewed
leveneTest(PO4 ~ Treatment*Soil, data=Pots1)  # data has unequal variance: p=4.024e-07
# Transform data
leveneTest(log(PO4) ~ Treatment*Soil, data=Pots1)  # equal variance: p=0.07069
leveneTest(log10(PO4) ~ Treatment*Soil, data=Pots1)  #  equal variance: p=0.07069
leveneTest(sqrt(PO4) ~ Treatment*Soil, data=Pots1)  # improved variance: p=0.001024
shapiro.test(log(Pots1$PO4)) # p=1.4e-06
shapiro.test(log10(Pots1$PO4)) # p=1.4e-06
shapiro.test(sqrt(Pots1$PO4)) # p=1.063e-06
hist((log(Pots1$PO4)))
hist((log10(Pots1$PO4)))
hist((sqrt(Pots1$PO4)))
#Mod9 linear model
Mod9<-lm(PO4~Treatment*Soil,data=Pots1) 
anova(Mod9) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod9)
summary(Mod9)$adj.r.squared  # 0.8969
shapiro.test(resid(Mod9)) # p=1.973e-10
plot(fitted(Mod9),resid(Mod9),pch=16) #clusered to the left
qqnorm(resid(Mod9)) # fat tails
qqline(resid(Mod9))
#Mod9a - anova
Mod9a <- aov(PO4~Treatment*Soil, data=Pots1) 
anova(Mod9a)
summary(Mod9a)
shapiro.test(resid(Mod9a)) #residuals not normal p=1.973e-10
plot(fitted(Mod9a),resid(Mod9a),pch=16) #clusteredto the left
qqnorm(resid(Mod9a)) # fat tails
qqline(resid(Mod9a))
Mod9a_tidy <- tidy(Mod9a)
Mod9asum_sq_reg <- Mod9a_tidy$sumsq[1] 
Mod9asum_sq_resid <- Mod9a_tidy$sumsq[2]  
Mod9asum_sq_reg / (Mod9asum_sq_reg + Mod9asum_sq_resid) # 0.3674
#Mod9b - lmer
Mod9b<-lmer(PO4~Treatment*Soil+(1|Soil),data=Pots1) #model convergence issues
anova(Mod9b)
summary(Mod9b)
rsq(Mod9b) # adjusted R squared: 0.91
shapiro.test(resid(Mod9b)) # p=1.973e-10
plot(fitted(Mod9b),resid(Mod9b),pch=16) #clusters are evident, especially to the left
qqnorm(resid(Mod9b)) # fat tails
qqline(resid(Mod9b))
#Mod9c - log transform data 
Mod9c<-lmer(log(PO4)~Treatment*Soil+(1|Soil),data=Pots1)
anova(Mod9c)
shapiro.test(resid(Mod9c))  # p=0.475
rsq(Mod9c) # adjusted R squared: 0.9536405
plot(fitted(Mod9c),resid(Mod9c),pch=16) # data looks more random
qqnorm(resid(Mod9c)) #tails have improved - almost normal
qqline(resid(Mod9c))
#Mod9d - results similar as for natural log
Mod9d<-lmer(log10(PO4)~Treatment*Soil+(1|Soil),data=Pots1) # model failed to converge
anova(Mod9d)
shapiro.test(resid(Mod9d))
rsq(Mod9d) # adjusted R squared: 0.953674
plot(fitted(Mod9d),resid(Mod9d),pch=16) # data looks more random
qqnorm(resid(Mod9d)) #tails have improved - almost normal
qqline(resid(Mod9d))
#Mod4e - sqrt transformation - did not work, data non-normally distributed and has unequal variances
Mod9e <- lmer(sqrt(PO4)~Treatment*Soil+(1|Soil),data=Pots1) # model failed to converge
anova(Mod9e)
summary(Mod9e)
shapiro.test(resid(Mod9e)) # p=5.173e-05
rsq(Mod9e) # adjusted r squared: 0.9486084
plot(fitted(Mod9e),resid(Mod9e),pch=16) # clusters forming
qqnorm(resid(Mod9e)) #fatter tails
qqline(resid(Mod9e))
#Mod4f using "gamma" distribution in glmer test - did not work, data less normally distributed with unequal variance
Mod9f <- glmer(PO4~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log")) # singular issues
anova(Mod9f)
summary(Mod9f)
shapiro.test(resid(Mod9f)) # p=0.3705
bf.test(Nrecovery~Treatment, data=Pots1) # p=2.507187e-07 , variances unequal
rsq(Mod9f) # adjusted r squared: 0.9486084
plot(fitted(Mod9f),resid(Mod9f),pch=16) # clusters forming
qqnorm(resid(Mod9f)) #fatter tails
qqline(resid(Mod9f))
#Mod9g glmer using gamma distribution with log transformation - better than Mod4f, similar to Mod4c
Mod9g <- glmer(log(PO4)~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log")) # singularity issues
anova(Mod9g)
summary(Mod9g)
shapiro.test(resid(Mod9g)) # p=0.3567
bf.test(PO4~Treatment, data=Pots1) # 0.0003149367 
rsq.glmm(Mod9g)
# Mod9h glmm
Mod9h <- glmmTMB(log(PO4)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod9h, type="III")
summary(Mod9h)
performance::r2(Mod9h) # 0.962
shapiro.test(resid(Mod9h)) # p= 0.475
plot(fitted(Mod9h),resid(Mod9h),pch=16) # normal
qqnorm(resid(Mod9h)) # slight  tails
qqline(resid(Mod9h))


# run AIC/BIC to test for best model fit
PO4_modlist <- list(Mod9, Mod9a, Mod9b, Mod9c, Mod9d, Mod9e, Mod9f, Mod9g, Mod9h)
AIC_values <- sapply(PO4_modlist, AIC)
BIC_values <- sapply(PO4_modlist, BIC)
PO4AB <- data.frame(Model=c("Mod9","Mod9a","Mod9b","Mod9c","Mod9d","Mod9e","Mod9f","Mod9g", "Mod9h"),
                    AIC_values, BIC_values)
print(PO4AB)

# Model  AIC_values BIC_values
#1  Mod9  454.700416  539.79331
#2 Mod9a  454.700416  539.79331
#3 Mod9b  420.101124  507.93895
#4 Mod9c   29.927791  117.76562
#5 Mod9d -111.857725  -24.01990
#6 Mod9e   91.535178  179.37301
#7 Mod9f  330.642924  418.48075
#8 Mod9g  -56.48083    31.3570
#9 Mod9h  -71.18115   16.65668

# Mod9h only appropriate model with no converge or other issues, very high rsq

#Run emmeans
Mod9em <- emmeans(Mod9h,~Treatment|Soil, subset=(Pots1$PO4>0), type="response")
Mod9cld <- cld(Mod9em, Letters=trimws(letters), reversed = TRUE, by="Soil")
View(Mod9cld)
write.csv(Mod9cld, file="Pots1_PO4.csv")



####   Resin P    ####
ResinP_Mean <- summary_by(ResinP~Soil+Treatment, data=Pots1, FUN=mean) 
ResinP_Mean <- as.numeric(ResinP_Mean$ResinP)
ResinP_skew <- skewness(ResinP_Mean,na.rm=TRUE)
ResinP_kur <- kurtosis(ResinP_Mean,na.rm=TRUE)
cat("Skewness:", ResinP_skew, "\n") ## data is mildly skewed @ 2.1
cat("Kurtosis:", ResinP_kur, "\n") ## data has low kurtosis @ 4.34
shapiro.test(Pots1$ResinP) # p=3.41e-12
hist(Pots1$ResinP) #  left skewed
leveneTest(ResinP ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 0.002065
# Transform data
leveneTest(log(ResinP) ~ Treatment*Soil, data=Pots1)  # improved variance: 0.132
leveneTest(log10(ResinP) ~ Treatment*Soil, data=Pots1)  #  improved variance: 0.132
leveneTest(sqrt(ResinP) ~ Treatment*Soil, data=Pots1)  # improved variance: 0.06481
shapiro.test(log(Pots1$ResinP)) # p=0.0002463
shapiro.test(log10(Pots1$ResinP)) # p=0.0002463
shapiro.test(sqrt(Pots1$ResinP)) # p=1.703e-05
hist((log(Pots1$ResinP)))
hist((log10(Pots1$ResinP)))
hist((sqrt(Pots1$ResinP)))
# Mod10
Mod10 <- aov(log10(ResinP)~Treatment*Soil, data=Pots1)
anova(Mod10)
summary(Mod10)
shapiro.test(resid(Mod10)) # p=0.01133
plot(fitted(Mod10),resid(Mod10),pch=16) # somewhat cluseterd to right
qqnorm(resid(Mod10)) # small tails
qqline(resid(Mod10))
Mod10_tidy <- tidy(Mod10) # checking R squared value
Mod10sum_sq_reg <- Mod10_tidy$sumsq[1] 
Mod10sum_sq_resid <- Mod10_tidy$sumsq[2]  
Mod10sum_sq_reg / (Mod10sum_sq_reg + Mod10sum_sq_resid) # 0.9033
# Mod10a lm
Mod10a <- lm(log10(ResinP)~Treatment*Soil,data=Pots1)
anova(Mod10a) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod10a)
summary(Mod10a)$adj.r.squared  # 0.7477
shapiro.test(resid(Mod10a)) # p=0.01133
plot(fitted(Mod10a),resid(Mod10a),pch=16) # cluistering to right
qqnorm(resid(Mod10a)) # small-medium tails
qqline(resid(Mod10a))
# Mod10b glmm
Mod10b <- glmmTMB(log(ResinP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod10b, type="III")
summary(Mod10b)
performance::r2(Mod10b) # 0.812
shapiro.test(resid(Mod10b)) # p= 0.01133
plot(fitted(Mod10b),resid(Mod10b),pch=16) # slight cluster to upper right
qqnorm(resid(Mod10b)) # small  tails
qqline(resid(Mod10b))


#AIC and BIC values - this indicated that Mod10 was the best fit
NO3_modlist <- list(Mod10, Mod10a, Mod10b)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod10", "Mod10a", "Mod10b"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod10    -71.30774   14.31965
#2 Mod10a  -71.30774   14.31965
#3 Mod10b  125.85585  214.24541

#run emmeans on Mod10b - only apprpriate model, high rsq
Mod10em<- emmeans(Mod10b,~Treatment|Soil, subset=(Pots1$ResinP), type="response")
Mod10em_cld <- cld(Mod10em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod10em_cld)
write.csv(Mod10em_cld, file="Pots1_ResinP.csv")



####   WaterSolP    ####
WaterSolP_Mean <- summary_by(WaterSolP~Soil+Treatment, data=Pots1, FUN=mean) 
WaterSolP_Mean <- as.numeric(WaterSolP_Mean$WaterSolP)
WaterSolP_skew <- skewness(WaterSolP_Mean,na.rm=TRUE)
WaterSolP_kur <- kurtosis(WaterSolP_Mean,na.rm=TRUE)
cat("Skewness:", WaterSolP_skew, "\n") ## data is mildly skewed @ 1.764
cat("Kurtosis:", WaterSolP_kur, "\n") ## data has low kurtosis @ 2.749
shapiro.test(Pots1$WaterSolP) # p=5.271e-12
hist(Pots1$WaterSolP) #  left skewed
leveneTest(WaterSolP ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 4.537e-07
# Transform data
leveneTest(log(WaterSolP) ~ Treatment*Soil, data=Pots1)  # improved variance: 0.04701
leveneTest(log10(WaterSolP) ~ Treatment*Soil, data=Pots1)  #  improved variance: 0.04701
leveneTest(sqrt(WaterSolP) ~ Treatment*Soil, data=Pots1)  # improved variance: 0.0006312
shapiro.test(log(Pots1$WaterSolP)) # p=8.324e-05
shapiro.test(log10(Pots1$WaterSolP)) # p=8.324e-05
shapiro.test(sqrt(Pots1$WaterSolP)) # p=1.39e-08
hist((log(Pots1$WaterSolP)))
hist((log10(Pots1$WaterSolP)))
hist((sqrt(Pots1$WaterSolP)))
# Mod11
Mod11 <- aov(log10(WaterSolP)~Treatment*Soil, data=Pots1)
anova(Mod11)
summary(Mod11)
shapiro.test(resid(Mod11)) # p=0.1704
plot(fitted(Mod11),resid(Mod11),pch=16) # somewhat clustered to left
qqnorm(resid(Mod11)) # small tails
qqline(resid(Mod11))
Mod11_tidy <- tidy(Mod11) # checking R squared value
Mod11sum_sq_reg <- Mod11_tidy$sumsq[1] 
Mod11sum_sq_resid <- Mod11_tidy$sumsq[2]  
Mod11sum_sq_reg / (Mod11sum_sq_reg + Mod11sum_sq_resid) # 0.9789
# Mod11a lm
Mod11a <- lm(log10(WaterSolP)~Treatment*Soil,data=Pots1)
anova(Mod11a) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod11a)
summary(Mod11a)$adj.r.squared  # 0.7369596
shapiro.test(resid(Mod11a)) # p=0.1704
plot(fitted(Mod11a),resid(Mod11a),pch=16) # clustered to left
qqnorm(resid(Mod11a)) # small tails
qqline(resid(Mod11a))
# Mod11b glmm
Mod11b <- glmmTMB(log(WaterSolP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod11b, type="III")
summary(Mod11b)
performance::r2(Mod11b) # 0.804
shapiro.test(resid(Mod11b)) # p= 0.1704
plot(fitted(Mod11b),resid(Mod11b),pch=16) # slight cluster to upper left
qqnorm(resid(Mod11b)) # small  tails
qqline(resid(Mod11b))

#AIC and BIC values - this indicated that Mod11 was the best fit
NO3_modlist <- list(Mod11, Mod11a, Mod11b)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod11", "Mod11a", "Mod11b"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod11    -247.85  -161.9588
#2 Mod11a    -247.85  -161.9588
#3 Mod11b  -49.01836   39.64354

#run emmeans on Mod11b 
Mod11em<- emmeans(Mod11b,~Treatment|Soil, subset=(Pots1$WaterSolP), type="response")
Mod11em_cld <- cld(Mod11em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod11em_cld)
write.csv(Mod11em_cld, file="Pots1_WaterSolP.csv")



####   Totalp2    ####
TotalP2_Mean <- summary_by(TotalP2~Soil+Treatment, data=Pots1, FUN=mean) 
TotalP2_Mean <- as.numeric(TotalP2_Mean$TotalP2)
TotalP2_skew <- skewness(TotalP2_Mean,na.rm=TRUE)
TotalP2_kur <- kurtosis(TotalP2_Mean,na.rm=TRUE)
cat("Skewness:", TotalP2_skew, "\n") ## data is mildly skewed @ 0.997
cat("Kurtosis:", TotalP2_kur, "\n") ## data has low kurtosis @ 0.284
shapiro.test(Pots1$TotalP2) # p=0.001277
hist(Pots1$TotalP2) #  left skewed
leveneTest(TotalP2 ~ Treatment*Soil, data=Pots1)  # data has equal variance: 0.1879
# Mod12
Mod12 <- aov(TotalP2~Treatment*Soil, data=Pots1)
anova(Mod12)
summary(Mod12)
shapiro.test(resid(Mod12)) # p=0.07059
plot(fitted(Mod12),resid(Mod12),pch=16) # somewhat clustered to left
qqnorm(resid(Mod12)) # small-medium tails
qqline(resid(Mod12))
Mod12_tidy <- tidy(Mod12) # checking R squared value
Mod12sum_sq_reg <- Mod12_tidy$sumsq[1] 
Mod12sum_sq_resid <- Mod12_tidy$sumsq[2]  
Mod12sum_sq_reg / (Mod12sum_sq_reg + Mod12sum_sq_resid) # 0.65341
# Mod12a lm
Mod12a <- lm(TotalP2~Treatment*Soil,data=Pots1)
anova(Mod12a) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod12a)
summary(Mod12a)$adj.r.squared  # 0.5276
shapiro.test(resid(Mod12a)) # p=0.07059
plot(fitted(Mod12a),resid(Mod12a),pch=16) # clustered to left
qqnorm(resid(Mod12a)) # small-medium tails
qqline(resid(Mod12a))
#Mod12b
Mod12b <- lmer(TotalP2~Treatment*Soil+(1|Soil), data=Pots1, na.action=na.exclude) #didn't converge properly
rsq(Mod12b)  # 0.6804
anova(Mod12b) 
summary(Mod12b)
shapiro.test(resid(Mod12b)) # p=0.07059
plot(fitted(Mod12b),resid(Mod12b),pch=16) # slightly clustered to left
qqnorm(resid(Mod12b)) # small-medium tails
qqline(resid(Mod12b))
# Mod12c glmm - convergence issues
Mod12c <- glmmTMB(TotalP2~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude,
                  control=glmmTMBControl(optimizer=optim, optArgs=list(parallel = TRUE, nthreads = 100)))
glmmTMB:::Anova.glmmTMB(Mod12c, type="III")
summary(Mod12c)
performance::r2(Mod12c) # 0.764
shapiro.test(resid(Mod12c)) # p= 2.019e-05
plot(fitted(Mod12c),resid(Mod12c),pch=16) # splitinto two vertical lines
qqnorm(resid(Mod12c)) # heavy right tail
qqline(resid(Mod12c))

#AIC and BIC values - this indicated that Mod12 was the best fit
NO3_modlist <- list(Mod12, Mod12a, Mod12b, Mod12c)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod12", "Mod12a", "Mod12b", "Mod12c"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod12    1149.313   1234.941
#2 Mod12a    1149.313   1234.941
#3 Mod12b    939.016   1027.406
#4 Mod12c         NA         NA

#run emmeans on Mod12b - highest Rsq & lowest AIC/BIc
Mod12em<- emmeans(Mod12b,~Treatment|Soil, subset=(Pots1$TotalP2), type="response")
Mod12em_cld <- cld(Mod12em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod12em_cld)
write.csv(Mod12em_cld, file="Pots1_TotalP2.csv")




####   pH   ####
pH_Mean <- summary_by(pH~Soil+Treatment, data=Pots1, FUN=mean) 
pH_Mean <- as.numeric(pH_Mean$pH)
pH_skew <- skewness(pH_Mean,na.rm=TRUE)
pH_kur <- kurtosis(pH_Mean,na.rm=TRUE)
cat("Skewness:", pH_skew, "\n") ## data is mildly skewed @ -0.649
cat("Kurtosis:", pH_kur, "\n") ## data has low kurtosis @ -0.348
shapiro.test(Pots1$pH) # p=0.0004384
hist(Pots1$pH) #  left skewed
leveneTest(pH ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 0.007327
# Transform data
leveneTest(log(pH) ~ Treatment*Soil, data=Pots1)  # slightly worsened variance: 0.006375
leveneTest(log10(pH) ~ Treatment*Soil, data=Pots1)  #  slightly worsened variance: 0.006375
leveneTest(sqrt(pH) ~ Treatment*Soil, data=Pots1)  # slightly worsened variance: 0.006841
shapiro.test(log(Pots1$pH)) # p=0.00028
shapiro.test(log10(Pots1$pH)) # p=0.00028
shapiro.test(sqrt(Pots1$pH)) # 0.0003507
hist((log(Pots1$pH)))
hist((log10(Pots1$pH)))
hist((sqrt(Pots1$pH)))
# Mod13
Mod13 <- aov(pH~Treatment*Soil, data=Pots1)
anova(Mod13)
summary(Mod13)
shapiro.test(resid(Mod13)) # p=1.083e-05
plot(fitted(Mod13),resid(Mod13),pch=16) # somewhat clustered to right
qqnorm(resid(Mod13)) # medium tails
qqline(resid(Mod13))
Mod13_tidy <- tidy(Mod13) # checking R squared value
Mod13sum_sq_reg <- Mod13_tidy$sumsq[1] 
Mod13sum_sq_resid <- Mod13_tidy$sumsq[2]  
Mod13sum_sq_reg / (Mod13sum_sq_reg + Mod13sum_sq_resid) # 0.53968
# Mod13a lm
Mod13a <- lm(pH~Treatment*Soil,data=Pots1)
anova(Mod13a) 
summary(Mod13a)
summary(Mod13a)$adj.r.squared  # 0.9189
shapiro.test(resid(Mod13a)) # p=1.083e-05
plot(fitted(Mod13a),resid(Mod13a),pch=16) # clustered to right
qqnorm(resid(Mod13a)) # medium tails
qqline(resid(Mod13a))
#Mod13b
Mod13b <- lmer(pH~Treatment*Soil+(1|Soil), data=Pots1) # convergence issues
print(vif(Mod13b)) #check collinearity
rsq(Mod13b)  # 0.9428
anova(Mod13b) 
summary(Mod13b)
shapiro.test(resid(Mod13b)) # p=1.083e-05
plot(fitted(Mod13b),resid(Mod13b),pch=16) # clustered to right
qqnorm(resid(Mod13b)) # medium tails
qqline(resid(Mod13b))
#Mod13c
Mod13c <- glmer(pH~Treatment*Soil+(1|Soil),data=Pots1, family=gaussian(link="log")) #singularity issues
print(vif(Mod13c)) 
rsq(Mod13c) # 0.9386
anova(Mod13c)
summary(Mod13c)
shapiro.test(resid(Mod13c))  #p=1.083e-05
bf.test(pH~Treatment, data=Pots1)  # variance equal p=0.2407674 
plot(fitted(Mod13c),resid(Mod13c),pch=16) # clustered to right
qqnorm(resid(Mod13c)) # medium tails
qqline(resid(Mod13c))
# Mod13d glmm - convergence issues
Mod13d <- glmmTMB(pH~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), 
                  control=glmmTMBControl(optimizer=optim, optArgs=list(parallel = TRUE, nthreads = 4)))
glmmTMB:::Anova.glmmTMB(Mod13d, type="III")
print(vif(Mod13d))
summary(Mod13d)
performance::r2(Mod13d) # 0.939
shapiro.test(resid(Mod13d)) # p= 8.949e-06
plot(fitted(Mod13d),resid(Mod13d),pch=16) # normalish
qqnorm(resid(Mod13d)) # moderate tails
qqline(resid(Mod13d))

# Rsq summary: Mod13=0.53968; Mod13a=0.9189; Mod13b=0.9428; Mod13c=0.9386
#AIC and BIC values - this indicated that Mod13 was the best fit
NO3_modlist <- list(Mod13, Mod13a, Mod13b, Mod13c, Mod13d)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod13", "Mod13a", "Mod13b", "Mod13c", "Mod13d"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod13  -464.4936  -378.0814
#2 Mod13a  -464.4936  -378.0814
#3 Mod13b  -263.3900  -174.1903
#4 Mod13c  -460.4936  -371.2939
#5 Mod13d         NA         NA

#run emmeans on Mod13c - singularity issue but only appropriate one that works
Mod13em<- emmeans(Mod13c,~Treatment|Soil, subset=(Pots1$pH), type="response")
Mod13em_cld <- cld(Mod13em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod13em_cld)
write.csv(Mod13em_cld, file="Pots1_pH.csv")






####  EC   ####
EC_Mean <- summary_by(EC~Soil+Treatment, data=Pots1, FUN=mean) 
EC_Mean <- as.numeric(EC_Mean$EC)
EC_skew <- skewness(EC_Mean,na.rm=TRUE)
EC_kur <- kurtosis(EC_Mean,na.rm=TRUE)
cat("Skewness:", EC_skew, "\n") ## data is mildly skewed @ 1.1314
cat("Kurtosis:", EC_kur, "\n") ## data has low kurtosis @ 0.7454
shapiro.test(Pots1$EC) # p=1.936e-08
hist(Pots1$EC) #  left skewed
leveneTest(EC ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 0.000132
# Transform data
leveneTest(log(EC) ~ Treatment*Soil, data=Pots1)  # improved variance: 0.004631
leveneTest(log10(EC) ~ Treatment*Soil, data=Pots1)  #  improved variance: 0.004631
leveneTest(sqrt(EC) ~ Treatment*Soil, data=Pots1)  # slightly improved variance: 0.000926
shapiro.test(log(Pots1$EC)) # p=6.109e-06
shapiro.test(log10(Pots1$EC)) # p=6.109e-06
shapiro.test(sqrt(Pots1$EC)) # 3.465e-07
hist((log(Pots1$EC)))
hist((log10(Pots1$EC)))
hist((sqrt(Pots1$EC)))
# Mod14
Mod14 <- aov(log(EC)~Treatment*Soil, data=Pots1)
anova(Mod14)
summary(Mod14)
shapiro.test(resid(Mod14)) # p=0.4634
plot(fitted(Mod14),resid(Mod14),pch=16) # somewhat clustered to left
qqnorm(resid(Mod14)) # small tails
qqline(resid(Mod14))
Mod14_tidy <- tidy(Mod14) # checking R squared value
Mod14sum_sq_reg <- Mod14_tidy$sumsq[1] 
Mod14sum_sq_resid <- Mod14_tidy$sumsq[2]  
Mod14sum_sq_reg / (Mod14sum_sq_reg + Mod14sum_sq_resid) # 0.9513
# Mod14a lm
Mod14a <- lm(log(EC)~Treatment*Soil,data=Pots1)
anova(Mod14a) #note that residuals=error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod14a)
summary(Mod14a)$adj.r.squared  # 0.75516
shapiro.test(resid(Mod14a)) # p=0.4634
plot(fitted(Mod14a),resid(Mod14a),pch=16) # clustered to left
qqnorm(resid(Mod14a)) # small tails
qqline(resid(Mod14a))
# Mod14b glmm - convergence issues
Mod14b <- glmmTMB(log(EC)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod14b, type="III")
summary(Mod14b)
performance::r2(Mod14b) # 0.818
shapiro.test(resid(Mod14b)) # p= 0.4634
plot(fitted(Mod14b),resid(Mod14b),pch=16) # normalish
qqnorm(resid(Mod14b)) # small tails
qqline(resid(Mod14b))

# Rsq summary: Mod14=0.95; Mod14a=0.76, Mod4b = 0.82
#AIC and BIC values 
NO3_modlist <- list(Mod14, Mod14a, Mod14b)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod14", "Mod14a", "Mod14b"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod14  -228.8464  -143.4851
#2 Mod14a  -228.8464  -143.4851
#3 Mod14b  -226.8464  -138.7315

#run emmeans on Mod14b
Mod14em<- emmeans(Mod14b,~Treatment|Soil, subset=(Pots1$EC), type="response")
Mod14em_cld <- cld(Mod14em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod14em_cld)
write.csv(Mod14em_cld, file="Pots1_EC.csv")



####   OC   ####
OC_Mean <- summary_by(OC~Soil+Treatment, data=Pots1, FUN=mean) 
OC_Mean <- as.numeric(OC_Mean$OC)
OC_skew <- skewness(OC_Mean,na.rm=TRUE)
OC_kur <- kurtosis(OC_Mean,na.rm=TRUE)
cat("Skewness:", OC_skew, "\n") ## data is slightly skewed @ -0.352
cat("Kurtosis:", OC_kur, "\n") ## data has low/medium kurtosis @ -1.32
shapiro.test(Pots1$OC) # p=0.002034
hist(Pots1$OC) #  slightly left skewed
leveneTest(OC ~ Treatment*Soil, data=Pots1)  # data has unequal variance: 2.511e-07
# Transform data
leveneTest(log(OC) ~ Treatment*Soil, data=Pots1)  # slightly improved variance: 2.138e-06
leveneTest(log10(OC) ~ Treatment*Soil, data=Pots1)  #  slightly improved variance: 2.138e-06
leveneTest(sqrt(OC) ~ Treatment*Soil, data=Pots1)  # slightly improved variance: 5.999e-07
shapiro.test(log(Pots1$OC)) # p=0.001123
shapiro.test(log10(Pots1$OC)) # p=0.001123
shapiro.test(sqrt(Pots1$OC)) # p=0.0024
hist((log(Pots1$OC)))
hist((log10(Pots1$OC)))
hist((sqrt(Pots1$OC)))
# Mod15
Mod15 <- aov(OC~Treatment*Soil, data=Pots1)
anova(Mod15)
summary(Mod15)
shapiro.test(resid(Mod15)) # p=5.413e-09
plot(fitted(Mod15),resid(Mod15),pch=16) # 2 clusters forming
qqnorm(resid(Mod15)) # medium/fat tails
qqline(resid(Mod15))
Mod15_tidy <- tidy(Mod15) # chOCking R squared value
Mod15sum_sq_reg <- Mod15_tidy$sumsq[1] 
Mod15sum_sq_resid <- Mod15_tidy$sumsq[2]  
Mod15sum_sq_reg / (Mod15sum_sq_reg + Mod15sum_sq_resid) # 0.1746
# Mod15a lm
Mod15a <- lm(OC~Treatment*Soil,data=Pots1)
anova(Mod15a) 
summary(Mod15a)
summary(Mod15a)$adj.r.squared  # 0.9101
shapiro.test(resid(Mod15a)) # p=5.413e-09
plot(fitted(Mod15a),resid(Mod15a),pch=16) # 2 clusters forming
qqnorm(resid(Mod15a)) # fat tails
qqline(resid(Mod15a))
#Mod13b
Mod15b <- lmer(OC~Treatment*Soil+(1|Soil), data=Pots1, na.action=na.exclude) # convergence issues
rsq(Mod15b)  # 0.9366
anova(Mod15b) 
summary(Mod15b)
shapiro.test(resid(Mod15b)) # p=5.413e-09
plot(fitted(Mod15b),resid(Mod15b),pch=16) # 2 clusters forming
qqnorm(resid(Mod15b)) # heavy tails
qqline(resid(Mod15b))
#Mod15c
Mod15c <- glmer(OC~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log"), na.action=na.exclude) #singularity
anova(Mod15c)
summary(Mod15c)
shapiro.test(resid(Mod15c))  #p=2.297e-07
bf.test(OC~Treatment, data=Pots1)  # variance equal p=0.2546385 
plot(fitted(Mod15c),resid(Mod15c),pch=16) # 2 clusters forming
qqnorm(resid(Mod15c)) # heavy tails
qqline(resid(Mod15c))
#Mod15d
Mod15d <- glm(OC ~ Treatment*Soil, family=gaussian, data=Pots1)
rsq(Mod15d) # 0.9334
anova(Mod15d, test="Chisq")
summary(Mod15d)
shapiro.test(resid(Mod15d))  # residuals p=5.413e-09
plot(fitted(Mod15d),resid(Mod15d),pch=16) # 2 clusters forming
qqnorm(resid(Mod15d))  # fat tails
qqline(resid(Mod15d))
#Mod15e - YJ transformation
OC_YJ <- yjPower(Pots1$OC, 0.5,jacobian.adjusted=TRUE) # convergende issues
Mod15e <- lmer(OC_YJ ~ Treatment*Soil + (1|Soil), data=Pots1, control=lmerControl(optCtrl=list(maxfun=100000)))
rsq(Mod15e) # adjusted R squared: 0.9223
anova(Mod15e)
summary(Mod15e)
leveneTest(OC_YJ~Treatment*Soil, data=Pots1)  #data has unequal variances 4.059e-07
shapiro.test(resid(Mod15e)) #data is not normally distributed: 1.727e-08
plot(fitted(Mod15e),resid(Mod15e),pch=16) # 2 clusters
qqnorm(resid(Mod15e)) # fat tails
qqline(resid(Mod15e))
# Mod115f glmm - convergence issues
Mod15f <- glmmTMB(OC~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Mod15f, type="III")
summary(Mod15f)
performance::r2(Mod15f) # 0.934
shapiro.test(resid(Mod15f)) # p= 5.413e-09
plot(fitted(Mod15f),resid(Mod15f),pch=16) # normalish
qqnorm(resid(Mod15f)) # heavy tails
qqline(resid(Mod15f))


# Rsq summary: Mod15=0.17; Mod15a=0.91, Mod15b=0.94; Mod15c=??; Mod15d=0.93; Mod15e=0.92; Mod15f=0.93
#AIC and BIC values 
NO3_modlist <- list(Mod15, Mod15a, Mod15b, Mod15c, Mod15d, Mod15e, Mod15f)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod15", "Mod15a", "Mod15b", "Mod15c", "Mod15d", "Mod15e", "Mod15f"),
                    AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod15  -59.00536   25.54366
#2 Mod15a  -59.00536   25.54366
#3 Mod15b   40.30443  127.58084
#4 Mod15c  -62.73739   24.53902
#5 Mod15d  -59.00536   25.54366
#6 Mod15e   36.31864  123.59505
#7 Mod15f -254.09000 -166.81359

# Appropriate model options are Mods15 b c e f

#run emmeans on Mod15f - only one that makes sense with emmeans and cld
Mod15em<- emmeans(Mod15f,~Treatment|Soil, subset=(Pots1$OC), type="response")
Mod15em_cld <- cld(Mod15em, Letters=trimws(letters), reversed = TRUE, by="Soil") 
View(Mod15em_cld)
write.csv(Mod15em_cld, file="Pots1_OC.csv")




####   Water holding capacity  ####
# no reps, couldn't fit to a model
WHC <- data.frame(
  Soil=as.factor(rep(c("Haverhill", "Oxbow"), each=5)),
  Treatment=as.factor(c("Control 2", "Canola Meal 10 t/ha", "Canola Hull 10 t/ha", "Manure 10 t/ha",
                        "Willow 10 t/ha", "Control 2", "Canola Meal 10 t/ha", "Canola Hull 10 t/ha", 
                        "Manure 10 t/ha", "Willow 10 t/ha")),
  VolWatContent=c(21, 22, 21, 24, 21, 22, 23, 24, 24, 24))
WHC$Soil <- factor(WHC$Soil, levels = c("Haverhill", "Oxbow"))
print(WHC)
# boxplot
(Pots1WHCbox <- ggplot(WHC, aes(x=Soil, y=VolWatContent))+
    geom_boxplot(width=0.5, fill="grey89") +
    geom_point(aes(color = Treatment), position = position_dodge(width = 0.75), size = 3) +
    geom_label_repel(aes(label = VolWatContent, color = Treatment), fill = "white",
                     box.padding = 0.5, segment.color = "black", segment.size = 0.5,
                     size = 6, nudge_x = 0.2, nudge_y = 0) +
    scale_color_manual(values = c("Control 2" = "red", "Canola Meal 10 t/ha" = "blue",
                                  "Canola Hull 10 t/ha" = "green4", "Manure 10 t/ha" = "orange3",
                                  "Willow 10 t/ha" = "purple"),
                       labels = c("Control 2", "Canola Meal 10 t/ha", "Canola Hull 10 t/ha",
                                  "Manure 10 t/ha", "Willow 10 t/ha")) +
    labs(x = "Soil", y = "Volumetric Water Content %") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size=18, face = "bold"),
          axis.title.x = element_text(size = 22, face = "bold"),
          axis.title.y = element_text(size = 22, face = "bold")))
#geom_text(aes(label=Treatment), position=position_dodge(width=0.5), vjust=-0.7, color="steelblue", size=3) +
ggsave(Pots1WHCbox, file="WaterHoldingCapacity_boxplot.jpg", width=8, height=8, dpi=150)


#model data
#ModWHC1 <- lm(VolWatContent~Treatment*Soil, data=WHC)
#anova(ModWHC1)
#summary(ModWHC1)
#shapiro.test(resid(ModWHC1)) # p=0.0008813
#hist(resid(ModWHC1)) # heavily flattened tails
#qqnorm(resid(ModWHC1)) # heavy tails
#qqline(resid(ModWHC1))
#pairwise.t.test(WHC$VolWatContent, list(WHC$Treatment, WHC$Soil))
#WHC %>% group_by(Soil) %>% do(tidy(pairwise.t.test(.$VolWatContent, .$Treatment)))

#bar graph - not used
(Pots1WHCbars <- ggplot(WHC, aes(x=Treatment, y=VolWatContent, pattern=Soil)) +
  geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values=c("Haverhill"="stripe", "Oxbow"="crosshatch"), 
                       labels=c("Haverhill", "Oxbow"))+
  geom_text(aes(label=paste0(VolWatContent, "%"), fontface=ifelse(Soil == "Haverhill", "italic", "plain")),
            size=6, position=position_dodge2(width=0.9), vjust=-1) +
  labs(x="Treatments", y="Volumetric Water Content (%)") +
  scale_x_discrete(labels=c("Control 2", "Canola Meal\n10t/ha", "Canola Hull\n10t/ha", "Manure\n10t/ha", "Willow\n10t/h"))+
  scale_y_continuous(limits=c(0, 28))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=14), legend.title = element_text(size = 16),
        plot.title=element_text(size=18),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x=element_blank(), axis.title.y=element_text(size=22, face="bold"),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pots1WHCbars, file="Pots1_WatHolCapacity.jpg", width=8, height=8, dpi=150)



####  Co-variance ####
#####  Haverhill  #####
######  Yield ######
#Split and scale data
YieldCovVar <- c("Yield", "NO3", "NH4", "PO4", "ResinP", "WaterSolP", "TotalP2", "pH", "EC", "OC")
HavCovYield <- subset(Pots1, Soil == "Haverhill", select=c("Treatment", YieldCovVar), # select per soil
                      na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
HavCovScaleYield <- as.data.frame(scale(HavCovYield[,-1])) # scale & remove treatment name from data frame
HavCovScaleYield$Treatment <- HavCovYield$Treatment # add treatment from oiginal subset to scales data frame
HavCovScaleYieldSplit <- split(HavCovScaleYield[, -ncol(HavCovScaleYield)], HavCovScaleYield$Treatment)
#HavCovScaleYieldSplit <- lapply(HavCovScaleYieldSplit, function(x) x[, -ncol(x)]) - used to remove last column

## calculate the covariance matrix for each treatment excluding missing data
YieldCov_Hav <- lapply(HavCovScaleYieldSplit, function(x) cov(x, use="pairwise.complete.obs"))
#YieldCov_Hav2 <- cov(HavCovScaleYield) - didn't work
YieldCovHavWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(YieldCov_Hav)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(YieldCov_Hav)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(YieldCovHavWb, sheet_name)
  writeData(YieldCovHavWb, sheet=sheet_name, x=YieldCov_Hav[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(YieldCovHavWb, "Pots1_Yield_CovMatrix_Haverhill.xlsx")
# Convert each covariance matrix to a dataframe
## Option 1 - provides a list of matrices
YieldCovHav_df1 <- lapply(YieldCov_Hav, as.data.frame) # converts everything into a combined dataframe
YieldCovHav_df1$treatment <- as.factor(YieldCovHav_df1$treatment) # adds a column for treatment
## Option 2 - does not include treatment names
YieldCovHav_df2 <- lapply(seq_along(YieldCov_Hav), function(i) {
  YieldCov_Hav <- as.matrix(YieldCov_Hav[[i]])
  rownames(YieldCov_Hav) <- colnames(YieldCov_Hav) # add rownames
  YieldCovHav_df1 <- melt(YieldCov_Hav) # reshape to long format
  YieldCovHav_df1$treatment <- names(YieldCov_Hav)[i] # add treatment column
  return(YieldCovHav_df1)
})
# Option 3 - best option 
YieldCovHav_df3 <- lapply(seq_along(YieldCov_Hav), function(i) {
  cov_mat1h <- as.matrix(YieldCov_Hav[[i]])
  cov_mat1h <- setNames(cov_mat1h, YieldCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(YieldCov_Hav)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
YieldCovHav_dfAll <- do.call(rbind, YieldCovHav_df3)
YieldCovHav_dfAll$Var1 <- factor(YieldCovHav_dfAll$Var1, levels=YieldCovVar)
YieldCovHav_dfAll$variable <- factor(YieldCovHav_dfAll$variable, levels=rev(YieldCovVar))
YieldCovHav_dfAll$treatment <- factor(YieldCovHav_dfAll$treatment, 
        levels=c("Control1", "Control2", "CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha", 
                 "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP", 
                 "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate"),
        labels=c("Control 1", "Control 2", "Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", 
                 "Manure 50kg P/ha", "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", 
                 "Manure 10t/ha", "Willow 10t/ha", "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", 
                 "Manure 10t/ha & TSP", "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
write.csv(YieldCovHav_dfAll, file="Pots1_Haverhill_YieldCov.csv")
# Generate the heatmap for each treatment and facet wrap them
# Do not use ggcorrplot- it is only applicable to correlation matrices
# heatmap(YieldCovHav_df, Rowv=NA, Colv=NA, col=rev(heat.colors(256))) - did not work
# ggplot best option - brackets on both sides of the variable and plot code assigns and calls all in one
(YieldCovHavHeat <- ggplot(YieldCovHav_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
  geom_tile() +
  scale_fill_gradientn(colors=brewer.pal(9, "YlGnBu"), limits=c(-1.5, 3.7), breaks=seq(-1.5, 3.7, by=0.5)) +
    #specify colour palette and limits specifies the upper and lower cov values, breaks specify legend details
  facet_wrap(~ treatment, nrow=5, ncol=3, scales="fixed") + #works better than facet grid to show separate treatments
  geom_text(aes(label=round(Covariance, 3)))+ # adds in covariance values within each block
  theme(legend.title=element_text(size=20, face="bold"), legend.key.size=unit(15,"mm"),
        legend.text=element_text(size=20), 
        strip.text=element_text(size=20, face="bold"), # strip=treatment labels at the top of each panel
        strip.placement="outside", # if not specified, then the label is immediately adjacent
        strip.background=element_blank(), # if not specified then it's grey
        strip.text.y=element_text(angle=0, vjust=0.5),
        strip.text.x=element_text(vjust=1),
        axis.line=element_blank(), # this is to remove x- and y-axes labels from the individual panels
        axis.text.x.bottom=element_text(size=15, angle=45, hjust=1), #place horizontal labels only along the bottom
        axis.text.y.left=element_text(size=15), #place vertical labels only along the left panel
        panel.spacing.x=unit(1, "cm"))+ # places a wider space between panels
  labs(x="", y=""))
ggsave(YieldCovHavHeat, file="Pots1_YieldCovHavHeat.jpg", width=20, height=20, dpi=150)

## Develop PCA for Haverhill Yield
# this uses the setup from the 'Split and scale data' section above
#HavYield_PCA <- princomp(YieldCov_Hav, cor=FALSE)
#HavYield_PCA <- prcomp(HavCovScaleYieldSplit[,], center=TRUE, scale.=TRUE) - issues with data not in a matrix
#HavYield_PCA <- lapply(YieldCovHav_dfAll, function(x) princomp(x))
# Extract covariance matrices from each data frame in the list
#HavYieldCov_matrices <- lapply(YieldCovHav_df3, function(df) {
#  df_mat <- as.matrix(df[, 3])
#  dimnames(df_mat) <- list(df$Var1, df$Var2)
#  return(df_mat)
#})
# Perform PCA on each covariance matrix
#HavYield_PCA_list <- lapply(HavYieldCov_matrices, prcomp, center=TRUE, scale.=TRUE)
# Get summary information for each PCA
#HavYield_PCA_Summary <- lapply(HavYield_PCA_list, summary)
# Get the proportion of variance explained by each PC for each treatment
#HavYield_PCA_output <- lapply(HavYield_PCA_list, function(pca) {
#  var_prop <- pca$sdev^2 / sum(pca$sdev^2)
#  return(var_prop)
#})
# Plot PCA scores separately for each treatment
#ggplot(augment(pca), aes(PC1, PC2, color=Treatment)) +
#  geom_point() +
#  facet_wrap(~ Treatment) +
#  labs(x=paste0("PC1 (", round(summary(pca)$importance[2, 1]*100, 2), "%)"),
#       y=paste0("PC2 (", round(summary(pca)$importance[2, 2]*100, 2), "%)"),
#       title="PCA scores for Haverhill soil")



######  P uptake  ######
UptakeCovVar <- c("Puptake", "NO3", "NH4", "PO4", "ResinP", "WaterSolP", "TotalP2", "pH", "EC", "OC")
HavCovUptake <- subset(Pots1, Soil == "Haverhill", select=c("Treatment", UptakeCovVar), 
                      na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
HavCovScaleUptake <- as.data.frame(scale(HavCovUptake[,-1]))
HavCovScaleUptake$Treatment <- HavCovUptake$Treatment
HavCovScaleUptakeSplit <- split(HavCovScaleUptake[, -ncol(HavCovScaleUptake)], HavCovScaleUptake$Treatment)
## calculate the covariance matrix for each treatment excluding missing data
UptakeCov_Hav <- lapply(HavCovScaleUptakeSplit, function(x) cov(x, use="pairwise.complete.obs"))
UptakeCovHavWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(UptakeCov_Hav)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(UptakeCov_Hav)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(UptakeCovHavWb, sheet_name)
  writeData(UptakeCovHavWb, sheet=sheet_name, x=UptakeCov_Hav[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(UptakeCovHavWb, "Pots1_Uptake_CovMatrix_Haverhill.xlsx")
# Convert each covariance matrix to a dataframe
UptakeCovHav_df <- lapply(seq_along(UptakeCov_Hav), function(i) {
  cov_mat1h <- as.matrix(UptakeCov_Hav[[i]])
  cov_mat1h <- setNames(cov_mat1h, UptakeCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(UptakeCov_Hav)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
UptakeCovHav_dfAll <- do.call(rbind, UptakeCovHav_df)
UptakeCovHav_dfAll$Var1 <- factor(UptakeCovHav_dfAll$Var1, levels=UptakeCovVar, labels=c("Puptake"="P Uptake", 
          "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", "WaterSolP"="Water Soluble P", 
          "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
UptakeCovHav_dfAll$variable <- factor(UptakeCovHav_dfAll$variable, levels=UptakeCovVar, labels=
          c("Puptake"="P Uptake", "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", 
            "WaterSolP"="Water Soluble P", "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
UptakeCovHav_dfAll$treatment <- factor(UptakeCovHav_dfAll$treatment, 
       levels=c("Control1", "Control2", "CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha", 
             "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP", 
             "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate"),
       labels=c("Control 1", "Control 2", "Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", "Manure 50kg P/ha", 
                  "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", "Manure 10t/ha", "Willow 10t/ha", 
                  "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", "Manure 10t/ha & TSP", 
                  "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
write.csv(UptakeCovHav_dfAll, file="Pots1_Haverhill_UptakeCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(UptakeCovHavHeat <- ggplot(UptakeCovHav_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "PuBuGn"), limits=c(-1.2, 3.7), breaks=seq(-1.2, 3.7, by=0.5)) +
    facet_wrap(~ treatment, nrow=5, ncol=3, scales="fixed") +
    geom_text(aes(label=round(Covariance, 3))) +
    geom_text(aes(label=round(Covariance, 3)))+
    theme(legend.title=element_text(size=20, face="bold"), legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=20, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=15, angle=45, hjust=1),
          axis.text.y.left=element_text(size=15),
          panel.spacing.x=unit(1, "cm"))+
    labs(x="", y=""))
ggsave(UptakeCovHavHeat, file="Pots1_UptakeCovHavHeat.jpg", width=20, height=20, dpi=150)

######   P Recovery  #######
RecoveryCovVar <- c("Precovery", "NO3", "NH4", "PO4", "ResinP", "WaterSolP", "TotalP2", "pH", "EC", "OC")
HavCovRecovery <- subset(Pots1, Soil == "Haverhill", select=c("Treatment", RecoveryCovVar), 
                       na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
HavCovScaleRecovery <- as.data.frame(scale(HavCovRecovery[,-1]))
HavCovScaleRecovery$Treatment <- HavCovRecovery$Treatment
HavCovScaleRecoverySplit <- split(HavCovScaleRecovery[, -ncol(HavCovScaleRecovery)], HavCovScaleRecovery$Treatment)
RemoveControls <- c("Control1", "Control2")
HavCovScaleRecoverySplit <- HavCovScaleRecoverySplit[!(names(HavCovScaleRecoverySplit) %in% RemoveControls)]
## calculate the covariance matrix for each treatment excluding missing data
RecoveryCov_Hav <- lapply(HavCovScaleRecoverySplit, function(x) cov(x, use="pairwise.complete.obs"))
RecoveryCovHavWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(RecoveryCov_Hav)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(RecoveryCov_Hav)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(RecoveryCovHavWb, sheet_name)
  writeData(RecoveryCovHavWb, sheet=sheet_name, x=RecoveryCov_Hav[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(RecoveryCovHavWb, "Pots1_Recovery_CovMatrix_Haverhill.xlsx")
# Convert each covariance matrix to a dataframe
RecoveryCovHav_df <- lapply(seq_along(RecoveryCov_Hav), function(i) {
  cov_mat1h <- as.matrix(RecoveryCov_Hav[[i]])
  cov_mat1h <- setNames(cov_mat1h, RecoveryCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(RecoveryCov_Hav)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
RecoveryCovHav_dfAll <- do.call(rbind, RecoveryCovHav_df)
RecoveryCovHav_dfAll$Var1 <- factor(RecoveryCovHav_dfAll$Var1, levels=RecoveryCovVar, labels=c("Precovery"="P Recovery", 
         "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", "WaterSolP"="Water Soluble P", 
         "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
RecoveryCovHav_dfAll$variable <- factor(RecoveryCovHav_dfAll$variable, levels=RecoveryCovVar, labels=
         c("Precovery"="P Recovery", "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", 
         "WaterSolP"="Water Soluble P", "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
RecoveryCovHav_dfAll$treatment <- factor(RecoveryCovHav_dfAll$treatment, 
         levels=c("CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha", 
                  "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP", 
                  "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate"),
         labels=c("Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", "Manure 50kg P/ha", 
                  "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", "Manure 10t/ha", "Willow 10t/ha", 
                  "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", "Manure 10t/ha & TSP", 
                  "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
write.csv(RecoveryCovHav_dfAll, file="Pots1_Haverhill_RecoveryCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(RecoveryCovHavHeat <- ggplot(RecoveryCovHav_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "YlOrRd"), limits=c(-1.2, 3.7), breaks=seq(-1.2, 3.7, by=0.5)) +
    facet_wrap(~ treatment, nrow=5, ncol=3, scales="fixed") +
    geom_text(aes(label=round(Covariance, 3)))+
    theme(legend.title=element_text(size=20, face="bold"), legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=20, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=15, angle=45, hjust=1),
          axis.text.y.left=element_text(size=15),
          panel.spacing.x=unit(1, "cm"))+
        labs(x="", y=""))
ggsave(RecoveryCovHavHeat, file="Pots1_RecoveryCovHavHeat.jpg", width=20, height=20, dpi=150)




#####  Oxbow  #####
######   Yield  ######
OxCovYield <- subset(Pots1, Soil=="Oxbow", select=c("Treatment", YieldCovVar),  
                     na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
OxCovScaleYield <- as.data.frame(scale(OxCovYield[,-1]))
OxCovScaleYield$Treatment <- OxCovYield$Treatment
OxCovScaleYieldSplit <- split(OxCovScaleYield[, -ncol(OxCovScaleYield)], OxCovScaleYield$Treatment)
YieldCov_Ox <- lapply(OxCovScaleYieldSplit, function(x) cov(x, use="pairwise.complete.obs"))
YieldCovOxWb <- createWorkbook() 
for (i in seq_along(YieldCov_Ox)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(YieldCov_Ox)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(YieldCovOxWb, sheet_name)
  writeData(YieldCovOxWb, sheet=sheet_name, x=YieldCov_Ox[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(YieldCovOxWb, "Pots1_Yield_CovMatrix_Oxbow.xlsx")
# Convert each covariance matrix to a dataframe
YieldCovOx_df <- lapply(seq_along(YieldCov_Ox), function(i) {
  cov_mat1h <- as.matrix(YieldCov_Ox[[i]])
  cov_mat1h <- setNames(cov_mat1h, YieldCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(YieldCov_Ox)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
YieldCovOx_dfAll <- do.call(rbind, YieldCovOx_df)
YieldCovOx_dfAll$Var1 <- factor(YieldCovOx_dfAll$Var1, levels=YieldCovVar, labels=c("Yield"="Yield", 
            "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", "WaterSolP"="Water Soluble P", 
            "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
YieldCovOx_dfAll$variable <- factor(YieldCovOx_dfAll$variable, levels=YieldCovVar, labels=
            c("Yield"="Yield", "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", 
              "WaterSolP"="Water Soluble P", "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
YieldCovOx_dfAll$treatment <- factor(YieldCovOx_dfAll$treatment, 
        levels=c("Control1", "Control2", "CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha", 
                 "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP", 
                 "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate"),
        labels=c("Control 1", "Control 2", "Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", "Manure 50kg P/ha", 
                 "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", "Manure 10t/ha", "Willow 10t/ha", 
                 "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", "Manure 10t/ha & TSP", 
                 "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
write.csv(YieldCovOx_dfAll, file="Pots1_Oxbow_YieldCov.csv")
# ggplot best option - brackets on both sides of the variable and plot code assigns and calls all in one
(YieldCovOxHeat <- ggplot(YieldCovOx_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "YlGnBu"), limits=c(-0.9, 2.7), breaks=seq(-0.9, 2.7, by=0.5)) +
    facet_wrap(~ treatment, nrow=5, ncol=3, scales="fixed") +
    geom_text(aes(label=round(Covariance, 3)))+
    theme(legend.title=element_text(size=20, face="bold"), legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=20, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=15, angle=45, hjust=1),
          axis.text.y.left=element_text(size=15),
          panel.spacing.x=unit(1, "cm"))+
    labs(x="", y=""))
ggsave(YieldCovOxHeat, file="Pots1_YieldCovOxHeat.jpg", width=20, height=20, dpi=150)



######   Uptake  ######
UptakeCovVar <- c("Puptake", "NO3", "NH4", "PO4", "ResinP", "WaterSolP", "TotalP2", "pH", "EC", "OC")
OxCovUptake <- subset(Pots1, Soil == "Oxbow", select=c("Treatment", UptakeCovVar), 
                       na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
OxCovScaleUptake <- as.data.frame(scale(OxCovUptake[,-1]))
OxCovScaleUptake$Treatment <- OxCovUptake$Treatment
OxCovScaleUptakeSplit <- split(OxCovScaleUptake[, -ncol(OxCovScaleUptake)], OxCovScaleUptake$Treatment)
## calculate the covariance matrix for each treatment excluding missing data
UptakeCov_Ox <- lapply(OxCovScaleUptakeSplit, function(x) cov(x, use="pairwise.complete.obs"))
UptakeCovOxWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(UptakeCov_Ox)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(UptakeCov_Ox)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(UptakeCovOxWb, sheet_name)
  writeData(UptakeCovOxWb, sheet=sheet_name, x=UptakeCov_Ox[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(UptakeCovOxWb, "Pots1_Uptake_CovMatrix_Oxbow.xlsx")
# Convert each covariance matrix to a dataframe
UptakeCovOx_df <- lapply(seq_along(UptakeCov_Ox), function(i) {
  cov_mat1h <- as.matrix(UptakeCov_Ox[[i]])
  cov_mat1h <- setNames(cov_mat1h, UptakeCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(UptakeCov_Ox)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
UptakeCovOx_dfAll <- do.call(rbind, UptakeCovOx_df)
UptakeCovOx_dfAll$Var1 <- factor(UptakeCovOx_dfAll$Var1, levels=UptakeCovVar, labels=c("Puptake"="P Uptake", 
         "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", "WaterSolP"="Water Soluble P", 
         "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
UptakeCovOx_dfAll$variable <- factor(UptakeCovOx_dfAll$variable, levels=UptakeCovVar, labels=
         c("Puptake"="P Uptake", "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", 
         "WaterSolP"="Water Soluble P", "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
UptakeCovOx_dfAll$treatment <- factor(UptakeCovOx_dfAll$treatment, 
         levels=c("Control1", "Control2", "CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha", 
                  "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP", 
                  "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate"),
         labels=c("Control 1", "Control 2", "Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", "Manure 50kg P/ha", 
                  "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", "Manure 10t/ha", "Willow 10t/ha", 
                  "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", "Manure 10t/ha & TSP", 
                  "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
write.csv(UptakeCovOx_dfAll, file="Pots1_Oxbow_UptakeCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(UptakeCovOxHeat <- ggplot(UptakeCovOx_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "PuBuGn"), limits=c(-1.2, 3.7), breaks=seq(-1.2, 3.7, by=0.5)) +
    facet_wrap(~ treatment, nrow=5, ncol=3, scales="fixed") +
    geom_text(aes(label=round(Covariance, 3))) +
    geom_text(aes(label=round(Covariance, 3)))+
    theme(legend.title=element_text(size=20, face="bold"), legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=20, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=15, angle=45, hjust=1),
          axis.text.y.left=element_text(size=15),
          panel.spacing.x=unit(1, "cm"))+
    labs(x="", y=""))
ggsave(UptakeCovOxHeat, file="Pots1_UptakeCovOxHeat.jpg", width=20, height=20, dpi=150)

######   P Recovery  #######
RecoveryCovVar <- c("Precovery", "NO3", "NH4", "PO4", "ResinP", "WaterSolP", "TotalP2", "pH", "EC", "OC")
OxCovRecovery <- subset(Pots1, Soil == "Oxbow", select=c("Treatment", RecoveryCovVar), 
                         na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
OxCovScaleRecovery <- as.data.frame(scale(OxCovRecovery[,-1]))
OxCovScaleRecovery$Treatment <- OxCovRecovery$Treatment
OxCovScaleRecoverySplit <- split(OxCovScaleRecovery[, -ncol(OxCovScaleRecovery)], OxCovScaleRecovery$Treatment)
RemoveControls <- c("Control1", "Control2")
OxCovScaleRecoverySplit <- OxCovScaleRecoverySplit[!(names(OxCovScaleRecoverySplit) %in% RemoveControls)]
## calculate the covariance matrix for each treatment excluding missing data
RecoveryCov_Ox <- lapply(OxCovScaleRecoverySplit, function(x) cov(x, use="pairwise.complete.obs"))
RecoveryCovOxWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(RecoveryCov_Ox)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(RecoveryCov_Ox)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(RecoveryCovOxWb, sheet_name)
  writeData(RecoveryCovOxWb, sheet=sheet_name, x=RecoveryCov_Ox[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(RecoveryCovOxWb, "Pots1_Recovery_CovMatrix_Oxbow.xlsx")
# Convert each covariance matrix to a dataframe
RecoveryCovOx_df <- lapply(seq_along(RecoveryCov_Ox), function(i) {
  cov_mat1h <- as.matrix(RecoveryCov_Ox[[i]])
  cov_mat1h <- setNames(cov_mat1h, RecoveryCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(RecoveryCov_Ox)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
RecoveryCovOx_dfAll <- do.call(rbind, RecoveryCovOx_df)
RecoveryCovOx_dfAll$Var1 <- factor(RecoveryCovOx_dfAll$Var1, levels=RecoveryCovVar, labels=
               c("Precovery"="P Recovery", "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", 
                 "WaterSolP"="Water Soluble P", "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
RecoveryCovOx_dfAll$variable <- factor(RecoveryCovOx_dfAll$variable, levels=RecoveryCovVar, labels=
                   c("Precovery"="P Recovery", "NO3"="NO3", "NH4"="NH4", "PO4"="PO4", "ResinP"="Resin P", 
                     "WaterSolP"="Water Soluble P", "TotalP2"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
RecoveryCovOx_dfAll$treatment <- factor(RecoveryCovOx_dfAll$treatment, 
             levels=c("CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha", 
                      "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP", 
                      "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate"),
             labels=c("Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", "Manure 50kg P/ha", 
                      "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", "Manure 10t/ha", "Willow 10t/ha", 
                      "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", "Manure 10t/ha & TSP", 
                      "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
write.csv(RecoveryCovOx_dfAll, file="Pots1_Oxbow_RecoveryCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(RecoveryCovOxHeat <- ggplot(RecoveryCovOx_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "YlOrRd"), limits=c(-1.2, 3.7), breaks=seq(-1.2, 3.7, by=0.5)) +
    facet_wrap(~ treatment, nrow=5, ncol=3, scales="fixed") +
    geom_text(aes(label=round(Covariance, 3)))+
    theme(legend.title=element_text(size=20, face="bold"), legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=20, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=15, angle=45, hjust=1),
          axis.text.y.left=element_text(size=15),
          panel.spacing.x=unit(1, "cm"))+
    labs(x="", y=""))
ggsave(RecoveryCovOxHeat, file="Pots1_RecoveryCovOxHeat.jpg", width=20, height=20, dpi=150)



####   Yield to N & P Recovery  ####
# Set factor and numeric variables
Pots1$Soil <- as.factor(Pots1$Soil)
Pots1$Treatment <- as.factor(Pots1$Treatment)
Pots1$Yield <- as.numeric(Pots1$Yield)
Pots1$`N Recovery` <- as.numeric(Pots1$Nrecovery) # can change variable name, but as unspecified later it didn't matter
Pots1$`P Recovery` <- as.numeric(Pots1$Precovery)
#set up subset minus controls and including only necessary variables - var important for predict later on!
Pots1ContourSub <- subset(Pots1, Treatment != "Control1" & Treatment != "Control2", select = c(Soil, 
                         Treatment, Yield, Nrecovery, Precovery))
View(Pots1ContourSub)
# Set Treatment levels and in order, and relabel
Pots1ContourSub$Treatment <- factor(Pots1ContourSub$Treatment, levels=c("CanolaMeal50kgha","CanolaHull50kgha", 
                   "Manure50kgha", "Willow50kgha", "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", 
                   "Willow10tha", "CanolaMeal10thaTSP", "CanolaHull10thaTSP", "Manure10thaTSP", 
                   "Willow10thaTSP", "TripleSuperPhosphate"),
                labels=c("Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", "Manure 50kg P/ha", 
                  "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", "Manure 10t/ha", "Willow 10t/ha", 
                  "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", "Manure 10t/ha & TSP", 
                  "Willow 10t/ha & TSP", "Phosphorus Fertilizer"))
View(Pots1ContourSub)
Pots1ContourExcl <- na.exclude(Pots1ContourSub) # exclude missing values
View(Pots1ContourExcl)
# run generalised linear mixed model with soil as fixed and random effect
Pots1ContourMod <- glmmTMB(Yield ~ Nrecovery + Precovery + Treatment*Soil + (1|Soil), data = Pots1ContourExcl, 
                           na.action=na.exclude)
summary(Pots1ContourMod)
Anova(Pots1ContourMod) #Treatments and P recovery, as well as treatment*soil significant
#Set up N & P recovery grids per soil
# allows for individual N& P recovery values to be added to the grid instead of combined values
HavNrecovery_grid <- seq(min(Pots1ContourExcl$Nrecovery[Pots1ContourExcl$Soil == "Haverhill"], na.rm = TRUE),
                         max(Pots1ContourExcl$Nrecovery[Pots1ContourExcl$Soil == "Haverhill"], na.rm = TRUE),
                         length.out = 100)
HavPrecovery_grid <- seq(min(Pots1ContourExcl$Precovery[Pots1ContourExcl$Soil == "Haverhill"], na.rm = TRUE),
                         max(Pots1ContourExcl$Precovery[Pots1ContourExcl$Soil == "Haverhill"], na.rm = TRUE),
                         length.out = 100)
OxNrecovery_grid <- seq(min(Pots1ContourExcl$Nrecovery[Pots1ContourExcl$Soil == "Oxbow"], na.rm = TRUE),
                        max(Pots1ContourExcl$Nrecovery[Pots1ContourExcl$Soil == "Oxbow"], na.rm = TRUE),
                        length.out = 100)
OxPrecovery_grid <- seq(min(Pots1ContourExcl$Precovery[Pots1ContourExcl$Soil == "Oxbow"], na.rm = TRUE),
                        max(Pots1ContourExcl$Precovery[Pots1ContourExcl$Soil == "Oxbow"], na.rm = TRUE),
                        length.out = 100)
# Set up expanded grids for each soil then bind before assigning yield
# has to be combined before splitting as predict function requires all variables to be in the dataframe
HavContour_grid <- expand.grid(Soil = "Haverhill", 
                               Treatment = unique(Pots1ContourExcl$Treatment[Pots1ContourExcl$Soil == "Haverhill"]),
                               Nrecovery = HavNrecovery_grid, Precovery = HavPrecovery_grid)
OxContour_grid <- expand.grid(Soil = "Oxbow",
                              Treatment = unique(Pots1ContourExcl$Treatment[Pots1ContourExcl$Soil == "Oxbow"]),
                              Nrecovery = OxNrecovery_grid, Precovery = OxPrecovery_grid)
Pots1Contour_grid <- rbind(HavContour_grid, OxContour_grid)
Pots1Contour_grid$Yield <- predict(Pots1ContourMod, newdata = Pots1Contour_grid)
View(Pots1Contour_grid)
# Split data frames 
Pots1SplitContour <- split(Pots1Contour_grid, Pots1Contour_grid$Soil)
Pots1SplitContour_new <- c("HavContour_df", "OxContour_df") # give new names to split data frames
# Assign the data frames to new variable names
list2env(setNames(Pots1SplitContour, Pots1SplitContour_new), envir = .GlobalEnv)
HavContour_df <- HavContour_df[, -1] # remove soil name from dataset so it doesn't get used in plot
OxContour_df <- OxContour_df[, -1]
View(HavContour_df)
View(OxContour_df)

#develop contour plot -wrapping everything in () makes it execute upon running instead of assigning
(HavContours <- ggplot(HavContour_df, aes(x = Nrecovery, y = Precovery, z = Yield)) +
  geom_raster(aes(fill=Yield)) + #use rastar to make smooth lines instead of tile. Works better than contour or density 
  geom_contour(aes(z=Yield), color='gray30', binwidth = 1) + 
  facet_wrap(~Treatment, nrow = 5) + # specify by treatment, if rows specified, don't specify columns
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu")) +
  labs(x = "% N Recovery", y = "% P Recovery", fill = "Yield (g)") +
  theme(legend.title = element_text(size = 25, face = "bold"),
        legend.key.size = unit(15, "mm"),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 25, face = "bold"),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.x = element_text(vjust = 1),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=30, face="bold"),
        axis.title.y=element_text(size=30, face="bold"),
        panel.spacing = unit(0.5, "cm")))
ggsave(HavContours, file="Pots1_YieldContour_Haverhill.jpg", width=20, height=20, dpi=150)

(OxContours <- ggplot(OxContour_df, aes(x = Nrecovery, y = Precovery, z = Yield)) +
    geom_raster(aes(fill=Yield)) + 
    geom_contour(aes(z=Yield), color='gray30', binwidth = 1) + 
    facet_wrap(~Treatment, nrow = 5) +
    scale_fill_gradientn(colors = brewer.pal(9, "BuPu")) +
    labs(x = "%N Recovery", y = "%P Recovery", fill = "Yield (g)") +
    theme(legend.title = element_text(size = 25, face = "bold"),
          legend.key.size = unit(15, "mm"),
          legend.text = element_text(size = 20),
          strip.text = element_text(size = 25, face = "bold"),
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.x = element_text(vjust = 1),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.x=element_text(size=30, face="bold"),
          axis.title.y=element_text(size=30, face="bold"),
          panel.spacing = unit(0.5, "cm")))
ggsave(OxContours, file="Pots1_YieldContour_Oxbow.jpg", width=20, height=20, dpi=150)


#### Correlation & eigenvalues  ####
## for all treatments and both soils
Pots1EigenMatrix <- Pots1[complete.cases(Pots1), c("Nuptake", "Puptake", "NO3", "NH4", "PO4", "ResinP", "WaterSolP", "TotalP2", 
                                              "pH", "EC", "OC"),]
Pots1EigenCor <- cor(Pots1EigenMatrix)
Pots1EigenPCA <- PCA(Pots1EigenMatrix, scale.unit = TRUE, ncp = length(Pots1EigenMatrix)-1)
Pots1EigenPrin <-princomp(Pots1EigenCor) 
summary(Pots1EigenPrin)
Pots1EigenPrin$loadings[, 1:2]
## Per soil
#Haverhill
HavEigenMatrix <- Pots1[complete.cases(Pots1) & Pots1$Soil == "Haverhill", #sets up matrix per soil type
                        c("Nuptake", "Puptake", "NO3", "NH4", "PO4", "ResinP", #sets relevant variables
                          "WaterSolP", "TotalP2", "pH", "EC", "OC")] #exlude yield
HavEigenMatrix <- 
#have to use above code instead of subset to use complete.cases
HavEigenCor <- cor(HavEigenMatrix) #sets up correlation matrix
HavEigenPCA <- PCA(HavEigenMatrix, scale.unit = TRUE, ncp = length(HavEigenMatrix)-1, ) #do PCA with histograme
HavEigenPrin <-princomp(HavEigenCor) # compute importance of components
summary(HavEigenPrin)
HavEigenPrin$loadings[, 1:2] # print loadings linke dto variables
# model the most important variables
# variables for Haverhill: Nuptake, NO3, NH4, PO4, ResinP & WaterSolP
HavSubEig <- Pots1[Pots1$Soil == "Haverhill", c("Yield", "Nuptake", "NO3", "NH4", "PO4", "ResinP", "WaterSolP")] 
colnames(HavSubEig) <- c("Yield", "N Uptake", "NO3", "NH4", "PO4", "Resin P", "Water Soluble P")
View(HavSubEig)
HavModEig <- lm(Yield ~ `N Uptake`+NO3+NH4+PO4+`Resin P`+`Water Soluble P`, data = HavSubEig)
summary(HavModEig)
# scatterplot of the most prominant variables against yield
HavEigScatter <- function(data, model, targetColumn = 'Yield') {
  formula_vars <- all.vars(model$terms)
  plot_vars <- intersect(formula_vars, colnames(data))
  plot_vars <- setdiff(plot_vars, targetColumn)  # Remove targetColumn from plotting variables
  data_plot <- data[, c(plot_vars, targetColumn)]
  d <- reshape2::melt(data_plot, id.vars = targetColumn)
  ggplot(d, aes(value, !!rlang::sym(targetColumn))) + 
    geom_point() + 
    facet_wrap(~ variable, scales = 'free') +
    labs(x = "Variable values", y = "Yield (g) - Haverhill")
}
HavEigScatter(HavSubEig, HavModEig)
ggsave(HavEigScatter(HavSubEig, HavModEig), file="Pots1_EigenScatter_Haverhill.jpg", width=10, height=10, dpi=150)

## Oxbow
OxEigenMatrix <- Pots1[complete.cases(Pots1) & Pots1$Soil == "Oxbow", 
                        c("Nuptake", "Puptake", "NO3", "NH4", "PO4", "ResinP", 
                          "WaterSolP", "TotalP2", "pH", "EC", "OC")] 
OxEigenCor <- cor(OxEigenMatrix)
OxEigenPCA <- PCA(OxEigenMatrix, scale.unit = TRUE, ncp = length(OxEigenMatrix)-1, )
OxEigenPrin <-princomp(OxEigenCor) 
summary(OxEigenPrin)
OxEigenPrin$loadings[, 1:2] 
# model the most important variables
# variables for Haverhill: Nuptake, Puptake, NO3, NH4, WaterSolP, EC
OxSubEig <- Pots1[Pots1$Soil == "Oxbow", c("Yield", "Nuptake", "Puptake", "NO3", "NH4", "WaterSolP", "EC")] 
colnames(OxSubEig) <- c("Yield", "N Uptake", "P Uptake", "NO3", "NH4", "Water Soluble P", "Electrical Conductivity")
View(OxSubEig)
OxModEig <- lm(Yield ~ `N Uptake`+`P Uptake`+NO3+NH4+`Water Soluble P`+`Electrical Conductivity`, data = OxSubEig)
summary(OxModEig)
# scatterplot of the most prominant variables against yield
OxEigScatter <- function(data, model, targetColumn = 'Yield') {
  formula_vars <- all.vars(model$terms)
  plot_vars <- intersect(formula_vars, colnames(data))
  plot_vars <- setdiff(plot_vars, targetColumn)  # Remove targetColumn from plotting variables
  data_plot <- data[, c(plot_vars, targetColumn)]
  d <- reshape2::melt(data_plot, id.vars = targetColumn)
  ggplot(d, aes(value, !!rlang::sym(targetColumn))) + 
    geom_point() + 
    facet_wrap(~ variable, scales = 'free') +
    labs(x = "Variable values", y = "Yield (g) - Oxbow")
}
OxEigScatter(OxSubEig, OxModEig)
ggsave(OxEigScatter(OxSubEig, OxModEig), file="Pots1_EigenScatter_Oxbow.jpg", width=10, height=10, dpi=150)




####  Extract ANOVA tables  ####
# List models and ANOVA table
Mod1b <- glmmTMB(Yield~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
YieldAN <- glmmTMB:::Anova.glmmTMB(Mod1b, type="III")
# Add row names (treatment, soil, etc.) in excel sheet
YieldAN$RowNames <- row.names(YieldAN)
rownames(YieldAN) <- NULL

Mod3b<-lmer(log(Nuptake)~Treatment*Soil+(1|Soil), data=Pots1, na.action=na.exclude, 
            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
NupAn <- anova(Mod3b)
NupAn$RowNames <- row.names(NupAn)
rownames(NupAn) <- NULL

Mod4g <- glmmTMB(Nrecovery~Treatment*Soil+(1|Soil), data=Nrec_out, family=gaussian(), na.action=na.exclude)
NrecAN <- glmmTMB:::Anova.glmmTMB(Mod4g, type="III")
NrecAN$RowNames <- row.names(NrecAN)
rownames(NrecAN) <- NULL

Mod5c <- glmmTMB(Puptake~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
PupAN <- glmmTMB:::Anova.glmmTMB(Mod5c, type="III")
PupAN$RowNames <- row.names(PupAN)
rownames(PupAN) <- NULL

Mod6d <- glmmTMB(Precovery~Treatment*Soil+(1|Soil), data=Prec, family=gaussian(), na.action=na.exclude)
PrecAN <- glmmTMB:::Anova.glmmTMB(Mod6d, type="III")
PrecAN$RowNames <- row.names(PrecAN)
rownames(PrecAN) <- NULL

Mod7b <- glmmTMB(NO3~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
NO3AN <- glmmTMB:::Anova.glmmTMB(Mod7b, type="III")
NO3AN$RowNames <- row.names(NO3AN)
rownames(NO3AN) <- NULL

Mod8b <- glmmTMB(NH4~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
NH4AN <- glmmTMB:::Anova.glmmTMB(Mod8b, type="III")
NH4AN$RowNames <- row.names(NH4AN)
rownames(NH4AN) <- NULL

Mod9h <- glmmTMB(log(PO4)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
PO4AN <- glmmTMB:::Anova.glmmTMB(Mod9h, type="III")
PO4AN$RowNames <- row.names(PO4AN)
rownames(PO4AN) <- NULL

Mod10b <- glmmTMB(log(ResinP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
ResPAN <- glmmTMB:::Anova.glmmTMB(Mod10b, type="III")
ResPAN$RowNames <- row.names(ResPAN)
rownames(ResPAN) <- NULL

Mod11b <- glmmTMB(log(WaterSolP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
WSPAN <- glmmTMB:::Anova.glmmTMB(Mod11b, type="III")
WSPAN$RowNames <- row.names(WSPAN)
rownames(WSPAN) <- NULL

Mod12b <- lmer(TotalP2~Treatment*Soil+(1|Soil), data=Pots1, na.action=na.exclude) #didn't converge properly
TotPAN <- anova(Mod12b) 
TotPAN$RowNames <- row.names(TotPAN)
rownames(TotPAN) <- NULL

Mod13c <- glmer(pH~Treatment*Soil+(1|Soil),data=Pots1, family=gaussian(link="log")) #singularity issues
pHAN <- anova(Mod13c)
pHAN$RowNames <- row.names(pHAN)
rownames(pHAN) <- NULL

Mod14b <- glmmTMB(log(EC)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
ecAN <- glmmTMB:::Anova.glmmTMB(Mod14b, type="III")
ecAN$RowNames <- row.names(ecAN)
rownames(ecAN) <- NULL

Mod15f <- glmmTMB(OC~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
ocAN <- glmmTMB:::Anova.glmmTMB(Mod15f, type="III")
ocAN$RowNames <- row.names(ocAN)
rownames(ocAN) <- NULL

# Make list of anova table functions
Pots1ANOVAtables <- list(YieldAN, NupAn, NrecAN, PupAN, PrecAN, NO3AN, NH4AN, PO4AN, ResPAN, WSPAN, TotPAN, pHAN,
                         ecAN, ocAN)
# Rename worksheets
names(Pots1ANOVAtables) <- c("Yield", "Nuptake", "Nrecovery", "Puptake","Precovery", "NO3", "NH4", "PO4", 
                             "ResinP", "WaterSolP", "TotalP", "pH", "EC", "OC")
# Write anova tables to excel
write_xlsx(Pots1ANOVAtables, path = "Pots1ANOVAtables.xlsx")



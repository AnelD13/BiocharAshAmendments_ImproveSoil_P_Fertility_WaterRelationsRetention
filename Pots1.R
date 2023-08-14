##### Loading data in to R ####
Pots1<-read.csv("Pots1.csv", fileEncoding="UTF-8-BOM")
View(Pots1)
plot(Pots1$Drywt)

#Loading libraries
library(lme4)
library(nlme)
library(lmerTest)
library(doBy)
library(ggplot2)
library(ggpattern)
library(plotrix)
library(car)
library(afex)
library(onewaytests)
library(multcomp)
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


##### Summary and ordering of data   ####
#Check for missing values in a specific field
missing <- colSums(is.na(Pots1[,]))
print(missing)

#Change columns in a dataframe to factors/categorical values, useful for treatments and soils, str displays 
#the structure of R objects or the contents of a list
#treatment order specifies the order in which treatments will appear 
Trt_order <- c("Control1", "Control2", "CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha",
               "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha",
               "CanolaHull10thaTSP", "CanolaMeal10thaTSP", "Manure10thaTSP", "Willow10thaTSP", 
               "TripleSuperPhosphate")
Soil_order <- c("Haverhill", "Oxbow")
Pots1$Treatment <- factor(Pots1$Treatment, levels = Trt_order)
Pots1$Soil <- factor(Pots1$Soil, levels = Soil_order)
summary(Pots1)
str(Pots1) #displays the structure of the object
View(Pots1) #view the object in a separate window (e.g. as a table)

# Summary data (means, SD, etc.) for each treatment and variable
Pots1Mean <- summary_by(.~Soil+Treatment, data=Pots1, FUN=mean)
Pots1SD <- summary_by(.~Soil+Treatment, data=Pots1, FUN=sd)



#####   Check for outliers   ####
PotsRaw<-read.csv("Pots1raw.csv", fileEncoding="UTF-8-BOM")
##Drywt
ggplot(PotsRaw, aes(x = Treatment, y = Drywt, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Drywt")
ggsave("OutliersDrywt.jpg", width = 30, height = 30, dpi = 600)
##Total N
ggplot(PotsRaw, aes(x = Treatment, y = TotalN, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Total N")
ggsave("OutliersTotalN.jpg", width = 30, height = 30, dpi = 600)
##N Uptake
ggplot(PotsRaw, aes(x = Treatment, y = Nuptake, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "N uptake")
ggsave("OutliersNuptake.jpg", width = 30, height = 30, dpi = 600)
##N Recovery
ggplot(PotsRaw, aes(x = Treatment, y = Nrecovery, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "N Recovery")
ggsave("OutliersNrecovery.jpg", width = 30, height = 30, dpi = 600)
##Total P1
ggplot(PotsRaw, aes(x = Treatment, y = TotalP1, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Total P Plants")
ggsave("OutliersTotalPplants.jpg", width = 30, height = 30, dpi = 600)
##P Uptake
ggplot(PotsRaw, aes(x = Treatment, y = Puptake, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "P uptake")
ggsave("OutliersPuptake.jpg", width = 30, height = 30, dpi = 600)
##P Recovery
ggplot(PotsRaw, aes(x = Treatment, y = Precovery, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "P recovery")
ggsave("OutliersPrecovery.jpg", width = 30, height = 30, dpi = 600)
##NO3	
ggplot(PotsRaw, aes(x = Treatment, y = NO3, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "NO3")
ggsave("OutliersNO3.jpg", width = 30, height = 30, dpi = 600)
##NH4	
ggplot(PotsRaw, aes(x = Treatment, y = NH4, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "NH4")
ggsave("OutliersNH4.jpg", width = 30, height = 30, dpi = 600)
##PO4	
ggplot(PotsRaw, aes(x = Treatment, y = PO4, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "PO4")
ggsave("OutliersPO4.jpg", width = 30, height = 30, dpi = 600)
## Resin P
ggplot(PotsRaw, aes(x = Treatment, y = ResinP, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "ResinP")
ggsave("OutliersResinP.jpg", width = 30, height = 30, dpi = 600)
##WaterSolP	
ggplot(PotsRaw, aes(x = Treatment, y = WaterSolP, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Water Soluble P")
ggsave("OutliersWaterSolP.jpg", width = 30, height = 30, dpi = 600)
##TotalP2	
ggplot(PotsRaw, aes(x = Treatment, y = TotalP2, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Total P Soil")
ggsave("OutliersTotalPSoil.jpg", width = 30, height = 30, dpi = 600)
##pH	
ggplot(PotsRaw, aes(x = Treatment, y = pH, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "pH")
ggsave("OutlierspH.jpg", width = 30, height = 30, dpi = 600)
##EC	
ggplot(PotsRaw, aes(x = Treatment, y = EC, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Electrical Conductivity")
ggsave("OutliersEC.jpg", width = 30, height = 30, dpi = 600)
##OC
ggplot(PotsRaw, aes(x = Treatment, y = OC, fill=Soil)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Soil+Treatment, scales = "free") +
  labs(x = "Treatment", y = "Organic Carbon %")
ggsave("OutliersOC.jpg", width = 30, height = 30, dpi = 600)





#Modelling data using simple linear model for CRD, no random intercept possible for my project & mixed model thus not possible
#look at glmr - equivalent of GLMMIX
#ANOVA model - this describes the response variable is influenced by the explanatory variables
####P-VALUE INTERPRETATION: p >0.05, no significant difference, fail to reject null hypothesis
#p-value for intercept <0.05
#Plots to check residuals to be equally centered around 0, vertical lines are ok if centered around 0


##### Dry Weight ####
#Running models
Mod1<-lm(Drywt~Treatment+Soil,data=Pots1) #using a linear model as the best fit
anova(Mod1) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod1)
shapiro.test(resid(Mod1)) ##S-W p value = 0.4382; Data is considered normal
leveneTest(Drywt~Treatment, data = Pots1) #check for sig dif between variance, Varian is equal P=0.04259
#the residuals plot, qqnorm and shapiro.test all checks normality
plot(fitted(Mod1),resid(Mod1),pch=16) #plots the residuals vs the fitted values from a linear regression model
qqnorm(resid(Mod1))
qqline(resid(Mod1))
#Mod1a
Mod1a <- aov(Drywt~Treatment*Soil, data=Pots1) #Two-way anova for Dry weight - another way to do the same
anova(Mod1a)
summary(Mod1a)
leveneTest(Drywt ~ Treatment, data = Pots1)
shapiro.test(resid(Mod1a))
#The two-way anova appears to be a better fit for the dry weight data

#extract the anova results in a tidy format
Mod1_tidy <- tidy(Mod1a)
View(Mod1_tidy)

#Run emmeans (the new lsmeans) - it creates a Tukey HSD pairwise comparison
Mod1em <- emmeans(Mod1a,~Soil+Treatment) #a combined table by soil, but comparing all treatments per soil in one
Mod1cld <- cld(Mod1em, Letters = letters) #use Compact Letter Display (CLD) with a means test to show sig dif as letters
View(Mod1cld)
#emmeans for each soil separately - # cld use directly in ggplot - make sure labels are correct in ggplot
Mod1em_split <- emmeans(Mod1a,~Treatment|Soil, subset=(Pots1$Drywt))
Mod1cld_split <- cld(Mod1em_split, Letters = letters, by="Soil")
View(Mod1cld_split)


##Developing visualizations
#set up data frame with the means and SE of Dry weight !!!!   Does NOT include letters - do not use !!!!!
#Drywt_df <- Pots1 %>%
#  select(Soil, Treatment, Drywt) %>% # select only relevant columns
#  group_by(Soil, Treatment) %>% # group by soil and treatment
#  summarise(Mean = mean(Drywt, na.rm=TRUE), # calculate the mean dry weight for each group
#            SE = sd(Drywt, na.rm=TRUE)/sqrt(length(Drywt)), # calculate the standard error for each group
#            .groups = 'drop')
#View(Drywt_df)
#write.csv(Drywt_df,"Drywt.csv")

# Plot means with error bars and CLD
# par(mar=c(5,6,4,2)+0.1) #c(bottom, left, top, right) + 0.1 lines
#plot all dry weights onto one graph
#ggplot(Mod1cld_split, aes(x = Treatment, y = emmean, fill = Soil)) +
#  geom_bar(stat = "identity", position=position_dodge2(padding=0.2)) +
#  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
#                width = .2, position = position_dodge(width = 0.9)) +
#  geom_text(aes(label=.group), nudge_y=1.5, nudge_x=0.1, vjust = -0.5, size=2) + 
#  labs(title = "Dry Weight by Treatment and Soil", x = "Treatment", y = "Dry Weight (ug/g)") +
#  theme_bw() +
#  theme(plot.title = element_text(size = 12))+
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8)) +
#  scale_fill_manual(values = c("blue", "red"), breaks = c("Haverhill","Oxbow"), labels = c("Haverhill","Oxbow"))
##select specific treatments and use those in the graph - repeat for all subsets
#Plotting dry weight using constant P and variable biochar rates
Drywt_trtVar <- c("Control1", "Control2","CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha",
                  "TripleSuperPhosphate")
Drywt_subVar <- Mod1cld_split %>%
  filter(Treatment %in% Drywt_trtVar)
ggplot(Drywt_subVar, aes(x = Treatment, y = emmean, pattern = Soil))+
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=6, position = position_dodge2(width = 0.9), vjust=-0.5) + 
  labs(y = "Biomass yield (g) for chars at 50kg P/ha")+
  scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ggsave("Pots1_Biomass_50kgPha.jpg", width = 12, height = 8, dpi = 600)
#Plotting constant biochar rates with var P rates
Drywt_charCon <- c("Control1", "Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                   "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                   "Willow10tha", "Willow10thaTSP")
Drywt_subCon <- Mod1cld_split %>%
  filter(Treatment %in% Drywt_charCon)
ggplot(Drywt_subCon, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=6, position = position_dodge2(width = 0.9), vjust=-0.5) + 
  labs(x = "Treatments", y = "Biomass yield (g) for chars at 10t/ha") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ggsave("Pots1_Biomass_10tha.jpg", width = 12, height = 8, dpi = 600)



#### Total N (Mod2) - only used to calculate N uptake and recovery, don't analyse


##### N uptake ######
#check kurtosis and skewness - the data is not too moderate to highly skewed with moderate/high kurtosis
Nup_Mean <- summary_by(Nuptake~Soil+Treatment, data=Pots1, FUN=mean) # calculate means of N recovery
Nup_Mean <- as.numeric(Nup_Mean$Nuptake)
Nup_skew <- skewness(Nup_Mean,na.rm=TRUE)
Nup_kur <- kurtosis(Nup_Mean,na.rm=TRUE)
cat("Skewness:", Nup_skew, "\n") ## data is moderately skewed @ -1.446
cat("Kurtosis:", Nup_kur, "\n") ## data has very high kurtosis @ 2.310
#check distribution normality
shapiro.test(Pots1$Nuptake) #  p=3.599e-07
hist(Pots1$Nuptake) # slight right  skew
leveneTest(Nuptake ~ Treatment, data = Pots1)  # 0.0001608
qqnorm(Pots1$Nuptake) # very heavy left tail
qqline(Pots1$Nuptake)
# transform
shapiro.test(log(Pots1$Nuptake)) #p=2.008e-15
hist(log(Pots1$Nuptake))  # heavy right skew
leveneTest(log(Nuptake) ~ Treatment, data = Pots1) # p=0.0005804
shapiro.test(sqrt(Pots1$Nuptake)) #p=1.471e-11 
hist(sqrt(Pots1$Nuptake)) # heavy right skew
leveneTest(sqrt(Nuptake) ~ Treatment, data = Pots1) # p=0.0006209
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
rsq(Mod3a1) # r sq = 0.750
summary(Mod3a1)$adj.r.squared # check adjusted R squared value: 0.639
summary(Mod3a1)
shapiro.test(resid(Mod3a1)) ##S-W p value 0.0007889; Data is considered non-normal and needs to be transformed
plot(fitted(Mod3a1),resid(Mod3a1),pch=16) # not normally distributed
qqnorm(resid(Mod3a1)) #not normally distributed
qqline(resid(Mod3a1)) 
#Mod3a - N uptake (outliers removed)
Mod3a <- lmer(Nuptake~Treatment*Soil+(1|Soil), data=Pots1, na.action = na.exclude,
              control=lmerControl(optCtrl = list(maxfun = 1000000)))
rsq(Mod3a) # adjusted R squared: 0.762
anova(Mod3a)
summary(Mod3a)
shapiro.test(resid(Mod3a)) ##S-W p value 0.00544; Data is considered non-normal and needs to be transformed
plot(fitted(Mod3a),resid(Mod3a),pch=16) # cluster at right end is less severely compacted than for other models
qqnorm(resid(Mod3a)) #not normally distributed
qqline(resid(Mod3a)) 
# Log-transforming Total N and checking normality of transformed data
Mod3b<-lmer(log(Nuptake)~Treatment*Soil+(1|Soil), data=Pots1, na.action = na.exclude, 
            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
rsq(Mod3b) # 0.842
anova(Mod3b)
shapiro.test(resid(Mod3b)) # p=2.081e-08
plot(fitted(Mod3b),resid(Mod3b),pch=16) #more clustered at the right end
qqnorm(resid(Mod3b)) # heavy tails
qqline(resid(Mod3b))
# Sqrt transformation
Mod3c<- lmer(sqrt(Nuptake)~Treatment*Soil+(1|Soil),data=Pots1, na.action = na.exclude)
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
Mod3d <- glm(Nuptake ~ Treatment*Soil, family = gaussian, data = Pots1)
rsq(Mod3d) #0.750
anova(Mod3d)
summary(Mod3d)
shapiro.test(resid(Mod3d))  # p=0.000881
plot(fitted(Mod3d),resid(Mod3d),pch=16) # cluster at right end is less severely compacted than for other models
qqnorm(resid(Mod3d))  #medium-heavy tails
qqline(resid(Mod3d))
#using "gamma" distribution in glmer test
Mod3e <- glmer(Nuptake~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log"))
#rsq.glmm(Mod3e)
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
Mod3g <- lmer(Nuptake_YJ ~ Treatment*Soil + (1|Soil), data=Pots1, control=lmerControl(optCtrl = list(maxfun = 100000)))
rsq(Mod3g) # 0.807
anova(Mod3g)
summary(Mod3g)
leveneTest(Nuptake_YJ~Treatment*Soil, data=Pots1)  # 3.842e-09
shapiro.test(resid(Mod3g)) # p=4.088e-05
plot(fitted(Mod3g),resid(Mod3g),pch=16) # heavy right cluster
qqnorm(resid(Mod3g)) # medium-heavy tails
qqline(resid(Mod3g))
# weighted lm model
Mod3var <- tapply(log(Pots1$Nuptake), Pots1$Treatment, var, na.rm=TRUE)
weightsP1Nuptake <- 1 / Mod3var
weightsP1Nuptake_full <- rep(weightsP1Nuptake, each = length(Pots1$Nuptake) / length(weightsP1Nuptake))
Mod3h <- lm(Nuptake ~ Treatment*Soil, data=Pots1, weights=weightsP1Nuptake_full) 
anova(Mod3h)
summary(Mod3h)
hist(resid(Mod3h)) # flattened
shapiro.test(resid(Mod3h))  # 0.0008813
plot(fitted(Mod3h),resid(Mod3h),pch=16)   # clusters forming
qqnorm(resid(Mod3h)) # slight-medium tails
qqline(resid(Mod3h))
rsq(Mod3h) # 0.672

#Comparing models using various options:
## AIC & BIC indicate that Mod3b is the best fit.
# Create a list of the models
N_modlist <- list(Mod3, Mod3a1, Mod3a, Mod3b, Mod3c, Mod3d, Mod3e, Mod3g, Mod3h)
AIC_values <- sapply(N_modlist, AIC)
BIC_values <- sapply(N_modlist, BIC)
N_AB <- data.frame(Model=c("Mod3", "Mod3a1", "Mod3a", "Mod3b", "Mod3c", "Mod3d", "Mod3e", "Mod3g", "MOd3h"),
                   AIC_values, BIC_values)
print(N_AB)
  #   Model AIC_values BIC_values
  #1   Mod3   912.0315   997.3928
  #2 Mod3a1   912.0315   997.3928
  #3  Mod3a   760.3687   848.4836
  #4  Mod3b   119.6651   207.7800
  #5  Mod3c   307.3099   395.4247
  #6  Mod3d   912.0315   997.3928
  #7  Mod3e   971.5901  1059.7050
  #8  Mod3g   768.5644   856.6793
  #9  MOd3h   964.1322  1049.4935
# R squared values for Mod3 (aov) = 0.866; Mod3a1 (lm) = 0.750; Mod3a (lmer) =0.762; Mod3b (lmerlog) = 0.841
# Mod3c (lmersqrt) = 0.8086; Mod3d (glm) = 0.750;Mod3g (glmr) = 0.807, MOd3h (weightedlm) = 0.672

#Mod3b chosen as best fit
#emmeans 
Mod3em <- emmeans(Mod3e,~Treatment|Soil, data=Pots1)
Mod3cld <- cld(Mod3em, Letters = letters, by="Soil", type="response") 
Mod3cld <- Mod3cld %>% rename(emmean = "response")
View(Mod3cld)

## Visualizations
Nuptake_trtVar <- c("Control1", "Control2","CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha",
                  "TripleSuperPhosphate")
Nuptake_subVar <- Mod3cld %>%
  filter(Treatment %in% Nuptake_trtVar)
ggplot(Nuptake_subVar, aes(x = Treatment, y = emmean, pattern = Soil))+
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=8, position = position_dodge2(width = 0.9), vjust=-1) + 
  labs(x = "Treatments", y = "N uptake (ug) for chars at 50kg p/ha")+
  scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 20))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=20, face="bold", colour="black"),
        axis.title.x = element_text(size = 22, face="bold"), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Nuptake_50kgPha.jpg", width = 18, height = 18, dpi = 500)
#Plotting constant biochar rates with var P rates
Nuptake_charCon <- c("Control1", "Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                   "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                   "Willow10tha", "Willow10thaTSP")
Nuptake_subCon <- Mod3cld %>%
  filter(Treatment %in% Nuptake_charCon)
ggplot(Nuptake_subCon, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=8, position = position_dodge2(width = 0.9), vjust=-1) + 
  labs(x = "Treatments", y = "N uptake (ug) for chars at 10t/ha") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=20, face="bold", colour="black"),
        axis.title.x = element_text(size = 22, face="bold"), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Nuptake_10tha.jpg", width = 21, height = 21, dpi = 500)






##### N recovery #######
Nrec_Mean <- summary_by(Nrecovery~Soil+Treatment, data=Pots1, FUN=mean) # calculate means of N recovery
Nrec_Mean <- as.numeric(Nrec_Mean$Nrecovery)
Nrec_skew <- skewness(Nrec_Mean,na.rm=TRUE)
Nrec_kur <- kurtosis(Nrec_Mean,na.rm=TRUE)
cat("Skewness:", Nrec_skew, "\n") ## data is mildly skewed @ 0.797
cat("Kurtosis:", Nrec_kur, "\n") ## data has very low kurtosis @ -0.231
hist(Pots1$Nrecovery)
leveneTest(Nrecovery ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 6.39e-12
### Nrecovery has missing values for Control1 - the subset removes these values for the glmer model
Nrec_out <- Pots1[complete.cases(Pots1$Nrecovery),] #set up a subset removing the missing data only from column Nrecovery
View(Nrec_out)
leveneTest(Nrecovery ~ Treatment*Soil, data = Nrec_out)  # 6.39e-12; results are the same as for the whole dataset  
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
leveneTest(log(Nrecovery) ~ Treatment*Soil, data = Nrec_out) #log transformation improved variance: p=1.507e-06
leveneTest(log10(Nrecovery) ~ Treatment*Soil, data = Nrec_out) # variances improved compared to raw data p=1.507e-06
leveneTest(sqrt(Nrecovery) ~ Treatment*Soil, data = Nrec_out) # not much improvement on variances p=8.751e-12
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
Mod4b<-lmer(Nrecovery~Treatment*Soil+(1|Soil),data=Pots1)
anova(Mod4b)
summary(Mod4b)
shapiro.test(resid(Mod4b))  # p=2.046e-06
plot(fitted(Mod4b),resid(Mod4b),pch=16) #looks relatively normal
qqnorm(resid(Mod4b)) #long tails
qqline(resid(Mod4b))
#Mod4c - log and base log 10 transform data for N recovery
Mod4c<-lmer(log(Nrecovery)~Treatment*Soil+(1|Soil),data=Pots1)
rsq(Mod4c) # adjusted R squared: 0.6234
anova(Mod4c)
summary(Mod4c)
shapiro.test(resid(Mod4c))  # p=1.297e-14
plot(fitted(Mod4c),resid(Mod4c),pch=16)  # data clustering
qqnorm(resid(Mod4c)) #most data fit better but tails are exaggerated
qqline(resid(Mod4c))
#Mod4d 
Mod4d<-lmer(log10(Nrecovery)~Treatment*Soil+(1|Soil),data=Pots1)
ref.grid(Mod4d)
Mod4doptim <- lmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun=1e9))
Mod4dOp <- update(Mod4d, control=Mod4doptim)
rsq(Mod4d) #0.62
anova(Mod4d)
summary(Mod4d)
shapiro.test(resid(Mod4d)) # p= 1.297e-14
#Mod4e - sqrt transformation - did not work, data non-normally distributed and has unequal variances
Mod4e <- lmer(sqrt(Nrecovery)~Treatment*Soil+(1|Soil),data=Pots1)
anova(Mod4e)
summary(Mod4e)
shapiro.test(resid(Mod4e)) # = 8.829e-09
#Mod4f using "gamma" distribution in glmer test - did not work, data less normally distributed with unequal variance
Mod4f <- glmer(Nrecovery~Treatment*Soil+(1|Soil),data=Nrec_out,family=Gamma(link="log"))
anova(Mod4f)
summary(Mod4f)
shapiro.test(resid(Mod4f)) # p=2.471e-13
bf.test(Nrecovery~Treatment, data=Pots1) # variances are significantly different 
plot(fitted(Mod4f),resid(Mod4f),pch=16)  # data clustering at the top of the plot
qqnorm(resid(Mod4f)) #data clustering at the top of the plot
qqline(resid(Mod4f))


##since the S-W values for all models show non-normally distributed data, look at other options:
#Shapiro-Wilk values for the various models showed Mod4 to have the most normal data
#Levene test showed Mod4c&d to have the most equal variances of all the models, but still unequal p=1.507e-06
#AIC and BIC values - this indicated that Mod4d (lmer with log10 transform) was the best fit
# Create a list of the models
Nrec_modlist <- list(Mod4, Mod4_aovlog, Mod4a, Mod4b, Mod4c, Mod4d, Mod4e, Mod4f)
AIC_Nrec <- sapply(Nrec_modlist, AIC)
BIC_Nrec <- sapply(Nrec_modlist, BIC)
NrecAB <- data.frame(Model=c("Mod4", "Mod4_aovlog", "Mod4a", "Mod4b", "Mod4c", "Mod4d", "Mod4e", "Mod4f"),AIC_Nrec, BIC_Nrec)
print(NrecAB)
#Model   AIC_Nrec BIC_Nrec
#1        Mod4 656.4140 734.1958
#2 Mod4_aovlog 132.5385 210.3203
#3       Mod4a 656.4140 734.1958
#4       Mod4b 564.9435 645.4075
#5       Mod4c 176.8876 257.3515
#6       Mod4d  43.4424 123.9063
#7       Mod4e 239.5915 320.0554
#8       Mod4f 701.5753 782.0392


# R squared values - these were check for Mod4 (0.753), Mod4a (0.682), Mod4c (0.6234) & Mod4d (0.62) which showed
# Mod 4 to be best)
# considering that none of the models fit very well, Mod4c was chosen as it had good AIC, decent R squared and 
# the transformation in the model improved the unequal variances significantly. Also no problem fitting it to emmeans

#run emmeans on Mod4 - showed significant differences
Mod4em <- emmeans(Mod4c,~Treatment|Soil, subset=(Nrec$Nrecovery))
Mod4em_cld <- cld(Mod4em, Letters = letters, by="Soil")
View(Mod4em_cld)

## Visualizations
par(mar=c(5,6,4,2)+0.1) #c(bottom, left, top, right) + 0.1 lines
##select specific treatments and use those in the graph - repeat for all subsets
#Plotting dry weight using constant P and variable biochar rates
Nrec_trtVar <- c("Control2","CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha", "TripleSuperPhosphate")
Nrec_subVar <- Mod4em_cld %>%
  filter(Treatment %in% Nrec_trtVar)
ggplot(Nrec_subVar, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=8, position = position_dodge2(width = 0.9), vjust=-7) +
  labs(x = "Treatments", y = "Nitrogen Recovery (%)") +
  scale_x_discrete(labels = c("Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_text(size = 22, face="bold"), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ggsave("Pots1_Nrec_50kg_ha.jpg", width = 20, height = 20, dpi = 600)
#Plotting constant biochar rates with var P rates
Nrec_charCon <- c("Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                   "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                   "Willow10tha", "Willow10thaTSP")
Nrec_subCon <- Mod4em_cld %>%
  filter(Treatment %in% Nrec_charCon)
ggplot(Nrec_subCon, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=8, position = position_dodge2(width = 0.9), vjust=-7) +
  labs(x = "Treatments", y = "Nitrogen Recovery (%)") +
  scale_x_discrete(labels = c("Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_text(size = 22, face="bold"), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Nrec_10t_ha.jpg", width = 20, height = 20, dpi = 500)





##### Total P ######  No need to analyze - only used for P uptake & P recovery




##### P uptake ######## 
## Issues with identical SE values - investigate
Pup_Mean <- summary_by(Puptake~Soil+Treatment, data=Pots1, FUN=mean) 
Pup_Mean <- as.numeric(Pup_Mean$Puptake)
Pup_skew <- skewness(Pup_Mean,na.rm=TRUE)
Pup_kur <- kurtosis(Pup_Mean,na.rm=TRUE)
cat("Skewness:", Pup_skew, "\n") ## data is not skewed @ 0.035
cat("Kurtosis:", Pup_kur, "\n") ## data has low/moderate kurtosis @ -1.115
shapiro.test(Pots1$Puptake)  # p=0.001144
hist(Pots1$Puptake) # left skewed
leveneTest(Puptake ~ Treatment*Soil, data = Pots1) # variances are equal; p=0.1601
#Mod5 - P uptake
Mod5 <- lm(Puptake~Soil*Treatment,data=Pots1) 
rsq(Mod5) # 0.9428224
anova(Mod5)
summary(Mod5)
shapiro.test(resid(Mod5))  ##S-W p value = 0.1046; Data is considered normal
plot(fitted(Mod4),resid(Mod4),pch=16) # slightly clustered to the left
qqnorm(resid(Mod4)) #long tails
qqline(resid(Mod4))
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
Mod5b <- lmrob(Puptake ~ Treatment * Soil, data = Pots1, method = "MM")

#AIC and BIC values - the models were equal
Pup_modlist <- list(Mod5, Mod5a, Mod5b)
AIC_values <- sapply(Pup_modlist, AIC)
BIC_values <- sapply(Pup_modlist, BIC)
PupAB <- data.frame(Model=c("Mod5", "Mod5a", "Mod5b"), AIC_values, BIC_values)
print(PupAB)
#Model AIC_values BIC_values
#1  Mod5   2130.762   2216.389
#2 Mod5a   2130.762   2216.389

#Mod5a chosen as rsq is higher (0.99)
Mod5em <- emmeans(Mod5a,~Treatment|Soil)
Mod5em_cld <- cld(Mod5em, Letters = letters, by="Soil", type="response")
View(Mod5em_cld)


## Visualizations
par(mar=c(5,6,4,2)+0.1) #c(bottom, left, top, right) + 0.1 lines
Pup_trtVar <- c("Control1", "Control2","CanolaMeal50kgha","CanolaHull50kgha","Manure50kgha","Willow50kgha",
                "TripleSuperPhosphate")
Pup_subVar <- Mod5em_cld %>% filter(Treatment %in% Pup_trtVar)
ggplot(Pup_subVar, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=8, position = position_dodge2(width = 0.9), vjust=-1) +
  labs(x = "Treatments", y = "Phosphorus uptake (mg) for 50kg P/ha treatments") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_text(size = 20, face="bold"), axis.title.y = element_text(size = 20, face="bold")) +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Puptake_50kg_ha.jpg", width = 14, height = 12, dpi = 500)
#Plotting constant biochar rates with var P rates
Pup_charCon <- c("Control1", "Control2","CanolaHull10tha", "CanolaHull10thaTSP", "CanolaMeal10tha", 
                  "CanolaMeal10thaTSP", "Manure10tha", "Manure10thaTSP","TripleSuperPhosphate", 
                  "Willow10tha", "Willow10thaTSP")
Pup_subCon <- Mod5em_cld %>% filter(Treatment %in% Pup_charCon)
ggplot(Pup_subCon, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=8, position = position_dodge2(width = 0.9), vjust=-1) +
  labs(x = "Treatments", y = "Phosphorus uptake (mg) for 10t/ha treatments") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Canola Hull", "Manure", "Willow",
                              "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_text(size = 20, face="bold"), axis.title.y = element_text(size = 20, face="bold")) +
  theme(panel.border = element_blank(),         panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Puptake_10t_ha.jpg", width = 14, height = 12, dpi = 500)



##### P Recovery ########
Prec_Mean <- summary_by(Precovery~Soil+Treatment, data=Pots1, FUN=mean) 
Prec_Mean <- as.numeric(Prec_Mean$Precovery)
Prec_skew <- skewness(Prec_Mean,na.rm=TRUE)
Prec_kur <- kurtosis(Prec_Mean,na.rm=TRUE)
cat("Skewness:", Prec_skew, "\n") ## data is mildly skewed @ 1.12
cat("Kurtosis:", Prec_kur, "\n") ## data has low kurtosis @ 1.0798
hist(Pots1$Precovery) # moderately left skewed
leveneTest(Precovery ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 1.631e-06
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
summary(Mod6a)$adj.r.squared # check adjusted R squared value: 0.66
anova(Mod6a)
summary(Mod6a)
shapiro.test(resid(Mod6a)) #data is not normally distributed: 3.472e-08
plot(fitted(Mod6a),resid(Mod6a),pch=16) # left skewed but even distribution along the centre
qqnorm(resid(Mod6a)) # heavy tails
qqline(resid(Mod6a))
#Mod6b - applying Yeo-Johnson transformation
Prec_YJ <- yjPower(Pots1$Precovery, 0.5,jacobian.adjusted=TRUE)
Mod6b <- lmer(Prec_YJ ~ Treatment*Soil + (1|Soil), data=Pots1)
rsq(Mod6b) # adjusted R squared: 0.83
anova(Mod6b)
summary(Mod6b)
leveneTest(Prec_YJ~Treatment*Soil, data=Pots1)  #data has slightly less unequal variances 2.875e-08
shapiro.test(resid(Mod6b)) #data is not normally distributed: 3.853e-08
plot(fitted(Mod6b),resid(Mod6b),pch=16) # moderately clustered in the middel fo the graph and slightly towards the top
qqnorm(resid(Mod6b)) # heavy tails
qqline(resid(Mod6b))
# Mod 6c lmer model
Mod6c <- lme(Precovery ~ Treatment*Soil, random=~1|Soil, data=Pots1, na.action=na.omit)
summary(Mod6c)
anova(Mod6c)
rsq(Mod6c) # 0.86
plot(ranef(Mod6c))

#AIC and BIC values - this indicated that Mod6b was marginally the best fit with Mod6 coming in a close second on AIC
# Create a list of the models
Prec_modlist <- list(Mod6, Mod6a, Mod6b, Mod6c)
# Obtain the AIC and BIC values
AIC_values <- sapply(Prec_modlist, AIC)
BIC_values <- sapply(Prec_modlist, BIC)
# Print the AIC and BIC values
PrecAB <- data.frame(Model=c("Mod6", "Mod6a", "Mod6b","Mod6c"), AIC_values, BIC_values)
print(PrecAB)
#Model AIC_values BIC_values
#1  Mod6   748.9711   819.3107
#2 Mod6a   748.9711   819.3107
#3 Mod6b   628.1842   701.1290
#4 Mod6c   627.4533   691.9672

#run emmeans on Mod6b - this model chosen as the second lowest AIC/BIC and highest Rsq with improved variances
Mod6bEm<- emmeans(Mod6b,~Treatment|Soil, subset=(Prec$Precovery), type="response")
Mod6bEm_cld <- cld(Mod6bEm, Letters = letters, alpha = 0.1, by="Soil") 
View(Mod6bEm_cld)
# Run emmeans on Mod6 - no significant differences detected between treatments
#Mod6em <- emmeans(Mod6a,~Treatment*Soil, subset=(Pots1$Precovery))
#Mod6em_cld <- cld(Mod6em, Letters = letters, by="Soil") 
#View(Mod6em_cld)

##Developing visualizations
par(mar=c(4,4,4,4)+0.4) #c(bottom, left, top, right) + 0.1 lines
##select specific treatments and use those in the graph - repeat for all subsets
Prec_trtVar <- c("CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha", "TripleSuperPhosphate")
Prec_subVar <- Mod6bEm_cld %>%
  filter(Treatment %in% Prec_trtVar)
ggplot(Prec_subVar, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  scale_y_continuous(limits = c(-10, 70))+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=6, position = position_dodge2(width = 0.9), vjust=-1.5) +
  labs(x = "Treatments", y = "P Recovery (%) in the 50kg P/ha treatments") +
  scale_x_discrete(labels = c("Canola Meal", "Canola Hull", "Manure", "Willow", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Prec_50kg_ha.jpg", width = 12, height = 8, dpi = 500)
#Plotting constant biochar rates with var P rates
par(mar=c(5,6,6,2)+0.4) #c(bottom, left, top, right) + 0.1 lines
Prec_charCon <- c("CanolaMeal10tha", "CanolaHull10tha", "Manure10tha",  "Willow10tha", "CanolaMeal10thaTSP", "CanolaHull10thaTSP",
                  "Manure10thaTSP", "Willow10thaTSP","TripleSuperPhosphate")
Prec_subCon <- Mod6bEm_cld %>%
  filter(Treatment %in% Prec_charCon)
ggplot(Prec_subCon, aes(x = Treatment, y = emmean, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+ 
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE, fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=6, position = position_dodge2(width = 0.9), vjust=-0.5) +
  labs(x = "Treatments", y = "P Recovery (%) in 10t/ha treatments") +
  scale_x_discrete(labels = c("Canola Meal", "Canola Hull", "Manure", "Willow", "Canola Meal\n& TSP", "Canola Hull\n& TSP",
                              "Manure\n& TSP", "Willow\n& TSP", "Fert.\nPhosphorus"))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_Prec_10t_ha.jpg", width = 12, height = 8, dpi = 500)



#####   NO3   #######
NO3_Mean <- summary_by(NO3~Soil+Treatment, data=Pots1, FUN=mean) 
NO3_Mean <- as.numeric(NO3_Mean$NO3)
NO3_skew <- skewness(NO3_Mean,na.rm=TRUE)
NO3_kur <- kurtosis(NO3_Mean,na.rm=TRUE)
cat("Skewness:", NO3_skew, "\n") ## data is mildly skewed @ 1.29
cat("Kurtosis:", NO3_kur, "\n") ## data has very low kurtosis @ 0.2929
hist(Pots1$NO3) #  left skewed
leveneTest(NO3 ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 2.878e-11
# Transform data
leveneTest(log(NO3) ~ Treatment*Soil, data = Pots1)  # much improved variance: 0.003112
leveneTest(log10(NO3) ~ Treatment*Soil, data = Pots1)  #  much improved variance: 0.003112
leveneTest(sqrt(NO3) ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 6.088e-07
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
anova(Mod7a) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod7a)
summary(Mod7a)$adj.r.squared  # 0.6804
shapiro.test(resid(Mod7a)) # p=0.04706
plot(fitted(Mod7a),resid(Mod7a),pch=16) # somewhat clustered to the left
qqnorm(resid(Mod7a)) # medium tails
qqline(resid(Mod7a))

#AIC and BIC values - this indicated that Mod7 was the best fit
NO3_modlist <- list(Mod7, Mod7a)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod7", "Mod7a"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod7  -85.68761  -1.972715
# 2 Mod7a  -85.68761  -1.972715

#run emmeans on Mod7 - highest Rsq 
Mod7em<- emmeans(Mod7,~Treatment|Soil, subset=(Pots1$NO3), type="response")
Mod7em_cld <- cld(Mod7em, Letters = letters, by="Soil") 
View(Mod7em_cld)
write.csv(Mod7em_cld, file="Pots1_NO3.csv")




#####   NH4    #######
NH4_Mean <- summary_by(NH4~Soil+Treatment, data=Pots1, FUN=mean) 
NH4_Mean <- as.numeric(NH4_Mean$NH4)
NH4_skew <- skewness(NH4_Mean,na.rm=TRUE)
NH4_kur <- kurtosis(NH4_Mean,na.rm=TRUE)
cat("Skewness:", NH4_skew, "\n") ## data is mildly skewed @ 0.8415
cat("Kurtosis:", NH4_kur, "\n") ## data has low kurtosis @ 0.8102
shapiro.test(Pots1$NH4) # p=3.14e-05
hist(Pots1$NH4) #  left skewed
leveneTest(NH4 ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 2.37e-07
# Transform data
leveneTest(log(NH4) ~ Treatment*Soil, data = Pots1)  # improved variance: 5.441e-05
leveneTest(log10(NH4) ~ Treatment*Soil, data = Pots1)  #  improved variance: 5.441e-05
leveneTest(sqrt(NH4) ~ Treatment*Soil, data = Pots1)  # slightly improved variance: 2.975e-06
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
anova(Mod8a) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod8a)
summary(Mod8a)$adj.r.squared  # 0.8599
shapiro.test(resid(Mod8a)) # p=0.002862
plot(fitted(Mod8a),resid(Mod8a),pch=16) # looks normal, starting to cluster in vertical lines
qqnorm(resid(Mod8a)) # medium tails
qqline(resid(Mod8a))

#AIC and BIC values - this indicated that Mod8 was the best fit
NO3_modlist <- list(Mod8, Mod8a)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod8", "Mod8a"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod8    39.4422   123.1571
#2  Mod8a   39.4422   123.1571

#run emmeans on Mod8 - highest Rsq 
Mod8em<- emmeans(Mod8,~Treatment|Soil, subset=(Pots1$NH4), type="response")
Mod8em_cld <- cld(Mod8em, Letters = letters, by="Soil") 
View(Mod8em_cld)
write.csv(Mod8em_cld, file="Pots1_NH4.csv")




##### PO4  #########
PO4_Mean <- summary_by(PO4~Soil+Treatment, data=Pots1, FUN=mean) 
PO4_Mean <- as.numeric(PO4_Mean$PO4)
PO4_skew <- skewness(PO4_Mean,na.rm=TRUE)
PO4_kur <- kurtosis(PO4_Mean,na.rm=TRUE)
cat("Skewness:", PO4_skew, "\n") ## data is moderately skewed @ 0.769
cat("Kurtosis:", PO4_kur, "\n") ## data has very low kurtosis @ -0.1055
shapiro.test(Pots1$PO4) # p=1.214e-08
hist(Pots1$PO4) #  left skewed
leveneTest(PO4 ~ Treatment*Soil, data = Pots1)  # data has unequal variance: p=4.024e-07
# Transform data
leveneTest(log(PO4) ~ Treatment*Soil, data = Pots1)  # equal variance: p=0.07069
leveneTest(log10(PO4) ~ Treatment*Soil, data = Pots1)  #  equal variance: p=0.07069
leveneTest(sqrt(PO4) ~ Treatment*Soil, data = Pots1)  # improved variance: p=0.001024
shapiro.test(log(Pots1$PO4)) # p=1.4e-06
shapiro.test(log10(Pots1$PO4)) # p=1.4e-06
shapiro.test(sqrt(Pots1$PO4)) # p=1.063e-06
hist((log(Pots1$PO4)))
hist((log10(Pots1$PO4)))
hist((sqrt(Pots1$PO4)))
#Mod9 linear model
Mod9<-lm(PO4~Treatment*Soil,data=Pots1) 
anova(Mod9) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
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
Mod9b<-lmer(PO4~Treatment*Soil+(1|Soil),data=Pots1)
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
Mod9d<-lmer(log10(PO4)~Treatment*Soil+(1|Soil),data=Pots1) # same as for natural log
anova(Mod9d)
shapiro.test(resid(Mod9d))
rsq(Mod9d) # adjusted R squared: 0.953674
plot(fitted(Mod9d),resid(Mod9d),pch=16) # data looks more random
qqnorm(resid(Mod9d)) #tails have improved - almost normal
qqline(resid(Mod9d))
#Mod4e - sqrt transformation - did not work, data non-normally distributed and has unequal variances
Mod9e <- lmer(sqrt(PO4)~Treatment*Soil+(1|Soil),data=Pots1)
anova(Mod9e)
summary(Mod9e)
shapiro.test(resid(Mod9e)) # p=5.173e-05
rsq(Mod9e) # adjusted r squared: 0.9486084
plot(fitted(Mod9e),resid(Mod9e),pch=16) # clusters forming
qqnorm(resid(Mod9e)) #fatter tails
qqline(resid(Mod9e))
#Mod4f using "gamma" distribution in glmer test - did not work, data less normally distributed with unequal variance
Mod9f <- glmer(PO4~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log"))
anova(Mod9f)
summary(Mod9f)
shapiro.test(resid(Mod9f)) # p=0.3705
bf.test(Nrecovery~Treatment, data=Pots1) # p=2.507187e-07 , variances unequal
rsq(Mod9f) # adjusted r squared: 0.9486084
plot(fitted(Mod9f),resid(Mod9f),pch=16) # clusters forming
qqnorm(resid(Mod9f)) #fatter tails
qqline(resid(Mod9f))
#Mod9g glmer using gamma distribution with log transformation - better than Mod4f, similar to Mod4c
Mod9g <- glmer(log(PO4)~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log"))
anova(Mod9g)
summary(Mod9g)
shapiro.test(resid(Mod9g)) # p=0.3567
bf.test(PO4~Treatment, data=Pots1) # 0.0003149367 
rsq.glmm(Mod9g)

# run AIC/BIC to test for best model fit
PO4_modlist <- list(Mod9, Mod9a, Mod9b, Mod9c, Mod9d, Mod9e, Mod9f, Mod9g)
# Obtain the AIC and BIC values
AIC_values <- sapply(PO4_modlist, AIC)
BIC_values <- sapply(PO4_modlist, BIC)
# Print the AIC and BIC values
PO4AB <- data.frame(Model=c("Mod9","Mod9a","Mod9b","Mod9c","Mod9d","Mod9e","Mod9f","Mod9g"), AIC_values, BIC_values)
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

# Based on the AIC, S-W and Levene test results, Mod9c has the best fit

#Run emmeans
Mod9em <- emmeans(Mod9c,~Treatment|Soil, subset=(Pots1$PO4>0), type="response")
Mod9cld <- cld(Mod9em, Letters = letters, by="Soil")
View(Mod9cld)
write.csv(Mod9cld, file="Pots1_PO4.csv")



#####   Resin P    #######
ResinP_Mean <- summary_by(ResinP~Soil+Treatment, data=Pots1, FUN=mean) 
ResinP_Mean <- as.numeric(ResinP_Mean$ResinP)
ResinP_skew <- skewness(ResinP_Mean,na.rm=TRUE)
ResinP_kur <- kurtosis(ResinP_Mean,na.rm=TRUE)
cat("Skewness:", ResinP_skew, "\n") ## data is mildly skewed @ 2.1
cat("Kurtosis:", ResinP_kur, "\n") ## data has low kurtosis @ 4.34
shapiro.test(Pots1$ResinP) # p=3.41e-12
hist(Pots1$ResinP) #  left skewed
leveneTest(ResinP ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 0.002065
# Transform data
leveneTest(log(ResinP) ~ Treatment*Soil, data = Pots1)  # improved variance: 0.132
leveneTest(log10(ResinP) ~ Treatment*Soil, data = Pots1)  #  improved variance: 0.132
leveneTest(sqrt(ResinP) ~ Treatment*Soil, data = Pots1)  # improved variance: 0.06481
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
anova(Mod10a) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod10a)
summary(Mod10a)$adj.r.squared  # 0.7477
shapiro.test(resid(Mod10a)) # p=0.01133
plot(fitted(Mod10a),resid(Mod10a),pch=16) # cluistering to right
qqnorm(resid(Mod10a)) # small-medium tails
qqline(resid(Mod10a))
#AIC and BIC values - this indicated that Mod10 was the best fit
NO3_modlist <- list(Mod10, Mod10a)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod10", "Mod10a"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod10    -71.30774   14.31965
#2 Mod10a  -71.30774   14.31965

#run emmeans on Mod10 - highest Rsq 
Mod10em<- emmeans(Mod10,~Treatment|Soil, subset=(Pots1$ResinP), type="response")
Mod10em_cld <- cld(Mod10em, Letters = letters, by="Soil") 
View(Mod10em_cld)
write.csv(Mod10em_cld, file="Pots1_ResinP.csv")



#####   WaterSolP    #######
WaterSolP_Mean <- summary_by(WaterSolP~Soil+Treatment, data=Pots1, FUN=mean) 
WaterSolP_Mean <- as.numeric(WaterSolP_Mean$WaterSolP)
WaterSolP_skew <- skewness(WaterSolP_Mean,na.rm=TRUE)
WaterSolP_kur <- kurtosis(WaterSolP_Mean,na.rm=TRUE)
cat("Skewness:", WaterSolP_skew, "\n") ## data is mildly skewed @ 1.764
cat("Kurtosis:", WaterSolP_kur, "\n") ## data has low kurtosis @ 2.749
shapiro.test(Pots1$WaterSolP) # p=5.271e-12
hist(Pots1$WaterSolP) #  left skewed
leveneTest(WaterSolP ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 4.537e-07
# Transform data
leveneTest(log(WaterSolP) ~ Treatment*Soil, data = Pots1)  # improved variance: 0.04701
leveneTest(log10(WaterSolP) ~ Treatment*Soil, data = Pots1)  #  improved variance: 0.04701
leveneTest(sqrt(WaterSolP) ~ Treatment*Soil, data = Pots1)  # improved variance: 0.0006312
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
anova(Mod11a) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod11a)
summary(Mod11a)$adj.r.squared  # 0.7369596
shapiro.test(resid(Mod11a)) # p=0.1704
plot(fitted(Mod11a),resid(Mod11a),pch=16) # clustered to left
qqnorm(resid(Mod11a)) # small tails
qqline(resid(Mod11a))

#AIC and BIC values - this indicated that Mod11 was the best fit
NO3_modlist <- list(Mod11, Mod11a)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod11", "Mod11a"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod11    -247.85  -161.9588
#2 Mod11a    -247.85  -161.9588

#run emmeans on Mod11 - highest Rsq 
Mod11em<- emmeans(Mod11,~Treatment|Soil, subset=(Pots1$WaterSolP), type="response")
Mod11em_cld <- cld(Mod11em, Letters = letters, by="Soil") 
View(Mod11em_cld)
write.csv(Mod11em_cld, file="Pots1_WaterSolP.csv")



#####   Totalp2    #######
TotalP2_Mean <- summary_by(TotalP2~Soil+Treatment, data=Pots1, FUN=mean) 
TotalP2_Mean <- as.numeric(TotalP2_Mean$TotalP2)
TotalP2_skew <- skewness(TotalP2_Mean,na.rm=TRUE)
TotalP2_kur <- kurtosis(TotalP2_Mean,na.rm=TRUE)
cat("Skewness:", TotalP2_skew, "\n") ## data is mildly skewed @ 0.997
cat("Kurtosis:", TotalP2_kur, "\n") ## data has low kurtosis @ 0.284
shapiro.test(Pots1$TotalP2) # p=0.001277
hist(Pots1$TotalP2) #  left skewed
leveneTest(TotalP2 ~ Treatment*Soil, data = Pots1)  # data has equal variance: 0.1879
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
anova(Mod12a) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod12a)
summary(Mod12a)$adj.r.squared  # 0.5276
shapiro.test(resid(Mod12a)) # p=0.07059
plot(fitted(Mod12a),resid(Mod12a),pch=16) # clustered to left
qqnorm(resid(Mod12a)) # small-medium tails
qqline(resid(Mod12a))
#Mod12b
Mod12b <- lmer(TotalP2~Treatment*Soil+(1|Soil), data=Pots1, , na.action = na.exclude)
rsq(Mod12b)  # 0.6789
anova(Mod12b) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod12b)
shapiro.test(resid(Mod12b)) # p=0.07059
plot(fitted(Mod12b),resid(Mod12b),pch=16) # clustered to left
qqnorm(resid(Mod12b)) # small-medium tails
qqline(resid(Mod12b))

#AIC and BIC values - this indicated that Mod12 was the best fit
NO3_modlist <- list(Mod12, Mod12a, Mod12b)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod12", "Mod12a", "Mod12b"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod12    1149.313   1234.941
#2 Mod12a    1149.313   1234.941
#3 Mod12b    939.016   1027.406

#run emmeans on Mod12b - highest Rsq & lowest AIC/BIc
Mod12em<- emmeans(Mod12b,~Treatment|Soil, subset=(Pots1$TotalP2), type="response")
Mod12em_cld <- cld(Mod12em, Letters = letters, by="Soil") 
View(Mod12em_cld)
write.csv(Mod12em_cld, file="Pots1_TotalP2.csv")




#####   pH   #######
pH_Mean <- summary_by(pH~Soil+Treatment, data=Pots1, FUN=mean) 
pH_Mean <- as.numeric(pH_Mean$pH)
pH_skew <- skewness(pH_Mean,na.rm=TRUE)
pH_kur <- kurtosis(pH_Mean,na.rm=TRUE)
cat("Skewness:", pH_skew, "\n") ## data is mildly skewed @ -0.649
cat("Kurtosis:", pH_kur, "\n") ## data has low kurtosis @ -0.348
shapiro.test(Pots1$pH) # p=0.0004384
hist(Pots1$pH) #  left skewed
leveneTest(pH ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 0.007327
# Transform data
leveneTest(log(pH) ~ Treatment*Soil, data = Pots1)  # slightly worsened variance: 0.006375
leveneTest(log10(pH) ~ Treatment*Soil, data = Pots1)  #  slightly worsened variance: 0.006375
leveneTest(sqrt(pH) ~ Treatment*Soil, data = Pots1)  # slightly worsened variance: 0.006841
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
Mod13b <- lmer(pH~Treatment*Soil+(1|Soil), data=Pots1, , na.action = na.exclude)
rsq(Mod13b)  # 0.9428
anova(Mod13b) 
summary(Mod13b)
shapiro.test(resid(Mod13b)) # p=1.083e-05
plot(fitted(Mod13b),resid(Mod13b),pch=16) # clustered to right
qqnorm(resid(Mod13b)) # medium tails
qqline(resid(Mod13b))
#Mod13c
Mod13c <- glmer(pH~Treatment*Soil+(1|Soil),data=Pots1,family=gaussian(link="log"))
rsq(Mod13c) # 0.9386
anova(Mod13c)
summary(Mod13c)
shapiro.test(resid(Mod13c))  #p=1.083e-05
bf.test(pH~Treatment, data=Pots1)  # variance equal p=0.2407674 
plot(fitted(Mod13c),resid(Mod13c),pch=16) # clustered to right
qqnorm(resid(Mod13c)) # medium tails
qqline(resid(Mod13c))

# Rsq summary: Mod13 = 0.53968; Mod13a = 0.9189; Mod13b = 0.9428; Mod13c = 0.9386
#AIC and BIC values - this indicated that Mod13 was the best fit
NO3_modlist <- list(Mod13, Mod13a, Mod13b, Mod13c)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod13", "Mod13a", "Mod13b", "Mod13c"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod13  -464.4936  -378.0814
#2 Mod13a  -464.4936  -378.0814
#3 Mod13b  -263.3900  -174.1903
#4 Mod13c  -460.4936  -371.2939

#run emmeans on Mod13a - Models 13b&c have convergence and singularity issues
Mod13em<- emmeans(Mod13a,~Treatment|Soil, subset=(Pots1$pH), type="response")
Mod13em_cld <- cld(Mod13em, Letters = letters, by="Soil") 
View(Mod13em_cld)
write.csv(Mod13em_cld, file="Pots1_pH.csv")






#####  EC   #######
EC_Mean <- summary_by(EC~Soil+Treatment, data=Pots1, FUN=mean) 
EC_Mean <- as.numeric(EC_Mean$EC)
EC_skew <- skewness(EC_Mean,na.rm=TRUE)
EC_kur <- kurtosis(EC_Mean,na.rm=TRUE)
cat("Skewness:", EC_skew, "\n") ## data is mildly skewed @ 1.1314
cat("Kurtosis:", EC_kur, "\n") ## data has low kurtosis @ 0.7454
shapiro.test(Pots1$EC) # p=1.936e-08
hist(Pots1$EC) #  left skewed
leveneTest(EC ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 0.000132
# Transform data
leveneTest(log(EC) ~ Treatment*Soil, data = Pots1)  # improved variance: 0.004631
leveneTest(log10(EC) ~ Treatment*Soil, data = Pots1)  #  improved variance: 0.004631
leveneTest(sqrt(EC) ~ Treatment*Soil, data = Pots1)  # slightly improved variance: 0.000926
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
anova(Mod14a) #note that residuals = error in summary table, and that total SS is not printed (but can be calculated)
summary(Mod14a)
summary(Mod14a)$adj.r.squared  # 0.75516
shapiro.test(resid(Mod14a)) # p=0.4634
plot(fitted(Mod14a),resid(Mod14a),pch=16) # clustered to left
qqnorm(resid(Mod14a)) # small tails
qqline(resid(Mod14a))

# Rsq summary: Mod14 = 0.9513; Mod14a = 0.75516
#AIC and BIC values 
NO3_modlist <- list(Mod14, Mod14a)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod14", "Mod14a"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod14  -228.8464  -143.4851
#2 Mod14a  -228.8464  -143.4851

#run emmeans on Mod14 with highest rsq
Mod14em<- emmeans(Mod14,~Treatment|Soil, subset=(Pots1$EC), type="response")
Mod14em_cld <- cld(Mod14em, Letters = letters, by="Soil") 
View(Mod14em_cld)
write.csv(Mod14em_cld, file="Pots1_EC.csv")



#####   OC   #######
OC_Mean <- summary_by(OC~Soil+Treatment, data=Pots1, FUN=mean) 
OC_Mean <- as.numeric(OC_Mean$OC)
OC_skew <- skewness(OC_Mean,na.rm=TRUE)
OC_kur <- kurtosis(OC_Mean,na.rm=TRUE)
cat("Skewness:", OC_skew, "\n") ## data is slightly skewed @ -0.352
cat("Kurtosis:", OC_kur, "\n") ## data has low/medium kurtosis @ -1.32
shapiro.test(Pots1$OC) # p=0.002034
hist(Pots1$OC) #  slightly left skewed
leveneTest(OC ~ Treatment*Soil, data = Pots1)  # data has unequal variance: 2.511e-07
# Transform data
leveneTest(log(OC) ~ Treatment*Soil, data = Pots1)  # slightly improved variance: 2.138e-06
leveneTest(log10(OC) ~ Treatment*Soil, data = Pots1)  #  slightly improved variance: 2.138e-06
leveneTest(sqrt(OC) ~ Treatment*Soil, data = Pots1)  # slightly improved variance: 5.999e-07
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
Mod15b <- lmer(OC~Treatment*Soil+(1|Soil), data=Pots1, na.action = na.exclude)
rsq(Mod15b)  # 0.9366
anova(Mod15b) 
summary(Mod15b)
shapiro.test(resid(Mod15b)) # p=5.413e-09
plot(fitted(Mod15b),resid(Mod15b),pch=16) # 2 clusters forming
qqnorm(resid(Mod15b)) # fat tails
qqline(resid(Mod15b))
#Mod15c
Mod15c <- glmer(OC~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log"), na.action = na.exclude)
anova(Mod15c)
summary(Mod15c)
shapiro.test(resid(Mod15c))  #p=2.297e-07
bf.test(OC~Treatment, data=Pots1)  # variance equal p=0.2546385 
plot(fitted(Mod15c),resid(Mod15c),pch=16) # 2 clusters forming
qqnorm(resid(Mod15c)) # fat tails
qqline(resid(Mod15c))
#Mod15d
Mod15d <- glm(OC ~ Treatment*Soil, family = gaussian, data = Pots1)
rsq(Mod15d) # 0.9334
anova(Mod15d, test = "Chisq")
summary(Mod15d)
shapiro.test(resid(Mod15d))  # residuals p=5.413e-09
plot(fitted(Mod15d),resid(Mod15d),pch=16) # 2 clusters forming
qqnorm(resid(Mod15d))  # fat tails
qqline(resid(Mod15d))
#Mod15e - YJ transformation
OC_YJ <- yjPower(Pots1$OC, 0.5,jacobian.adjusted=TRUE)
Mod15e <- lmer(OC_YJ ~ Treatment*Soil + (1|Soil), data=Pots1, control=lmerControl(optCtrl = list(maxfun = 100000)))
rsq(Mod15e) # adjusted R squared: 0.9223
anova(Mod15e)
summary(Mod15e)
leveneTest(OC_YJ~Treatment*Soil, data=Pots1)  #data has unequal variances 4.059e-07
shapiro.test(resid(Mod15e)) #data is not normally distributed: 1.727e-08
plot(fitted(Mod15e),resid(Mod15e),pch=16) # 2 clusters
qqnorm(resid(Mod15e)) # fat tails
qqline(resid(Mod15e))

# Rsq summary: Mod15 = 0.1746; Mod15a = 0.9101, Mod15b = 0.9366; Mod15c = ??; Mod15d = 0.9334; Mod15e = 0.9223
#AIC and BIC values 
NO3_modlist <- list(Mod15, Mod15a, Mod15b, Mod15c, Mod15d, Mod15e)
AIC_values <- sapply(NO3_modlist, AIC)
BIC_values <- sapply(NO3_modlist, BIC)
NO3AB <- data.frame(Model=c("Mod15", "Mod15a", "Mod15b", "Mod15c", "Mod15d", "Mod15e"), AIC_values, BIC_values)
print(NO3AB)
#  Model AIC_values BIC_values
#1  Mod15  -59.00536   25.54366
#2 Mod15a  -59.00536   25.54366
#3 Mod15b   40.30443  127.58084
#4 Mod15c  -62.73739   24.53902
#5 Mod15d  -59.00536   25.54366
#6 Mod15e   36.31864  123.59505

#run emmeans on Mod15d high rsq, with lowest combined AIC & BIC
Mod15em<- emmeans(Mod15d,~Treatment|Soil, subset=(Pots1$OC), type="response")
Mod15em_cld <- cld(Mod15em, Letters = letters, by="Soil") 
View(Mod15em_cld)
write.csv(Mod15em_cld, file="Pots1_OC.csv")




####   Water holding capacity  #####
WHC <- data.frame(
  Soil = as.factor(rep(c("Haverhill", "Oxbow"), each=5)),
  Treatment = as.factor(c("Control2", "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "Control2",
              "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha")),
  VolWatContent = c(21, 22, 21, 24, 21, 22, 23, 24, 24, 24))
print(WHC)
# boxplot
ggplot(WHC, aes(x = Soil, y = VolWatContent)) +
  geom_boxplot() +
  geom_jitter(shape = 15, color = "steelblue", position = position_jitter(0.21)) +
    labs(x = "Treatment", y = "Volumetric Water Content %")
ggsave("WaterHoldingCapacity_boxplot.jpg", width = 8, height = 8, dpi = 300)

#geom_text(aes(label = Treatment), position = position_dodge(width = 0.5), vjust = -0.7, color = "steelblue", size = 3) +

#model data
ModWHC1 <- lm(VolWatContent~Treatment*Soil, data=WHC)
anova(ModWHC1)
summary(ModWHC1)
shapiro.test(resid(ModWHC1)) # p=0.0008813
hist(resid(ModWHC1)) # heavily flattened tails
qqnorm(resid(ModWHC1)) # heavy tails
qqline(resid(ModWHC1))




pairwise.t.test(WHC$VolWatContent, list(WHC$Treatment, WHC$Soil))


WHC %>%
  group_by(Soil) %>%
  do(tidy(pairwise.t.test(.$VolWatContent, .$Treatment)))

ggplot(WHC, aes(x = Treatment, y = VolWatContent, pattern = Soil)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("Haverhill" = "stripe", "Oxbow" = "crosshatch"), 
                       labels = c("Haverhill", "Oxbow"))+
  geom_text(aes(label = paste0(VolWatContent, "%"), fontface = ifelse(Soil == "Haverhill", "italic", "plain")),
            size=6, position = position_dodge2(width = 0.9), vjust=-1) +
  labs(x = "Treatments", y = "Volumetric Water Content (%)") +
  scale_x_discrete(labels = c("Control 2", "Canola Meal\n10t/ha", "Canola Hull\n10t/ha", "Manure\n10t/ha", "Willow\n10t/h"))+
  scale_y_continuous(limits = c(0, 28))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Pots1_WatHolCapacity.jpg", width = 12, height = 8, dpi = 500)


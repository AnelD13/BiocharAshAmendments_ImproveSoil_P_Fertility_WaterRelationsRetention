## Field data for Pots2 study
#Loading data in to R
Pots2<-read.csv("Pots2.csv", fileEncoding="UTF-8-BOM")
View(Pots2)
Pots2Raw<-read.csv("Pots2raw.csv", fileEncoding="UTF-8-BOM")

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
library(ggpubr)
library(stringr)
library(corrplot)
library(writexl)
library(glmmTMB)
library(viridis)
library(RColorBrewer)
library(corrplot)
library(glmmTMB)
library(writexl)
library(openxlsx)


##### Summary and ordering of data   ####
#Check for missing values in a specific field
missing <- colSums(is.na(Pots2[,]))
print(missing)

#Change columns in a dataframe to factors/categorical values, str displays 
Pots2$Block <- factor(Pots2$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Pots2$Treatment <- factor(Pots2$Treatment,levels = c("Control1", "Control2", "CanolaMeal", "Manure", "Willow",
                                                     "MBMACoarse", "MBMAFine", "Phosphorus"))
Pots2$Grain <- as.numeric(as.character(Pots2$Grain))
Pots2$Straw <- as.numeric(as.character(Pots2$Straw))
summary(Pots2)
str(Pots2) #displays the structure of the object

# Summary data (means, SD, etc.) for each treatment and variable
Pots2Mean <- summary_by(.~Soil+Treatment, data=Pots2, FUN=mean) #overall means for the data set
Pots2SD <- summary_by(.~Soil+Treatment, data=Pots2, FUN=sd) #overall SD for the dataset
View(Pots2Mean)
View(Pots2SD)
Straw_summary <- Pots2 %>% #summary data specifically for Straw variable
  group_by(Treatment) %>%
  summarize(Mean=mean(Straw),
            SD=sd(Straw),
            SE=sd(Straw)/sqrt(n()))
View(Straw_summary) #summary data specifically for Grain variable
Grain_summary <- Pots2 %>%
  group_by(Treatment) %>%
  summarize(Mean=mean(Grain),
            SD=sd(Grain),
            SE=sd(Grain)/sqrt(n()))
View(Grain_summary)


#####   Check for outliers   ####
#Grain
ggplot(Pots2Raw, aes(x = Treatment, y = Grain, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Grain")
ggsave("OutliersWheat_Grain.jpg", width = 10, height = 10, dpi = 150)
#Straw
ggplot(Pots2Raw, aes(x = Treatment, y = Straw, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Straw")
ggsave("OutliersWheat_Straw.jpg", width = 10, height = 10, dpi = 150)
#Biomass
ggplot(Pots2Raw, aes(x = Treatment, y = Biomass, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Biomass")
ggsave("OutliersWheat_Biomass.jpg", width = 10, height = 10, dpi = 150)
# Nuptake
ggplot(Pots2Raw, aes(x = Treatment, y = Nuptake, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "N uptake")
ggsave("OutliersWheat_Nuptake.jpg", width = 10, height = 10, dpi = 150)
# P uptake
ggplot(Pots2Raw, aes(x = Treatment, y = Puptake, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "P uptake")
ggsave("OutliersWheat_Puptake.jpg", width = 10, height = 10, dpi = 150)
# Soil NO3
ggplot(Pots2Raw, aes(x = Treatment, y = SNO3, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Soil NO3")
ggsave("OutliersWheat_SNO3.jpg", width = 10, height = 10, dpi = 150)
# Soil NH4
ggplot(Pots2Raw, aes(x = Treatment, y = SNH4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Soil NH4")
ggsave("OutliersWheat_SNH4.jpg", width = 10, height = 10, dpi = 150)
# Soil PO4
ggplot(Pots2Raw, aes(x = Treatment, y = SPO4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Soil PO4")
ggsave("OutliersWheat_SPO4.jpg", width = 10, height = 10, dpi = 150)
# Resin P
ggplot(Pots2Raw, aes(x = Treatment, y = ResinP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Resin P")
ggsave("OutliersWheat_ResinP.jpg", width = 10, height = 10, dpi = 150)
# Water Soluble P
ggplot(Pots2Raw, aes(x = Treatment, y = WatSolP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Water Soluble P")
ggsave("OutliersWheat_WatSolP.jpg", width = 10, height = 10, dpi = 150)
# Total Soil P
ggplot(Pots2Raw, aes(x = Treatment, y = TotalP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Total Soil P")
ggsave("OutliersWheat_TotalP.jpg", width = 10, height = 10, dpi = 150)
# pH
ggplot(Pots2Raw, aes(x = Treatment, y = pH, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "pH")
ggsave("OutliersWheat_pH.jpg", width = 10, height = 10, dpi = 150)
# EC
ggplot(Pots2Raw, aes(x = Treatment, y = EC, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Electric Conductivity")
ggsave("OutliersWheat_EC.jpg", width = 10, height = 10, dpi = 150)
# OC
ggplot(Pots2Raw, aes(x = Treatment, y = OC, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "% Organic Carbon")
ggsave("OutliersWheat_OC.jpg", width = 10, height = 10, dpi = 150)
# Leachate NO3
ggplot(Pots2Raw, aes(x = Treatment, y = LNO3, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Leachate NO3")
ggsave("OutliersWheat_LNO3.jpg", width = 10, height = 10, dpi = 150)
# Leachate NH4
ggplot(Pots2Raw, aes(x = Treatment, y = LNH4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Leachate NH4")
ggsave("OutliersWheat_LNH4.jpg", width = 10, height = 10, dpi = 150)
# Leachate PO4
ggplot(Pots2Raw, aes(x = Treatment, y = LPO4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Leachate PO4")
ggsave("OutliersWheat_LPO4.jpg", width = 10, height = 10, dpi = 150)


#####   GRAIN   #####
ModP2Grain_Mean <- summary_by(Grain~Treatment+Block, data=Pots2, FUN=mean) 
ModP2Grain_Mean <- as.numeric(ModP2Grain_Mean$Grain)
ModP2Grain_skew <- skewness(ModP2Grain_Mean)
ModP2Grain_kur <- kurtosis(ModP2Grain_Mean)
cat("Skewness:", ModP2Grain_skew, "\n") # 0.4478962 
cat("Kurtosis:", ModP2Grain_kur, "\n") # -0.09995475  
hist(Pots2$Grain) #  slightly right skewed
shapiro.test(Pots2$Grain) # p=0.6225
leveneTest(Grain ~ Treatment, data = Pots2)  # 0.379
hist(log(Pots2$Grain)) #  right skewed
shapiro.test(log(Pots2$Grain)) # p=0.02587
leveneTest(log(Grain) ~ Treatment, data = Pots2)  # 0.2947
hist(log10(Pots2$Grain)) #  slightly right skewed
shapiro.test(log10(Pots2$Grain)) # p=0.02587
leveneTest(log10(Grain) ~ Treatment, data = Pots2)  # 0.2947
hist(sqrt(Pots2$Grain)) #  right skewed
shapiro.test(sqrt(Pots2$Grain)) # p=0.1911
leveneTest(sqrt(Grain) ~ Treatment, data = Pots2)  # 0.3263
hist(1/(Pots2$Grain)) #  very left skewed
shapiro.test(1/(Pots2$Grain)) # p=0.0001527
leveneTest(1/(Grain) ~ Treatment, data = Pots2)  # 0.28
#ModP2G1 aov
ModP2G1 <- aov(Grain~Treatment+Block,data=Pots2)
anova(ModP2G1)
summary(ModP2G1)
residS1 <- resid(ModP2G1) 
hist(residS1)  # data is normally distributed
shapiro.test(resid(ModP2G1))  # p=0.3836
plot(fitted(ModP2G1),resid(ModP2G1),pch=16)  # 
qqnorm(resid(ModP2G1)) # slight tails
qqline(resid(ModP2G1))
# lm model with weighted least squares
ModP2Gvar <- tapply(Pots2$Grain, Pots2$Treatment, var) # Calculate the variances for each treatment
weightsGrain <- 1 / ModP2Gvar # Create a vector of weights
weightsGrain_full <- rep(weightsGrain, each = length(Pots2$Grain) / length(weightsGrain))
ModP2G2 <- lm(Grain ~ Treatment + Block, data=Pots2, weights=weightsGrain_full) # Fit a WLS model
hist(resid(ModP2G2))  # right skew
shapiro.test(resid(ModP2G2))  # p=0.2325
plot(fitted(ModP2G2),resid(ModP2G2),pch=16) 
qqnorm(resid(ModP2G2)) # left tail
qqline(resid(ModP2G2))
rsq(ModP2G2)  # 0.7286155
# ModP2G3 lme model
ModP2G3 <- lme(Grain ~ Treatment, random=~1|Block, data=Pots2)
summary(ModP2G3)
anova(ModP2G3)
shapiro.test(resid(ModP2G3)) # p= 0.528
plot(fitted(ModP2G3),resid(ModP2G3),pch=16) # normal, slightly above midline
qqnorm(resid(ModP2G3)) # slight-moderate left tail
qqline(resid(ModP2G3))
rsq(ModP2G3) # 0.5607
# ModP2G4 glmm - convergence issues related to non-positive values
ModP2G4<- glmmTMB(Grain~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2G4, type="III")
summary(ModP2G4)
shapiro.test(resid(ModP2G4)) # p=0.528
plot(fitted(ModP2G4),resid(ModP2G4),pch=16) # normal, slightly towards top
qqnorm(resid(ModP2G4)) # moderate left tail
qqline(resid(ModP2G4))
performance::r2(ModP2G4) # 0.571

#emmeans for glmm model, decent df and rsq
ModP2Gem <- emmeans(ModP2G4,~Treatment, type="response")
ModP2Gem_cld <- cld(ModP2Gem, Letters = trimws(letters), reversed = TRUE) 
View(ModP2Gem_cld)
write.csv(ModP2Gem_cld, file="Pots2_Grain.csv")



#####   STRAW   #############
ModP2Straw_Mean <- summary_by(Straw~Treatment+Block, data=Pots2, FUN=mean) # calculate means of Straw
ModP2Straw_Mean <- as.numeric(ModP2Straw_Mean$Straw)
ModP2Straw_skew <- skewness(ModP2Straw_Mean)
ModP2Straw_kur <- kurtosis(ModP2Straw_Mean)
cat("Skewness:", ModP2Straw_skew, "\n") # -0.08877305
cat("Kurtosis:", ModP2Straw_kur, "\n") # -1.095164 
hist(Pots2$Straw) #  
shapiro.test(Pots2$Straw) # p=0.5918
leveneTest(Straw ~ Treatment, data = Pots2)  # 0.8324
#ModP2S1 aov
ModP2S1 <- aov(Straw~Treatment+Block,data=Pots2)
anova(ModP2S1)
summary(ModP2S1)
hist(resid(ModP2S1))  # data is normally distributed
shapiro.test(resid(ModP2S1))  # p= 0.9415
plot(fitted(ModP2S1),resid(ModP2S1),pch=16)  # data is normally distributed
qqnorm(resid(ModP2S1)) # data is normally distributed
qqline(resid(ModP2S1))
# lm model with weighted least squares
ModP2Svar <- tapply(Pots2$Straw, Pots2$Treatment, var) # Calculate the variances for each treatment
weightsStraw <- 1 / ModP2Svar # Create a vector of weights
weightsStraw_full <- rep(weightsStraw, each = length(Pots2$Straw) / length(weightsStraw))
ModP2S2 <- lm(Straw ~ Treatment + Block, data=Pots2, weights=weightsStraw_full) # Fit a WLS model
hist(resid(ModP2S2))  # left skew
shapiro.test(resid(ModP2S2))  # p=0.8112
plot(fitted(ModP2S2),resid(ModP2S2),pch=16) 
qqnorm(resid(ModP2S2)) # right tail
qqline(resid(ModP2S2))
rsq(ModP2S2)  # 0.7827512
# ModP2S3 lme model
ModP2S3 <- lme(Straw ~ Treatment, random=~1|Block, data=Pots2)
summary(ModP2S3)
anova(ModP2S3)
shapiro.test(resid(ModP2S3)) # p= 0.867
plot(fitted(ModP2S3),resid(ModP2S3),pch=16) # normal
qqnorm(resid(ModP2S3)) # slight left tail
qqline(resid(ModP2S3))
rsq(ModP2S3) # 0.635
# ModP2S4 glmm - convergence issues related to non-positive values
ModP2S4<- glmmTMB(Straw~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2S4, type="III")
summary(ModP2S4)
shapiro.test(resid(ModP2S4)) # p=0.567
plot(fitted(ModP2S4),resid(ModP2S3),pch=16) # normal
qqnorm(resid(ModP2S4)) # slight left tail
qqline(resid(ModP2S4))
performance::r2(ModP2S4) # 0.658

#emmeans for glmm - decent df, higher rsq
ModP2emS <- emmeans(ModP2S4,~Treatment, type="response")
ModP2emS1_cld <- cld(ModP2emS, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emS1_cld)
write.csv(ModP2emS1_cld, file="Pots2_Straw.csv")



#####   BIOMASS   #####
PotBio_Mean <- summary_by(Biomass~Treatment+Block, data=Pots2, FUN=mean)
PotBio_Mean <- as.numeric(PotBio_Mean$Biomass)
PotBio_skew <- skewness(PotBio_Mean,na.rm=TRUE)
PotBio_kur <- kurtosis(PotBio_Mean,na.rm=TRUE)
cat("Skewness:", PotBio_skew, "\n") # 0.04157241 
cat("Kurtosis:", PotBio_kur, "\n") # -1.08367 
shapiro.test(Pots2$Biomass) # p=0.6171
hist(Pots2$Biomass) #  
car::leveneTest(Biomass~Treatment, data=Pots2)  # p=0.451
ggplot(Pots2, aes(x = Block, y = Biomass)) + #checking treatments in boxplots to see if variances are equal across blocks
  geom_boxplot() +
  labs(x = "Block", y = "Biomass") +
  theme_bw()
#ModP2Bio1
ModP2Bio1 <- aov(Biomass~Treatment+Block, data=Pots2)
anova(ModP2Bio1)
summary(ModP2Bio1)
hist(resid(ModP2Bio1))
shapiro.test(resid(ModP2Bio1))  # p=0.1125
plot(fitted(ModP2Bio1),resid(ModP2Bio1),pch=16)
qqnorm(resid(ModP2Bio1)) # slight tails
qqline(resid(ModP2Bio1))
ModP2Bio1_tidy <- tidy(ModP2Bio1)
ModP2Bio1sum_sq_reg <- ModP2Bio1_tidy$sumsq[1] 
ModP2Bio1sum_sq_resid <- ModP2Bio1_tidy$sumsq[2] 
ModP2Bio1sum_sq_reg / (ModP2Bio1sum_sq_reg + ModP2Bio1sum_sq_resid) # 0.874
# lm model with weighted least squares
ModP2Biovar <- tapply(Pots2$Biomass, Pots2$Treatment, var) # Calculate the variances for each treatment
weightsBio <- 1 / ModP2Biovar # Create a vector of weights
weightsBio_full <- rep(weightsBio, each = length(Pots2$Biomass) / length(weightsBio))
ModP2Bio2 <- lm(Biomass ~ Treatment + Block, data=Pots2, weights=weightsBio_full) # Fit a WLS model
hist(resid(ModP2Bio2))  # left skew
shapiro.test(resid(ModP2Bio2))  # p=0.1394
plot(fitted(ModP2Bio2),resid(ModP2Bio2),pch=16) 
qqnorm(resid(ModP2Bio2)) # longer left tail
qqline(resid(ModP2Bio2))
rsq(ModP2Bio2)  # 0.7748827
# ModP2Bio3 glmm - convergence issues related to non-positive values
ModP2Bio3<- glmmTMB(Biomass~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2Bio3, type="III")
summary(ModP2Bio3)
shapiro.test(resid(ModP2Bio3)) # p=0.027
plot(fitted(ModP2Bio3),resid(ModP2Bio3),pch=16) # normal
qqnorm(resid(ModP2Bio3)) # moderate-heavy tails tail
qqline(resid(ModP2Bio3))
performance::r2(ModP2Bio3) # 0.709
#ModP2Bio4
ModP2Bio4 <- lmer(Biomass~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2Bio4) 
summary(ModP2Bio4)
shapiro.test(resid(ModP2Bio4)) # p=0.027
plot(fitted(ModP2Bio4),resid(ModP2Bio4),pch=16) # normal
qqnorm(resid(ModP2Bio4)) # heavy left & moderate right tail
qqline(resid(ModP2Bio4))
rsq(ModP2Bio4)  # 0.688
# ModP2Bio5 lme model
ModP2Bio5 <- lme(Biomass ~ Treatment, random=~1|Block, data=Pots2)
summary(ModP2Bio5)
anova(ModP2Bio5)
shapiro.test(resid(ModP2Bio5)) # p= 0.027
plot(fitted(ModP2Bio5),resid(ModP2Bio5),pch=16) # normal
qqnorm(resid(ModP2Bio5)) # heavy left & moderate right tail
qqline(resid(ModP2Bio5))
rsq(ModP2Bio5) # 0.688
#ModP2Bio6 glmer
ModP2Bio6 <- glmer(Biomass~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"))
anova(ModP2Bio6)
summary(ModP2Bio6)
shapiro.test(resid(ModP2Bio6)) # p=0.046
bf.test(Biomass~Treatment, data=Pots2) # p=0.00069, variances unequal
plot(fitted(ModP2Bio6),resid(ModP2Bio6),pch=16) # normal
qqnorm(resid(ModP2Bio6)) # heavy left & moderate right tail
qqline(resid(ModP2Bio6))
rsq(ModP2Bio6) # r=0.725

#AIC and BIC values
Pots2Bio_modlist <- list(ModP2Bio3, ModP2Bio4, ModP2Bio5, ModP2Bio6)
AIC_values <- sapply(Pots2Bio_modlist, AIC)
BIC_values <- sapply(Pots2Bio_modlist, BIC)
Pots2AICBio <- data.frame(Model=c("ModP2Bio3", "ModP2Bio4", "ModP2Bio5", "ModP2Bio6"), AIC_values, BIC_values)
print(Pots2AICBio)
#Model AIC_values BIC_values
#1 ModP2Bio3   115.7187   130.3760
#2 ModP2Bio4   109.7837   124.4411
#3 ModP2Bio5   109.7837   121.5643
#4 ModP2Bio6   116.6323   131.2896

#emmeans using glmm model - simple,  AIC, highest rsq, df decent
ModP2emBio <- emmeans(ModP2Bio3,~Treatment, type="response")
ModP2emBio_cld <- cld(ModP2emBio, Letters = trimws(letters), reversed = TRUE) 
View(ModP2emBio_cld)
write.csv(ModP2emBio_cld, file="Pots2_Biomass.csv")

#create combined grain & straw data frame
ModP2Gem_cld$origin <- "Grain"
ModP2emS1_cld$origin <- "Straw"
BiomassEm <- rbind(ModP2emS1_cld,ModP2Gem_cld)
BiomassEm <- as.data.frame(BiomassEm)
BiomassEm <- BiomassEm[, c("Treatment", "emmean", "SE", ".group", "origin")]
BiomassEm$.group <- str_trim(BiomassEm$.group)
write.csv(BiomassEm, file="BiomassEm.csv")
View(BiomassEm)
#reshape dataframe to longer format
BiomassEm_long <- BiomassEm|> pivot_longer(cols = c("emmean", "SE"), names_to = "name", values_to = "value")|>
  mutate(name = forcats::fct_rev(name))
print(BiomassEm_long)
View(BiomassEm_long)

#Create stacked barplot
## call dataframe
BiomassEm_Manual <- subset(Pots2, select=c("Treatment", "Grain", "Straw"), na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
BiomassEm_Manual$Treatment <- factor(BiomassEm_Manual$Treatment, levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow",
                                                                          "MBMACoarse", "MBMAFine", "Phosphorus"),
                                     labels=c("Control 1", "Control 2", "Canola Meal", "Manure", "Willow", "Meat & Bone\nMeal- Coarse",
                                              "Meat & Bone\nMeal- Fine", "Fertilizer\nPhosphorus"))
print(BiomassEm_Manual)
# use ggbarplot
(Pots2BioStack <- BiomassEm_Manual|>
    pivot_longer(-Treatment) |>
    mutate(name = forcats::fct_relevel(name, "Grain", "Straw")) |>
    ggbarplot(x = "Treatment", y = "value", fill = "name", add = "mean_se") +
    labs(x = "Treatment", y = "Total grain and straw yield (g)", fill = "") +
    scale_fill_manual(values = c("grey30", "grey89")) +
    scale_x_discrete(labels = c("Control 1", "Control 2", "Canola Meal", "Manure", "Willow", "Meat & Bone\nMeal- Coarse",
                                "Meat & Bone\nMeal- Fine", "Fertilizer\nPhosphorus"))+
    theme(legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"),
          legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pots2BioStack, file="Pots2_biomass_stack.jpg", width = 10, height = 8, dpi = 150)

#creating side by side plot of grain and straw 
(Pots2BioPlot <- ggplot(BiomassEm, aes(x=Treatment, y=emmean, pattern=origin)) +
    geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label = ifelse(origin == "Straw", toupper(trimws(.group)), trimws(.group)),
                  y = emmean + SE + 0.5), size = 9, position = position_dodge(width = 0.9))+
    labs(x = "Treatment", y = "Total wheat grain and straw yield (g)", pattern="") +
    scale_pattern_manual(values = c("Grain" = "stripe", "Straw" = "crosshatch"), 
                       labels = c("Grain", "Straw"))+
    scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", 
                                "Meat and Bone\nMeal - Coarse", "Meat and Bone\nMeal - Fine", 
                                "Fertilizer\nPhosphorus"))+
    scale_y_continuous(limits = c(0, 10))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pots2BioPlot, file="Pots2_biomass.jpg", width = 10, height = 8, dpi = 150)



#####   N UPTAKE  #####
Pots2Nup_Mean <- summary_by(Nuptake~Treatment+Block, data=Pots2, FUN=mean) 
Pots2Nup_Mean <- as.numeric(Pots2Nup_Mean$Nuptake)
Pots2Nup_skew <- skewness(Pots2Nup_Mean,na.rm=TRUE)
Pots2Nup_kur <- kurtosis(Pots2Nup_Mean,na.rm=TRUE)
cat("Skewness:", Pots2Nup_skew, "\n") # 0.56394 
cat("Kurtosis:", Pots2Nup_kur, "\n") # -0.2916061
shapiro.test(Pots2$Nuptake) # p=0.1701
hist(Pots2$Nuptake) #  left skew
leveneTest(Nuptake~Treatment, data=Pots2)  # 0.7617
#ModP2Nup1 - all SE values are identical
ModP2Nup1 <- aov(Nuptake~Treatment+Block, data=Pots2)
anova(ModP2Nup1)
summary(ModP2Nup1)
hist(resid(ModP2Nup1))
shapiro.test(resid(ModP2Nup1))  # p=0.1458
plot(fitted(ModP2Nup1),resid(ModP2Nup1),pch=16) #slightly left  skewed but very random
qqnorm(resid(ModP2Nup1)) #left tail somewhat longer
qqline(resid(ModP2Nup1))
ModP2Nup1_tidy <- tidy(ModP2Nup1)
ModP2Nup1sum_sq_reg <- ModP2Nup1_tidy$sumsq[1] 
ModP2Nup1sum_sq_resid <- ModP2Nup1_tidy$sumsq[2]
ModP2Nup1sum_sq_reg / (ModP2Nup1sum_sq_reg + ModP2Nup1sum_sq_resid) # rsq =0.5212608
# lm model with weighted least squares
ModP2Nupvar <- tapply(Pots2$Nuptake, Pots2$Treatment, var) # Calculate the variances for each treatment
weightsNup <- 1 / ModP2Nupvar # Create a vector of weights
weightsNup_full <- rep(weightsNup, each = length(Pots2$Nuptake) / length(weightsNup))
ModP2Nup2 <- lm(Nuptake ~ Treatment + Block, data=Pots2, weights=weightsNup_full) # Fit a WLS model
anova(ModP2Nup2)
hist(resid(ModP2Nup2))  # slight left skew
shapiro.test(resid(ModP2Nup2))  # p=0.1592
plot(fitted(ModP2Nup2),resid(ModP2Nup2),pch=16) #slightly left  skewed but very random
qqnorm(resid(ModP2Nup2)) #left tail somewhat longer
qqline(resid(ModP2Nup2))
rsq(ModP2Nup2)  # 0.5543654
# ModP2Nup3 glmm 
ModP2Nup3<- glmmTMB(Nuptake~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2Nup3, type="III")
summary(ModP2Nup3)
shapiro.test(resid(ModP2Nup3)) # p=0.13
plot(fitted(ModP2Nup3),resid(ModP2Nup3),pch=16) # left cluster
qqnorm(resid(ModP2Nup3)) # moderate tail
qqline(resid(ModP2Nup3))
performance::r2(ModP2Nup3) # 0.517
#ModP2Nup4 - issue with df, no significance in emmeans
ModP2Nup4 <- lmer(Nuptake~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2Nup4) 
summary(ModP2Nup4)
shapiro.test(resid(ModP2Nup4)) # p=0.13
plot(fitted(ModP2Nup4),resid(ModP2Nup4),pch=16) # left and slightly below the mid-line
qqnorm(resid(ModP2Nup4)) # moderate left & slight right tail
qqline(resid(ModP2Nup4))
rsq(ModP2Nup4)  # 0.463
# ModP2Nup5 lme model - only 3 degrees of freedom, no significance in emmeans
ModP2Nup5 <- lme(Nuptake ~ Treatment, random=~1|Block, data=Pots2)
summary(ModP2Nup5)
anova(ModP2Nup5)
shapiro.test(resid(ModP2Nup5)) # p= 0.13
plot(fitted(ModP2Nup5),resid(ModP2Nup5),pch=16) # left and slightly below the mid-line
qqnorm(resid(ModP2Nup5)) # moderate left & slight right tail
qqline(resid(ModP2Nup5))
rsq(ModP2Nup5) # 0.463
#ModP2Nup6 glmer - infinite degrees of freedom
ModP2Nup6 <- glmer(Nuptake~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"))
anova(ModP2Nup6)
summary(ModP2Nup6)
shapiro.test(resid(ModP2Nup6)) # p=0.14
bf.test(Nuptake~Treatment, data=Pots2) # p=0.244, variances equal
plot(fitted(ModP2Nup6),resid(ModP2Nup6),pch=16) # left and slightly below the mid-line
qqnorm(resid(ModP2Nup6)) # moderate-heavy tails
qqline(resid(ModP2Nup6))
rsq(ModP2Nup6) # r=0.523

#AIC and BIC values
Pots2Bio_modlist <- list(ModP2Nup3, ModP2Nup4, ModP2Nup5, ModP2Nup6)
AIC_values <- sapply(Pots2Bio_modlist, AIC)
BIC_values <- sapply(Pots2Bio_modlist, BIC)
Pots2AICBio <- data.frame(Model=c("ModP2Nup3", "ModP2Nup4", "ModP2Nup5", "ModP2Nup6"), AIC_values, BIC_values)
print(Pots2AICBio)
#Model AIC_values BIC_values
#1 ModP2Nup3   316.0476   330.7050
#2 ModP2Nup4   260.0305   274.6878
#3 ModP2Nup5   260.0305   271.8110
#4 ModP2Nup6   311.8554   326.5128

#emmeans on glmm - on suitable model
ModP2emNup <- emmeans(ModP2Nup3,~Treatment, alpha = 0.1, type="response")
ModP2emNup_cld <- cld(ModP2emNup, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emNup_cld)
write.xlsx(ModP2emNup_cld, file="Pots2_Nuptake.xlsx")

# Plotting the summary data
(Pots2NuptakePlot <- ggplot(ModP2emNup_cld, aes(x=Treatment, y=emmean)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01, width=0.7)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=trimws(.group), y=emmean+SE), size=9, vjust=-0.5)+
  labs(x = "Treatment", y = "Wheat N uptake (ug/g soil)") +
  scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", 
                              "Meat and Bone\nMeal - Coarse", "Meat and Bone\nMeal - Fine", 
                              "Fertilizer\nPhosphorus"))+
  scale_y_continuous(limits = c(0, 210))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pots2NuptakePlot, file="Pots2_Nuptake.jpg", width = 8, height = 6, dpi = 150)



#####   P UPTAKE   #####
Pots2Pup_Mean <- summary_by(Puptake~Treatment+Block, data=Pots2, FUN=mean) 
Pots2Pup_Mean <- as.numeric(Pots2Pup_Mean$Puptake)
Pots2Pup_skew <- skewness(Pots2Pup_Mean,na.rm=TRUE)
Pots2Pup_kur <- kurtosis(Pots2Pup_Mean,na.rm=TRUE)
cat("Skewness:", Pots2Pup_skew, "\n") # -0.1449922 
cat("Kurtosis:", Pots2Pup_kur, "\n") # -0.8374298
shapiro.test(Pots2$Puptake) # p=0.5904
hist(Pots2$Puptake) 
leveneTest(Puptake~Treatment, data=Pots2)  # 0.1659
#ModP2Pup1
ModP2Pup1 <- aov(Puptake~Treatment+Block, data=Pots2)
anova(ModP2Pup1)
summary(ModP2Pup1)
hist(resid(ModP2Pup1))
shapiro.test(resid(ModP2Pup1))  # p=0.6417
plot(fitted(ModP2Pup1),resid(ModP2Pup1),pch=16) #slightly left skewed but very random
qqnorm(resid(ModP2Pup1))
qqline(resid(ModP2Pup1))
ModP2Pup1_tidy <- tidy(ModP2Pup1)
ModP2Pup1sum_sq_reg <- ModP2Pup1_tidy$sumsq[1] 
ModP2Pup1sum_sq_resid <- ModP2Pup1_tidy$sumsq[2]
ModP2Pup1sum_sq_reg / (ModP2Pup1sum_sq_reg + ModP2Pup1sum_sq_resid) # 0.7522534
# lm model with weighted least squares
ModP2Pupvar <- tapply(Pots2$Puptake, Pots2$Treatment, var, na.rm=TRUE)
weightsPup <- 1 / ModP2Pupvar
weightsPup_full <- rep(weightsPup, each = length(Pots2$Puptake) / length(weightsPup))
ModP2Pup2 <- lm(Puptake ~ Treatment + Block, data=Pots2, weights=weightsPup_full) 
hist(resid(ModP2Pup2))  # slight left skew
shapiro.test(resid(ModP2Pup2))  # p= 0.2411
plot(fitted(ModP2Pup2),resid(ModP2Pup2),pch=16) #slightly left  skewed but very random
qqnorm(resid(ModP2Pup2)) #left tail somewhat longer
qqline(resid(ModP2Pup2))
rsq(ModP2Pup2)  # 0.6712941
# ModP2Pup3 glmm - possible singularity issues
ModP2Pup3<- glmmTMB(Puptake~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2Pup3, type="III")
summary(ModP2Pup3)
shapiro.test(resid(ModP2Pup3)) # p=0.16
plot(fitted(ModP2Pup3),resid(ModP2Pup3),pch=16) # normal
qqnorm(resid(ModP2Pup3)) # moderate tails
qqline(resid(ModP2Pup3))
performance::r2(ModP2Pup3) # 0.709
#ModP2Pup4 - singular
ModP2Pup4 <- lmer(Puptake~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2Pup4) 
summary(ModP2Pup4)
shapiro.test(resid(ModP2Pup4)) # p=0.027
plot(fitted(ModP2Pup4),resid(ModP2Pup4),pch=16) # normal
qqnorm(resid(ModP2Pup4)) # heavy left & moderate right tail
qqline(resid(ModP2Pup4))
rsq(ModP2Pup4)  # 0.688
# ModP2Pup5 lme model - only 3 degrees of freedom
ModP2Pup5 <- lme(Puptake ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
summary(ModP2Pup5)
anova(ModP2Pup5)
shapiro.test(resid(ModP2Pup5)) # p= 0.16
plot(fitted(ModP2Pup5),resid(ModP2Pup5),pch=16) # normal
qqnorm(resid(ModP2Pup5)) # moderate tails
qqline(resid(ModP2Pup5))
rsq(ModP2Pup5) # NA
#ModP2Pup6 glmer - infinite degrees of freedom
ModP2Pup6 <- glmer(Puptake~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"), na.action=na.exclude)
anova(ModP2Pup6)
summary(ModP2Pup6)
shapiro.test(resid(ModP2Pup6)) # p=0.203
bf.test(Puptake~Treatment, data=Pots2) # p=0.541, variances equal
plot(fitted(ModP2Pup6),resid(ModP2Pup6),pch=16) # normal
qqnorm(resid(ModP2Pup6)) # slight left & moderate right tail
qqline(resid(ModP2Pup6))
rsq(ModP2Pup6) # can't get value

#AIC and BIC values
Pots2Pup_modlist <- list(ModP2Pup3, ModP2Pup4, ModP2Pup5, ModP2Pup6)
AIC_values <- sapply(Pots2Pup_modlist, AIC)
BIC_values <- sapply(Pots2Pup_modlist, BIC)
Pots2AICPup <- data.frame(Model=c("ModP2Pup3", "ModP2Pup4", "ModP2Pup5", "ModP2Pup6"), AIC_values, BIC_values)
print(Pots2AICPup)
#Model AIC_values BIC_values
#1 ModP2Pup3   190.7314   205.0713
#2 ModP2Pup4   164.3397   178.6796
#3 ModP2Pup5   164.3397   175.6947
#4 ModP2Pup6   194.0916   208.4315

#emmeans on lmer model
ModP2emPup <- emmeans(ModP2Pup4,~Treatment, alpha=0.1, type="response")
ModP2emPup_cld <- cld(ModP2emPup, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emPup_cld)
write.csv(ModP2emPup_cld, file="Pots2_Puptake.csv")

# Plotting the summary data
(Pots2PuptakePlot <- ggplot(ModP2emPup_cld, aes(x=Treatment, y=emmean)) +
    geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01, width=0.7)+
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE), size=9, vjust=-0.5)+
    labs(x = "Treatment", y = "Wheat P uptake (ug/g soil)") +
    scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", 
                              "Meat and Bone\nMeal - Coarse", "Meat and Bone\nMeal - Fine", 
                              "Fertilizer\nPhosphorus"))+
    scale_y_continuous(limits = c(0, 30))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(Pots2PuptakePlot, file="Pots2_Puptake.jpg", width = 8, height = 6, dpi = 150)


#####   SOIL NO3   #####
Pots2SNO3_Mean <- summary_by(SNO3~Treatment+Block, data=Pots2, FUN=mean) 
Pots2SNO3_Mean <- as.numeric(Pots2SNO3_Mean$SNO3)
Pots2SNO3_skew <- skewness(Pots2SNO3_Mean,na.rm=TRUE)
Pots2SNO3_kur <- kurtosis(Pots2SNO3_Mean,na.rm=TRUE)
cat("Skewness:", Pots2SNO3_skew, "\n") # 2.721196 
cat("Kurtosis:", Pots2SNO3_kur, "\n") # 9.18808 
shapiro.test(Pots2$SNO3) # p=2.965e-06
hist(Pots2$SNO3) #  extreme left skew
leveneTest(SNO3~Treatment, data=Pots2)  # P=0.3258
#trnasformations
shapiro.test(log(Pots2$SNO3)) # p=0.2487
hist(log(Pots2$SNO3)) #  left skew
leveneTest(log(SNO3)~Treatment, data=Pots2)  # P=0.2182
#ModP2SNO31
ModP2SNO31 <- aov(log(SNO3)~Treatment+Block, data=Pots2)
anova(ModP2SNO31)
summary(ModP2SNO31)
hist(resid(ModP2SNO31))
shapiro.test(resid(ModP2SNO31))  # p=0.07536
plot(fitted(ModP2SNO31),resid(ModP2SNO31),pch=16) 
qqnorm(resid(ModP2SNO31)) # long right tail
qqline(resid(ModP2SNO31))
ModP2SNO31_tidy <- tidy(ModP2SNO31)
ModP2SNO31sum_sq_reg <- ModP2SNO31_tidy$sumsq[1] 
ModP2SNO31sum_sq_resid <- ModP2SNO31_tidy$sumsq[2]
ModP2SNO31sum_sq_reg / (ModP2SNO31sum_sq_reg + ModP2SNO31sum_sq_resid) # rsq=0.3888387
# lm model
ModP2SNO32 <- lm(log(SNO3)~Treatment+Block, data=Pots2)
anova(ModP2SNO32)
summary(ModP2SNO32)
hist(resid(ModP2SNO32))
shapiro.test(resid(ModP2SNO32))  # p=0.07536
plot(fitted(ModP2SNO32),resid(ModP2SNO32),pch=16) 
qqnorm(resid(ModP2SNO32)) # long right tail
qqline(resid(ModP2SNO32))
rsq(ModP2SNO32) # rsq=0.6791344
# ModP2SNO33 glmm 
ModP2SNO33<- glmmTMB(SNO3~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2SNO33, type="III")
summary(ModP2SNO33)
shapiro.test(resid(ModP2SNO33)) # p=0.027
plot(fitted(ModP2SNO33),resid(ModP2SNO33),pch=16) # clustered below mid-line
qqnorm(resid(ModP2SNO33)) # moderate left & heavy right tail
qqline(resid(ModP2SNO33))
performance::r2(ModP2SNO33) # 0.439

#emmeans 
ModP2emSNO3 <- emmeans(ModP2SNO33,~Treatment, type="response")
ModP2emSNO3_cld <- cld(ModP2emSNO3, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emSNO3_cld)
write.csv(ModP2emSNO3_cld, file="Pots2_SoilNO3.csv")


#####   SOIL NH4   #####
Pots2SNH4_Mean <- summary_by(SNH4~Treatment+Block, data=Pots2, FUN=mean) 
Pots2SNH4_Mean <- as.numeric(Pots2SNH4_Mean$SNH4)
Pots2SNH4_skew <- skewness(Pots2SNH4_Mean,na.rm=TRUE)
Pots2SNH4_kur <- kurtosis(Pots2SNH4_Mean,na.rm=TRUE)
cat("Skewness:", Pots2SNH4_skew, "\n") # 1.083862 
cat("Kurtosis:", Pots2SNH4_kur, "\n") # 0.7036936 
shapiro.test(Pots2$SNH4) # p=0.003774
hist(Pots2$SNH4) 
leveneTest(SNH4~Treatment, data=Pots2)  # P=0.5493
#ModP2SNH41
ModP2SNH41 <- aov(SNH4~Treatment+Block, data=Pots2)
anova(ModP2SNH41)
summary(ModP2SNH41)
hist(resid(ModP2SNH41))
shapiro.test(resid(ModP2SNH41))  # p=0.435
plot(fitted(ModP2SNH41),resid(ModP2SNH41),pch=16) 
qqnorm(resid(ModP2SNH41))
qqline(resid(ModP2SNH41))
ModP2SNH41_tidy <- tidy(ModP2SNH41)
ModP2SNH41sum_sq_reg <- ModP2SNH41_tidy$sumsq[1] 
ModP2SNH41sum_sq_resid <- ModP2SNH41_tidy$sumsq[2]
ModP2SNH41sum_sq_reg / (ModP2SNH41sum_sq_reg + ModP2SNH41sum_sq_resid) #0.2443073
# lm model
ModP2SNH4var <- tapply(Pots2$SNH4, Pots2$Treatment, var, na.rm=TRUE)
weightsSNH4 <- 1 / ModP2SNH4var
weightsSNH4_full <- rep(weightsSNH4, each = length(Pots2$SNH4) / length(weightsSNH4))
ModP2SNH42 <- lm(SNH4 ~ Treatment + Block, data=Pots2, weights=weightsSNH4_full) 
anova(ModP2SNH42)
summary(ModP2SNH42)
hist(resid(ModP2SNH42))
shapiro.test(resid(ModP2SNH42))  # p=0.1163
plot(fitted(ModP2SNH42),resid(ModP2SNH42),pch=16) 
qqnorm(resid(ModP2SNH42)) 
qqline(resid(ModP2SNH42))
rsq(ModP2SNH42) # rsq=0.4700318
# ModP2SNH43 glmm 
ModP2SNH43<- glmmTMB(SNH4~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2SNH43, type="III")
summary(ModP2SNH43)
shapiro.test(resid(ModP2SNH43)) # p=0.27
plot(fitted(ModP2SNH43),resid(ModP2SNH43),pch=16) # normal
qqnorm(resid(ModP2SNH43)) # moderate right tail
qqline(resid(ModP2SNH43))
performance::r2(ModP2SNH43) # 0.54

#emmeans 
ModP2emSNH4 <- emmeans(ModP2SNH43,~Treatment, type="response")
ModP2emSNH4_cld <- cld(ModP2emSNH4, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emSNH4_cld)
write.csv(ModP2emSNH4_cld, file="Pots2_SoilNH4.csv")



#####   SOIL PO4   #####
Pots2SPO4_Mean <- summary_by(SPO4~Treatment+Block, data=Pots2, FUN=mean) 
Pots2SPO4_Mean <- as.numeric(Pots2SPO4_Mean$SPO4)
Pots2SPO4_skew <- skewness(Pots2SPO4_Mean,na.rm=TRUE)
Pots2SPO4_kur <- kurtosis(Pots2SPO4_Mean,na.rm=TRUE)
cat("Skewness:", Pots2SPO4_skew, "\n") # 0.7319427 
cat("Kurtosis:", Pots2SPO4_kur, "\n") # -0.7178281 
shapiro.test(Pots2$SPO4) # p=0.007283
hist(Pots2$SPO4) #  left skew
leveneTest(SPO4~Treatment, data=Pots2)  # P=0.5364
# transform
shapiro.test(log(Pots2$SPO4)) # p=0.1813
hist(log(Pots2$SPO4)) 
leveneTest(log(SPO4)~Treatment, data=Pots2)  # P=0.8856
#ModP2SPO41
ModP2SPO41 <- aov(log(SPO4)~Treatment+Block, data=Pots2)
anova(ModP2SPO41)
summary(ModP2SPO41)
hist(resid(ModP2SPO41))
shapiro.test(resid(ModP2SPO41))  # p= 0.3196
plot(fitted(ModP2SPO41),resid(ModP2SPO41),pch=16) 
qqnorm(resid(ModP2SPO41))
qqline(resid(ModP2SPO41))
ModP2SPO41_tidy <- tidy(ModP2SPO41)
ModP2SPO41sum_sq_reg <- ModP2SPO41_tidy$sumsq[1] 
ModP2SPO41sum_sq_resid <- ModP2SPO41_tidy$sumsq[2]
ModP2SPO41sum_sq_reg / (ModP2SPO41sum_sq_reg + ModP2SPO41sum_sq_resid) #0.8786474
# ModP2SPO43 glmm
ModP2SPO43<- glmmTMB(SPO4~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2SPO43, type="III")
summary(ModP2SPO43)
shapiro.test(resid(ModP2SPO43)) # p=0.36
plot(fitted(ModP2SPO43),resid(ModP2SPO43),pch=16) # normalish
qqnorm(resid(ModP2SPO43)) # slight left & moderate right tail
qqline(resid(ModP2SPO43))
performance::r2(ModP2SPO43) # Na, possible singularity effect

#emmeans 
ModP2emSPO4 <- emmeans(ModP2SPO43,~Treatment, type="response")
ModP2emSPO4_cld <- cld(ModP2emSPO4, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emSPO4_cld)
write.csv(ModP2emSPO4_cld, file="Pots2_SoilPO4.csv")



#####   SOIL RESIN P   #####
Pots2ResP_Mean <- summary_by(ResinP~Treatment+Block, data=Pots2, FUN=mean) 
Pots2ResP_Mean <- as.numeric(Pots2ResP_Mean$ResinP)
Pots2ResP_skew <- skewness(Pots2ResP_Mean,na.rm=TRUE)
Pots2ResP_kur <- kurtosis(Pots2ResP_Mean,na.rm=TRUE)
cat("Skewness:", Pots2ResP_skew, "\n") # 1.6256
cat("Kurtosis:", Pots2ResP_kur, "\n") # 3.175637  
shapiro.test(Pots2$ResinP) # p=0.0005627
hist(Pots2$ResinP) #  left skew
leveneTest(ResinP~Treatment, data=Pots2)  # P=0.0674
shapiro.test(log(Pots2$ResinP))  #p=0.2711
leveneTest(log(ResinP)~Treatment, data=Pots2)  # p=0.02545
#ModP2ResP1
ModP2ResP1 <- aov(log(ResinP)~Treatment+Block, data=Pots2)
anova(ModP2ResP1) # no significance
summary(ModP2ResP1)
hist(resid(ModP2ResP1))
shapiro.test(resid(ModP2ResP1))  # p=0.5466
plot(fitted(ModP2ResP1),resid(ModP2ResP1),pch=16)  # random
qqnorm(resid(ModP2ResP1))
qqline(resid(ModP2ResP1)) 
ModP2ResP1_tidy <- tidy(ModP2ResP1)
ModP2ResP1sum_sq_reg <- ModP2ResP1_tidy$sumsq[1] 
ModP2ResP1sum_sq_resid <- ModP2ResP1_tidy$sumsq[2]
ModP2ResP1sum_sq_reg / (ModP2ResP1sum_sq_reg + ModP2ResP1sum_sq_resid) #0.548
# ModP2ResP1 glmm 
ModP2ResP2<- glmmTMB(log(ResinP)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2ResP2, type="III")
summary(ModP2ResP2)
shapiro.test(resid(ModP2ResP2)) # p=0.456
plot(fitted(ModP2ResP2),resid(ModP2ResP2),pch=16) # normal
qqnorm(resid(ModP2ResP2)) # slight tails
qqline(resid(ModP2ResP1))
performance::r2(ModP2ResP2) # Na, possible singularity effect

#emmeans 
ModP2emResP1 <- emmeans(ModP2ResP2,~Treatment, type="response")
ModP2emResP1_cld <- cld(ModP2emNup, Letters = trimws(letters), reversed = TRUE) 
View(ModP2emResP1_cld)
write.csv(ModP2emResP1_cld, file="Pots2_resinP.csv")




#####   WATER SOLUBLE P   #####
Pots2WSP_Mean <- summary_by(WatSolP~Treatment+Block, data=Pots2, FUN=mean) 
Pots2WSP_Mean <- as.numeric(Pots2WSP_Mean$WatSolP)
Pots2WSP_skew <- skewness(Pots2WSP_Mean,na.rm=TRUE)
Pots2WSP_kur <- kurtosis(Pots2WSP_Mean,na.rm=TRUE)
cat("Skewness:", Pots2WSP_skew, "\n") # 1.575096 
cat("Kurtosis:", Pots2WSP_kur, "\n") # 3.261601 
shapiro.test(Pots2$WatSolP) # p=0.001116
hist(Pots2$WatSolP) #  left skew
leveneTest(WatSolP~Treatment, data=Pots2)  # P=0.3716
# transform
shapiro.test(log(Pots2$WatSolP)) # p=0.1204
hist(log(Pots2$WatSolP)) #  slight left skew
leveneTest(log(WatSolP)~Treatment, data=Pots2)  # P=0.4992
#ModP2WSP1
ModP2WSP1 <- aov(log(WatSolP)~Treatment+Block, data=Pots2)
anova(ModP2WSP1)
summary(ModP2WSP1)
hist(resid(ModP2WSP1))
shapiro.test(resid(ModP2WSP1))  # p=0.4645
plot(fitted(ModP2WSP1),resid(ModP2WSP1),pch=16) 
qqnorm(resid(ModP2WSP1))
qqline(resid(ModP2WSP1))
ModP2WSP1_tidy <- tidy(ModP2WSP1)
ModP2WSP1sum_sq_reg <- ModP2WSP1_tidy$sumsq[1] 
ModP2WSP1sum_sq_resid <- ModP2WSP1_tidy$sumsq[2]
ModP2WSP1sum_sq_reg / (ModP2WSP1sum_sq_reg + ModP2WSP1sum_sq_resid) #0.9799464
# ModP2WSP2 glmm 
ModP2WSP2<- glmmTMB(log(WatSolP)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2WSP2, type="III")
summary(ModP2WSP2)
shapiro.test(resid(ModP2WSP2)) # p=0.35
plot(fitted(ModP2WSP2),resid(ModP2WSP2),pch=16) # normal
qqnorm(resid(ModP2WSP2)) # slight tails
qqline(resid(ModP2WSP2))
performance::r2(ModP2WSP2) # Na, possible singularity effect

#emmeans 
ModP2emWSP <- emmeans(ModP2WSP2,~Treatment, type="response")
ModP2emWSP_cld <- cld(ModP2emWSP, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emWSP_cld)
write.csv(ModP2emWSP_cld, file="Pots2_WatSolP.csv")


#####   TOTAL P   #####
Pots2TotalP_Mean <- summary_by(TotalP~Treatment+Block, data=Pots2, FUN=mean) 
Pots2TotalP_Mean <- as.numeric(Pots2TotalP_Mean$TotalP)
Pots2TotalP_skew <- skewness(Pots2TotalP_Mean,na.rm=TRUE)
Pots2TotalP_kur <- kurtosis(Pots2TotalP_Mean,na.rm=TRUE)
cat("Skewness:", Pots2TotalP_skew, "\n") # 1.146251 
cat("Kurtosis:", Pots2TotalP_kur, "\n") # 0.6666898 
shapiro.test(Pots2$TotalP) # p=0.001441
hist(Pots2$TotalP) #  left skew
leveneTest(TotalP~Treatment, data=Pots2)  # P=0.202
# transform
shapiro.test(log(Pots2$TotalP)) # p=0.006947
hist(log(Pots2$TotalP)) #  left skew
leveneTest(log(TotalP)~Treatment, data=Pots2)  # P=0.1899
shapiro.test(log10(Pots2$TotalP)) # p=0.006947
hist(log10(Pots2$TotalP)) #  more normal
leveneTest(log10(TotalP)~Treatment, data=Pots2)  # P=0.1899
shapiro.test(sqrt(Pots2$TotalP)) # p=0.003228
hist(sqrt(Pots2$TotalP)) #  left skew
leveneTest(sqrt(TotalP)~Treatment, data=Pots2)  # P=0.1952
shapiro.test(1/(Pots2$TotalP)) # p=0.0271
hist(1/(Pots2$TotalP)) #  right skew
leveneTest(1/(TotalP)~Treatment, data=Pots2)  # P=0.1842
#ModP2TotalP1
ModP2TotalP1 <- aov(log(TotalP)~Treatment+Block, data=Pots2)
anova(ModP2TotalP1)
summary(ModP2TotalP1)
hist(resid(ModP2TotalP1))
shapiro.test(resid(ModP2TotalP1))  # p=0.8654
plot(fitted(ModP2TotalP1),resid(ModP2TotalP1),pch=16) 
qqnorm(resid(ModP2TotalP1))
qqline(resid(ModP2TotalP1))
ModP2TotalP1_tidy <- tidy(ModP2TotalP1)
ModP2TotalP1sum_sq_reg <- ModP2TotalP1_tidy$sumsq[1] 
ModP2TotalP1sum_sq_resid <- ModP2TotalP1_tidy$sumsq[2]
ModP2TotalP1sum_sq_reg / (ModP2TotalP1sum_sq_reg + ModP2TotalP1sum_sq_resid) #0.6312668
# ModP2TotalP2 glmm 
ModP2TotalP2<- glmmTMB(log(TotalP)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2TotalP2, type="III")
summary(ModP2TotalP2)
shapiro.test(resid(ModP2TotalP2)) # p=0.84
plot(fitted(ModP2TotalP2),resid(ModP2TotalP2),pch=16) # normal
qqnorm(resid(ModP2TotalP2)) # slight left & moderate-heavy right tails
qqline(resid(ModP2TotalP2))
performance::r2(ModP2TotalP2) # 0.261

#emmeans 
ModP2emTotalP <- emmeans(ModP2TotalP2,~Treatment, type="response")
ModP2emTotalP_cld <- cld(ModP2emTotalP, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emTotalP_cld)
write.csv(ModP2emTotalP_cld, file="Pots2_TotalP.csv")


#####   pH   #####
Pots2pH_Mean <- summary_by(pH~Treatment+Block, data=Pots2, FUN=mean) 
Pots2pH_Mean <- as.numeric(Pots2pH_Mean$pH)
Pots2pH_skew <- skewness(Pots2pH_Mean,na.rm=TRUE)
Pots2pH_kur <- kurtosis(Pots2pH_Mean,na.rm=TRUE)
cat("Skewness:", Pots2pH_skew, "\n") # 0.8153002 
cat("Kurtosis:", Pots2pH_kur, "\n") # -0.06151628  
shapiro.test(Pots2$pH) # p=0.01752
hist(Pots2$pH) # left skew
leveneTest(pH~Treatment, data=Pots2)  # P=0.9357
#transform
shapiro.test(log(Pots2$pH)) # p=0.02644
hist(log(Pots2$pH)) # left skew
leveneTest(log(pH)~Treatment, data=Pots2)  # P=0.9321
shapiro.test(sqrt(Pots2$pH)) # p=0.02156
hist(sqrt(Pots2$pH)) # left skew
leveneTest(sqrt(pH)~Treatment, data=Pots2)  # P=0.934
#ModP2pH1
ModP2pH1 <- aov(pH~Treatment+Block, data=Pots2)
anova(ModP2pH1)
summary(ModP2pH1)
hist(resid(ModP2pH1))
shapiro.test(resid(ModP2pH1))  # p=0.1862
plot(fitted(ModP2pH1),resid(ModP2pH1),pch=16) # clusters to each side
qqnorm(resid(ModP2pH1)) # medium tails
qqline(resid(ModP2pH1))
ModP2pH1_tidy <- tidy(ModP2pH1)
ModP2pH1sum_sq_reg <- ModP2pH1_tidy$sumsq[1] 
ModP2pH1sum_sq_resid <- ModP2pH1_tidy$sumsq[2]
ModP2pH1sum_sq_reg / (ModP2pH1sum_sq_reg + ModP2pH1sum_sq_resid) #0.05141096
# weighted lm model
ModP2pHvar <- tapply(Pots2$pH, Pots2$Treatment, var, na.rm=TRUE)
weightspH <- 1 / ModP2pHvar
weightspH_full <- rep(weightspH, each = length(Pots2$pH) / length(weightspH))
ModP2pH2 <- lm(pH ~ Treatment + Block, data=Pots2, weights=weightspH_full) 
anova(ModP2pH2)
summary(ModP2pH2)
hist(resid(ModP2pH2)) 
shapiro.test(resid(ModP2pH2))  # p=0.4605
plot(fitted(ModP2pH2),resid(ModP2pH2),pch=16) 
qqnorm(resid(ModP2pH2)) # tails, especially on right
qqline(resid(ModP2pH2))
rsq(ModP2pH2) # rsq=0.4528747
# ModP2pH3 glmm 
ModP2pH3<- glmmTMB(pH~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2pH3, type="III")
summary(ModP2pH3)
shapiro.test(resid(ModP2pH3)) # p=0.25
plot(fitted(ModP2pH3),resid(ModP2pH3),pch=16) # normal
qqnorm(resid(ModP2pH3)) # moderate-heavy tails
qqline(resid(ModP2pH3))
performance::r2(ModP2pH3) # 0.446

#emmeans 
ModP2empH1 <- emmeans(ModP2pH3,~Treatment, type="response")
ModP2empH1_cld <- cld(ModP2empH1, Letters = trimws(letters), reversed=TRUE) 
View(ModP2empH1_cld)
write.csv(ModP2empH1_cld, file="Pots2_pH.csv")



#####   ELECTRICAL CONDUCTIVITY   #####
Pots2EC_Mean <- summary_by(EC~Treatment+Block, data=Pots2, FUN=mean) 
Pots2EC_Mean <- as.numeric(Pots2EC_Mean$EC)
Pots2EC_skew <- skewness(Pots2EC_Mean,na.rm=TRUE)
Pots2EC_kur <- kurtosis(Pots2EC_Mean,na.rm=TRUE)
cat("Skewness:", Pots2EC_skew, "\n") # 1.293181 
cat("Kurtosis:", Pots2EC_kur, "\n") # 0.7705139
hist(Pots2$EC) # slight left skew
shapiro.test(Pots2$EC) # p=0.0001406
leveneTest(EC~Treatment, data=Pots2)  # P=0.8016
# transform
shapiro.test(log(Pots2$EC))  # p=0.05701
leveneTest(log(EC)~Treatment, data=Pots2)  # p =  0.7405
shapiro.test(log10(Pots2$EC))  # p=0.05701
leveneTest(log10(EC)~Treatment, data=Pots2)  # p =  0.7405
shapiro.test(sqrt(Pots2$EC))  # p=0.003086
leveneTest(sqrt(EC)~Treatment, data=Pots2)  # p = 0.7786
#ModP2EC1
ModP2EC1 <- aov(log(EC)~Treatment+Block, data=Pots2)
anova(ModP2EC1)
summary(ModP2EC1)
hist(resid(ModP2EC1))
shapiro.test(resid(ModP2EC1))  # p=0.1893
plot(fitted(ModP2EC1),resid(ModP2EC1),pch=16) 
qqnorm(resid(ModP2EC1)) # medium right tail
qqline(resid(ModP2EC1))
ModP2EC1_tidy <- tidy(ModP2EC1)
ModP2EC1sum_sq_reg <- ModP2EC1_tidy$sumsq[1] 
ModP2EC1sum_sq_resid <- ModP2EC1_tidy$sumsq[2]
ModP2EC1sum_sq_reg / (ModP2EC1sum_sq_reg + ModP2EC1sum_sq_resid) # 0.3595186
# weighted lm model
ModP2ECvar <- tapply(Pots2$EC, Pots2$Treatment, var, na.rm=TRUE)
weightsEC <- 1 / ModP2ECvar
weightsEC_full <- rep(weightsEC, each = length(Pots2$EC) / length(weightsEC))
ModP2EC2 <- lm(EC ~ Treatment + Block, data=Pots2, weights=weightsEC_full) 
anova(ModP2EC2)
summary(ModP2EC2)
hist(resid(ModP2EC2)) # left skew
shapiro.test(resid(ModP2EC2))  # p=0.002185
plot(fitted(ModP2EC2),resid(ModP2EC2),pch=16) # cluster towards middel bottom
qqnorm(resid(ModP2EC2)) # big right tail
qqline(resid(ModP2EC2))
rsq(ModP2EC2) # rsq=0.5976484
# ModP2EC2 glmm 
ModP2EC3<- glmmTMB(EC~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2EC3, type="III")
summary(ModP2EC3)
shapiro.test(resid(ModP2EC3)) # p=0.004
plot(fitted(ModP2EC3),resid(ModP2EC3),pch=16) # clusrtered below mid-line
qqnorm(resid(ModP2EC3)) # slight left & very heavy right tail
qqline(resid(ModP2EC3))
performance::r2(ModP2EC3) # 0.39
#ModP2EC4  - issue with degrees of freedom
ModP2EC4 <- lmer(EC~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2EC4) 
summary(ModP2EC4)
shapiro.test(resid(ModP2EC4)) # p=0.0039
plot(fitted(ModP2EC4),resid(ModP2EC4),pch=16) # clustered below mid-line
qqnorm(resid(ModP2EC4)) # very heavy right tail
qqline(resid(ModP2EC4))
rsq(ModP2EC4)  # 0.332
# ModP2EC5 lme model - only 3 degrees of freedom
ModP2EC5 <- lme(EC ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
summary(ModP2EC5)
anova(ModP2EC5)
shapiro.test(resid(ModP2EC5)) # p= 0.0039
plot(fitted(ModP2EC5),resid(ModP2EC5),pch=16) #  clustered below mid-line
qqnorm(resid(ModP2EC5)) # very heavy right tail
qqline(resid(ModP2EC5))
rsq(ModP2EC5) # 0332
#ModP2EC6 glmer - infinite degrees of freedom
ModP2EC6 <- glmer(EC~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"), na.action=na.exclude)
anova(ModP2EC6)
summary(ModP2EC6)
shapiro.test(resid(ModP2EC6)) # p=0.062
bf.test(EC~Treatment, data=Pots2) # p=0.671, variances equal
plot(fitted(ModP2EC6),resid(ModP2EC6),pch=16) # slight cluster to left and below mid-line
qqnorm(resid(ModP2EC6)) # slight left & heavy right tail
qqline(resid(ModP2EC6))
rsq(ModP2EC6) # 0.376

#emmeans 
ModP2emEC1 <- emmeans(ModP2EC5,~Treatment, type="response")
ModP2emEC1_cld <- cld(ModP2emEC1, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emEC1_cld)
write.csv(ModP2emEC1_cld, file="Pots2_EC.csv")



#####   ORGANIC CARBON   #####
Pots2OC_Mean <- summary_by(OC~Treatment+Block, data=Pots2, FUN=mean) 
Pots2OC_Mean <- as.numeric(Pots2OC_Mean$OC)
Pots2OC_skew <- skewness(Pots2OC_Mean,na.rm=TRUE)
Pots2OC_kur <- kurtosis(Pots2OC_Mean,na.rm=TRUE)
cat("Skewness:", Pots2OC_skew, "\n") # 2.122192 
cat("Kurtosis:", Pots2OC_kur, "\n") # 4.694283 
shapiro.test(Pots2$OC) # p=5.96e-06
hist(Pots2$OC) #  heavy left skew
leveneTest(OC~Treatment, data=Pots2)  # P=1.097e-05
#Transforming
shapiro.test(log(Pots2$OC)) # p=0.0002123
leveneTest(log(OC)~Treatment, data=Pots2)  # P=0.0008862
shapiro.test(log10(Pots2$OC)) # p=0.0002123
leveneTest(log10(OC)~Treatment, data=Pots2)  # P=0.0008862
shapiro.test(sqrt(Pots2$OC)) # p=3.431e-05
leveneTest(sqrt(OC)~Treatment, data=Pots2)  # P=0.000106 
#ModP2OC1
ModP2OC1 <- aov(log(OC)~Treatment+Block, data=Pots2)
anova(ModP2OC1)
summary(ModP2OC1)
hist(resid(ModP2OC1))
shapiro.test(resid(ModP2OC1))  # p=0.3491
plot(fitted(ModP2OC1),resid(ModP2OC1),pch=16) # heavy left cluster
qqnorm(resid(ModP2OC1)) # slight left tail
qqline(resid(ModP2OC1))
ModP2OC1_tidy <- tidy(ModP2OC1)
ModP2OC1sum_sq_reg <- ModP2OC1_tidy$sumsq[1] 
ModP2OC1sum_sq_resid <- ModP2OC1_tidy$sumsq[2]
ModP2OC1sum_sq_reg / (ModP2OC1sum_sq_reg + ModP2OC1sum_sq_resid) # 0.9227297
# ModP2OC2 glmm - singularity issues
ModP2OC2<- glmmTMB(OC~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2OC2, type="III")
summary(ModP2OC2)
shapiro.test(resid(ModP2OC2)) # p=0.44
plot(fitted(ModP2OC2),resid(ModP2OC2),pch=16) # clustered to left, equal around mid-line
qqnorm(resid(ModP2OC2)) # moderate tails
qqline(resid(ModP2OC2))
performance::r2(ModP2OC2) # NA
#ModP2OC3  - singular
ModP2OC3 <- lmer(OC~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
# ModP2OC4 lme model
ModP2OC4 <- lme(OC ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
summary(ModP2OC4)
anova(ModP2OC4)
shapiro.test(resid(ModP2OC4)) # p= 0.44
plot(fitted(ModP2OC4),resid(ModP2OC4),pch=16) #  clustered to left, equal around mid-line
qqnorm(resid(ModP2OC4)) # moderate tails
qqline(resid(ModP2OC4))
rsq(ModP2OC4) # 0.588
#ModP2OC5 glmer
ModP2OC5 <- glmer(OC~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"), na.action=na.exclude)
anova(ModP2OC5)
summary(ModP2OC5)
shapiro.test(resid(ModP2OC5)) # p=0.86
bf.test(OC~Treatment, data=Pots2) # p=0.0.25, variances unequal
plot(fitted(ModP2OC5),resid(ModP2OC5),pch=16) # cluster to left 
qqnorm(resid(ModP2OC5)) # slight tails
qqline(resid(ModP2OC5))
rsq(ModP2OC5) # 0.609

#emmeans on glmer - higher rsq value
ModP2emOC1 <- emmeans(ModP2OC5,~Treatment, type="response")
ModP2emOC1_cld <- cld(ModP2emNup, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emOC1_cld)
write.csv(ModP2emOC1_cld, file="Pots2_OC.csv")



#####   LEACHATE PO4   #####
Pots2LPO4_Mean <- summary_by(LPO4~Treatment*Block, data=Pots2, FUN=mean) 
Pots2LPO4_Mean <- as.numeric(Pots2LPO4_Mean$LPO4)
Pots2LPO4_skew <- skewness(Pots2LPO4_Mean,na.rm=TRUE)
Pots2LPO4_kur <- kurtosis(Pots2LPO4_Mean,na.rm=TRUE)
cat("Skewness:", Pots2LPO4_skew, "\n") # 0.9148318 
cat("Kurtosis:", Pots2LPO4_kur, "\n") # -0.2687111 
shapiro.test(Pots2$LPO4) # p=0.0003759
hist(Pots2$LPO4) # heavy left skew
leveneTest(LPO4~Treatment, data=Pots2)  # P=0.1088
# transform
shapiro.test(log(Pots2$LPO4)) # p=0.06458
hist(log(Pots2$LPO4)) # slight right skew
leveneTest(log(LPO4)~Treatment, data=Pots2)  # P=0.5318
#ModP2LPO41
ModP2LPO41 <- aov(log(LPO4)~Treatment+Block, data=Pots2)
anova(ModP2LPO41)
summary(ModP2LPO41)
hist(resid(ModP2LPO41))
shapiro.test(resid(ModP2LPO41))  # p=0.6565
plot(fitted(ModP2LPO41),resid(ModP2LPO41),pch=16) 
qqnorm(resid(ModP2LPO41)) #slight left tail
qqline(resid(ModP2LPO41))
ModP2LPO41_tidy <- tidy(ModP2LPO41)
ModP2LPO41sum_sq_reg <- ModP2LPO41_tidy$sumsq[1] 
ModP2LPO41sum_sq_resid <- ModP2LPO41_tidy$sumsq[2]
ModP2LPO41sum_sq_reg / (ModP2LPO41sum_sq_reg + ModP2LPO41sum_sq_resid) #0.9115165
# glmm model - singularity issues
ModP2LPO42<- glmmTMB(log(LPO4)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2LPO42, type="III")
summary(ModP2LPO42)
shapiro.test(resid(ModP2LPO42)) # p=0.349
plot(fitted(ModP2LPO42),resid(ModP2LPO42),pch=16) # normal, equal around mid-line
qqnorm(resid(ModP2LPO42)) # moderate tails
qqline(resid(ModP2LPO42))
performance::r2(ModP2LPO42) # NA
#ModP2LPO43  - singular
ModP2LPO43 <- lmer(log(LPO4)~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2LPO43) 
summary(ModP2LPO43)
shapiro.test(resid(ModP2LPO43)) # p=0.35
plot(fitted(ModP2LPO43),resid(ModP2LPO43),pch=16) # normal
qqnorm(resid(ModP2LPO43)) # slight-moderate tails
qqline(resid(ModP2LPO43))
rsq(ModP2LPO43)  # 0.423
# ModP2LPO44 lme model - only 3 degrees of freedom
ModP2LPO44 <- lme(log(LPO4) ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
summary(ModP2LPO44)
anova(ModP2LPO44)
shapiro.test(resid(ModP2LPO44)) # p= 0.35
plot(fitted(ModP2LPO44),resid(ModP2LPO44),pch=16) #  normal
qqnorm(resid(ModP2LPO44)) # moderate tails
qqline(resid(ModP2LPO44))
rsq(ModP2LPO44) # NA
#ModP2LPO45 glmer - infinite degrees of freedom
ModP2LPO45 <- glmer(log(LPO4)~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"), na.action=na.exclude)
anova(ModP2LPO45)
summary(ModP2LPO45)
shapiro.test(resid(ModP2LPO45)) # p=0.143
bf.test(LPO4~Treatment, data=Pots2) # p=0.16, variances equal
plot(fitted(ModP2LPO45),resid(ModP2LPO45),pch=16) # normal
qqnorm(resid(ModP2LPO45)) # heavy tails
qqline(resid(ModP2LPO45))
rsq(ModP2LPO45) # N/A

#emmeans on glmm - least issues
ModP2emLPO4 <- emmeans(ModP2LPO42,~Treatment, alpha=0.1, type="response")
ModP2emLPO4_cld <- cld(ModP2emLPO4, Letters = trimws(letters), reversed = TRUE) 
View(ModP2emLPO4_cld)
write.csv(ModP2emLPO4_cld, file="Pots2_LPO4.csv")



#####   LEACHATE NO3   #####
Pots2LNO3_Mean <- summary_by(LNO3~Treatment+Block, data=Pots2, FUN=mean) 
Pots2LNO3_Mean <- as.numeric(Pots2LNO3_Mean$LNO3)
Pots2LNO3_skew <- skewness(Pots2LNO3_Mean,na.rm=TRUE)
Pots2LNO3_kur <- kurtosis(Pots2LNO3_Mean,na.rm=TRUE)
cat("Skewness:", Pots2LNO3_skew, "\n") # 1.3047 
cat("Kurtosis:", Pots2LNO3_kur, "\n") # 0.5656862 
shapiro.test(Pots2$LNO3) # p=6.908e-05
hist(Pots2$LNO3) #  heavy left skew
leveneTest(LNO3~Treatment, data=Pots2)  # P=0.2194
#transform
shapiro.test(log(Pots2$LNO3)) # p=0.005626
hist(log(Pots2$LNO3)) #  heavy right skew
leveneTest(log(LNO3)~Treatment, data=Pots2)  # P=0.7185
shapiro.test(sqrt(Pots2$LNO3)) # p=0.06668
hist(sqrt(Pots2$LNO3)) #  slight left skew
leveneTest(sqrt(LNO3)~Treatment, data=Pots2)  # P=0.5208
#ModP2LNO31
ModP2LNO31 <- aov(sqrt(LNO3)~Treatment+Block, data=Pots2)
anova(ModP2LNO31)
summary(ModP2LNO31, level=0.1)
hist(resid(ModP2LNO31))
shapiro.test(resid(ModP2LNO31))  # p=0.45
plot(fitted(ModP2LNO31),resid(ModP2LNO31),pch=16) # slight left cluster
qqnorm(resid(ModP2LNO31)) # slight tails
qqline(resid(ModP2LNO31))
ModP2LNO31_tidy <- tidy(ModP2LNO31)
ModP2LNO31sum_sq_reg <- ModP2LNO31_tidy$sumsq[1] 
ModP2LNO31sum_sq_resid <- ModP2LNO31_tidy$sumsq[2]
ModP2LNO31sum_sq_reg / (ModP2LNO31sum_sq_reg + ModP2LNO31sum_sq_resid) # 0.9095553
# glmm model - singularity issues in rsq
ModP2LNO32<- glmmTMB(sqrt(LNO3)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2LNO32, type="III")
summary(ModP2LNO32)
shapiro.test(resid(ModP2LNO32)) # p=0.79
plot(fitted(ModP2LNO32),resid(ModP2LNO32),pch=16) # normal, equal around mid-line
qqnorm(resid(ModP2LNO32)) # almost no tails
qqline(resid(ModP2LNO32))
performance::r2(ModP2LNO32) # NA
#ModP2LNO33  - singular 
ModP2LNO33 <- lmer(sqrt(LNO3)~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2LNO33) 
summary(ModP2LNO33)
shapiro.test(resid(ModP2LNO33)) # p=0.79
plot(fitted(ModP2LNO33),resid(ModP2LNO33),pch=16) # normal
qqnorm(resid(ModP2LNO33)) # almost no tails
qqline(resid(ModP2LNO33))
rsq(ModP2LNO33)  # 0.317
# ModP2LNO34 lme model - only 3 degrees of freedom
ModP2LNO34 <- lme(sqrt(LNO3) ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
summary(ModP2LNO34)
anova(ModP2LNO34)
shapiro.test(resid(ModP2LNO34)) # p= 0.79
plot(fitted(ModP2LNO34),resid(ModP2LNO34),pch=16) #  normal
qqnorm(resid(ModP2LNO34)) # almost no tails
qqline(resid(ModP2LNO34))
rsq(ModP2LNO34) # 0.32
#ModP2LNO35 glmer - singular, infinite degrees of freedom
ModP2LNO35 <- glmer(sqrt(LNO3)~Treatment+(1|Block),data=Pots2,family=gaussian(link="log"), na.action=na.exclude)
anova(ModP2LNO35)
summary(ModP2LNO35)
shapiro.test(resid(ModP2LNO35)) # p=0.79
bf.test(sqrt(LNO3)~Treatment, data=Pots2) # p=0.205, variances equal
plot(fitted(ModP2LNO35),resid(ModP2LNO35),pch=16) # normal
qqnorm(resid(ModP2LNO35)) # almost no tails
qqline(resid(ModP2LNO35))
rsq(ModP2LNO35) # 317

#emmeans on glmm as it's the most suitable, no singularity, decent df
ModP2emLNO3 <- emmeans(ModP2LNO32,~Treatment, type="response") # check at 10% level and still no sig dif
ModP2emLNO3_cld <- cld(ModP2emLNO3, Letters = trimws(letters), alpha=0.1, reversed = TRUE) 
View(ModP2emLNO3_cld)
write.csv(ModP2emLNO3_cld, file="Pots2_LNO3.csv")


#####   LEACHATE NH4   #####
Pots2LNH4_Mean <- summary_by(LNH4~Treatment+Block, data=Pots2, FUN=mean) 
Pots2LNH4_Mean <- as.numeric(Pots2LNH4_Mean$LNH4)
Pots2LNH4_skew <- skewness(Pots2LNH4_Mean,na.rm=TRUE)
Pots2LNH4_kur <- kurtosis(Pots2LNH4_Mean,na.rm=TRUE)
cat("Skewness:", Pots2LNH4_skew, "\n") # 1.734622 
cat("Kurtosis:", Pots2LNH4_kur, "\n") # 3.487211
shapiro.test(Pots2$LNH4) # p=0.0001512
hist(Pots2$LNH4) #  heavy left skew
leveneTest(LNH4~Treatment, data=Pots2)  # P=0.01512 
# transform
shapiro.test(log(Pots2$LNH4)) # p=0.6571
hist(log(Pots2$LNH4)) #  slight left skew
leveneTest(log(LNH4)~Treatment, data=Pots2)  # P=0.006684
shapiro.test(sqrt(Pots2$LNH4)) # p=0.02646
hist(sqrt(Pots2$LNH4)) #  slight left skew
leveneTest(sqrt(LNH4)~Treatment, data=Pots2)  # P=0.005966
#ModP2LNH41
ModP2LNH41 <- aov(log(LNH4)~Treatment+Block, data=Pots2)
anova(ModP2LNH41)
summary(ModP2LNH41)
hist(resid(ModP2LNH41))
shapiro.test(resid(ModP2LNH41))  # p=0.9384
plot(fitted(ModP2LNH41),resid(ModP2LNH41),pch=16) 
qqnorm(resid(ModP2LNH41))
qqline(resid(ModP2LNH41))
ModP2LNH41_tidy <- tidy(ModP2LNH41)
ModP2LNH41sum_sq_reg <- ModP2LNH41_tidy$sumsq[1] 
ModP2LNH41sum_sq_resid <- ModP2LNH41_tidy$sumsq[2]
ModP2LNH41sum_sq_reg / (ModP2LNH41sum_sq_reg + ModP2LNH41sum_sq_resid) # 0.6988937
# glmm model
ModP2LNH42<- glmmTMB(log(LNH4)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModP2LNH42, type="III")
summary(ModP2LNH42)
shapiro.test(resid(ModP2LNH42)) # p=0.96
plot(fitted(ModP2LNH42),resid(ModP2LNH42),pch=16) # normal
qqnorm(resid(ModP2LNH42)) # slight tails
qqline(resid(ModP2LNH42))
performance::r2(ModP2LNH42) # 0.47
#ModP2LNH43  - issue with degrees of freedom
ModP2LNH43 <- lmer(log(LNH4)~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
anova(ModP2LNH43) 
summary(ModP2LNH43)
shapiro.test(resid(ModP2LNH43)) # p=0.96
plot(fitted(ModP2LNH43),resid(ModP2LNH43),pch=16) # normal
qqnorm(resid(ModP2LNH43)) # slight tails
qqline(resid(ModP2LNH43))
rsq(ModP2LNH43)  # 0.44
# ModP2LNH44 lme model - only 3 degrees of freedom
ModP2LNH44 <- lme(log(LNH4) ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
summary(ModP2LNH44)
anova(ModP2LNH44)
shapiro.test(resid(ModP2LNH44)) # p= 0.96
plot(fitted(ModP2LNH44),resid(ModP2LNH44),pch=16) #  normal
qqnorm(resid(ModP2LNH44)) # slight tails
qqline(resid(ModP2LNH44))
rsq(ModP2LNH44) # 0.44

#emmeans on glmm - only suitable model
ModP2emLNH4 <- emmeans(ModP2LNH42,~Treatment, type="response")
ModP2emLNH4_cld <- cld(ModP2emLNH4, alpha=0.1, Letters = trimws(letters), reversed=TRUE) 
View(ModP2emLNH4_cld)
write.csv(ModP2emLNH4_cld, file="Pots2_LNH4.csv")


####  CORRELATION OF P FRACTIONS  ####
    ## comparison of the P fractions overall is obvious, need to separate by treatment
  #Pots2PCorMatrix <- Pots2[complete.cases(Pots2), c("SPO4", "ResinP", "WatSolP", "TotalP", "Puptake", "LPO4")]
    # full matrix
  #Pots2PCor <- cor(Pots2PCorMatrix)
  #View(Pots2PCor)
    #select only LPO4 in the row
  #Pots2PCorSub<-cor(Pots2PCor[, 5:6], Pots2PCor[, 1:5], method = "spearman", use = "pairwise.complete.obs")
  #colnames(Pots2PCorSub)[1:5] <- c("Soil PO4", "Soil Resin P", "Soil Soluble P", "Soil Total P", "Crop P uptake")
  #rownames(Pots2PCorSub)[1:2] <- c("Crop P uptake", "Leachate PO4")
  #Pots2PCorSub[1,5] <- NA
  #View(Pots2PCorSub)
    # heatmap & correllelogram requires at least 2 columns and two rows
  #Correlellogram
    #jpeg("Pots2_Pcorplot.jpg", width = 8, height = 5, units = "in", res = 300)
    #corrplot(Pots2PCorSub, method = "circle", addCoef.col="black", tl.col = "black", mar = c(1,1,1,1), na.label = " ",
     #        col=viridis(n = 100, option = "D"))
    #dev.off()

## Plotting only Leachate PO4 against soil P - decided not to use this as the results are obvious
    # Pots2PCorMatrix2 <- Pots2[complete.cases(Pots2), c("SPO4", "ResinP", "WatSolP", "TotalP", "LPO4")]
    # # full matrix
    # Pots2PCor2 <- cor(Pots2PCorMatrix2)
    # View(Pots2PCor2)
    # #select only LPO4 in the row
    # Pots2PCorSub2<-cor(x=Pots2PCor2[5,], y=Pots2PCor2[,1:4], method = "spearman", use = "pairwise.complete.obs")
    # colnames(Pots2PCorSub2)[1:4] <- c("Soil PO4", "Soil Resin P", "Soil Soluble P", "Soil Total P")
    # rownames(Pots2PCorSub2)[1] <- c("Leachate\nPO4")
    # print(Pots2PCorSub2)
    #Correlellogram - cannot be properly manipulated
      #jpeg("Pots2_Pcorplot_LPO4.jpg", width = 8, height = 5, units = "in", res = 150)
      #corrplot(Pots2PCorSub2, method = "circle", addCoef.col="black", tl.col = "black", mar = c(0,0.2,0,0.2), na.label = " ",
      #       col=viridis(n = 100, option = "D"), tl.srt = 45)
      #title(main = "Correlation of leachte P to residual soil P fractions", cex.main = 1.5, line=-1)
      #scale_fill_gradientn(colors=COL2('BrBG'), breaks = seq(-1, 1, by = 0.5))
      #dev.off()
    # ggplot to manipulate the plot
    # Pots2PCorDF <- data.frame(Var1 = colnames(Pots2PCorSub2), Var2 = rownames(Pots2PCorSub2), Correlation = as.numeric(Pots2PCorSub2))
    # print(Pots2PCorDF)
    # (Pots2_Pcorplot_LPO4 <- ggplot(Pots2PCorDF, aes(x = Var1, y = Var2, fill = Correlation)) +
    #     geom_point(data=Pots2PCorDF, aes(size=abs(Correlation)*20), shape=21) + #set size of correlation circles
    #     scale_size(range = c(30,45)) +
    #     scale_fill_gradientn(colors=brewer.pal(9, "PiYG"), limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    #     geom_text(aes(label=sprintf("%.3f", Correlation)), size=7)+
    #     theme(plot.title=element_text(hjust=0.5, face="bold", size=22),
    #           legend.text=element_text(size=12),
    #           legend.title=element_text(size=16, face="bold"),
    #           legend.key.siz=unit(15,"mm"),
    #           axis.title.y=element_text(size=14, colour="black", face="bold"),
    #           axis.title.x=element_text(size=14, colour="black", face="bold"),
    #           axis.text.y=element_blank(),
    #           axis.text.x=element_blank(), #no labels
    #           axis.ticks.y=element_blank(),
    #           axis.ticks.x=element_blank(),
    #           panel.background=element_blank(),  # remove gray background
    #           panel.spacing.x=unit(1, "cm"),
    #           plot.margin=margin(5, 5, 5, 5))+
    #     guides(size = "none")+
    #     labs(x="", y="", title=expression("Percentage P Recovery - Soil residual PO"[4])))
    # plot(Pots2_Pcorplot_LPO4)
    # ggsave(Pots2_Pcorplot_LPO4, file="Pots2_Pcorplot_LPO4a.jpg", width=8, height=5, dpi=100)
  
## plot soil PO4 against leachte Po4 per treatment
Pots2PCorPdf <- data.frame(Treatment=Pots2$Treatment,
                           SPO4=Pots2$SPO4,
                           LPO4=Pots2$LPO4)
print(Pots2PCorPdf)
Pots2PCor3 <- Pots2PCorPdf %>%
  group_by(Treatment) %>%
  summarize(Correlation = cor(LPO4, SPO4, use = "complete.obs"), .groups = "drop")
Pots2PCor3$Treatment <- factor(Pots2PCor3$Treatment, levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow",
                                                              "MBMACoarse", "MBMAFine", "Phosphorus"),
                                 labels=c("Control 1", "Control 2", "Canola\nMeal", "Manure", "Willow", 
                                          "Meat and\nBoneMeal -\nCoarse", "Meat and\nBonemeal-\nFine", 
                                          "TSP\nFertilizer"))
print(Pots2PCor3)
# visualize correlation using ggplot - try 2
(Pots2_Pcorplot2 <- ggplot(Pots2PCor3, aes(x=Treatment, y=0.5, fill=Correlation)) +
    geom_point(data=Pots2PCor3, aes(size=abs(Correlation)*20), shape=21) + #set size of correlation circles
    scale_size(range = c(15,45)) +
    scale_fill_gradientn(colors=viridis(n = 100, option = "D"), limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f",Correlation), color = ifelse(Correlation < 0, "white", "black")), size = 6)+
    scale_color_manual(values = c("black", "white"))+
    theme(plot.title=element_text(hjust=0.5, face="bold", size=22, vjust = -0.5),
          legend.text=element_text(size=12),
          legend.title=element_text(size=16, face="bold"),
          legend.key.siz=unit(15,"mm"),
          axis.title.y=element_text(size=14, colour="black", face="bold"),
          axis.title.x=element_text(size=14, colour="black", face="bold"),
          axis.text.y=element_blank(),
          axis.text.x=element_text(angle=45, size=19, colour="black"),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background=element_blank(),  # remove gray background
          panel.spacing.x=unit(1, "cm"),
          plot.margin=margin(5, 5, 5, 5))+
    guides(size = "none", color="none")+
    labs(x="", y="", title= bquote(bold("Leachate PO"[4] ~ "-" ~ "Soil residual PO"[4]))))
ggsave(Pots2_Pcorplot2, file="Pots2_Pcorplot_LPO4.jpg", width=14, height=5, dpi=100)

# Plotting Leachate NO3, PO4 and NH4 load
#create and combine data frames for the three emmeans functions
P2emNO3 <- as.data.frame(ModP2emLNO3_cld)
P2emNH4 <- as.data.frame(ModP2emLNH4_cld)
P2emPO4 <- as.data.frame(ModP2emLPO4_cld)
P2em_labels <- list("EM1" = "NO3", "EM2" = "NH4", "EM3" = "PO4")
P2em_all <- bind_Pots2(list(EM1 = P2emNO3, EM2 = P2emNH4, EM3 = P2emPO4), .id = "EM") 
P2em_all$EM <- factor(P2em_all$EM, levels = names(em_labels), labels = unlist(em_labels))
P2em_all <- P2em_all %>% rename(emmean = "response")
View(P2em_all)
# define function to calculate position adjustment for secondary axis
(LeachatePlot <- ggplot(P2em_all, aes(x=Treatment, y=emmean, pattern=EM)) +
    geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white",
                   pattern_density=0.005, pattern_spacing=0.01)+
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) + 
    scale_pattern_manual(values = c("NO3" = "stripe", "NH4" = "crosshatch", "PO4" = "wave"))+
    facet_wrap(~ EM, scales = "free_y", ) +
    labs(y="Nutrient load in leachte (ug/g)") +
    scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", "MBMA Coarse",
                               "MBMA Fine", "TSP"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(LeachatePlot, file="Pots2_Leachate.jpg", width = 8, height = 8, dpi = 150)




####  Covariance heat maps  ####
#####   Yield  #####
Pots2CovVar <- c("Biomass", "SNO3", "SNH4", "SPO4", "ResinP", "WatSolP", "TotalP", "pH", "EC", "OC")
Pots2CovYield <- subset(Pots2, select=c("Treatment", Pots2CovVar), 
                       na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
View(Pots2CovYield)
Pots2CovScaleYield <- as.data.frame(scale(Pots2CovYield[,-1])) # remove treatment to be able to scale
Pots2CovScaleYield$Treatment <- Pots2CovYield$Treatment # put treatment back
Pots2CovYieldSplit <- split(Pots2CovScaleYield[, -ncol(Pots2CovScaleYield)], Pots2CovScaleYield$Treatment)
YieldCov_Pots2 <- lapply(Pots2CovYieldSplit, function(x) cov(x, use="pairwise.complete.obs"))
YieldCovPots2Wb <- createWorkbook() 
for (i in seq_along(YieldCov_Pots2)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(YieldCov_Pots2)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(YieldCovPots2Wb, sheet_name)
  writeData(YieldCovPots2Wb, sheet=sheet_name, x=YieldCov_Pots2[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(YieldCovPots2Wb, "Pots2_Yield_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
YieldCovPots2_df <- lapply(seq_along(YieldCov_Pots2), function(i) {
  cov_mat1h <- as.matrix(YieldCov_Pots2[[i]])
  cov_mat1h <- setNames(cov_mat1h, YieldCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(YieldCov_Pots2)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
YieldCovPots2_dfAll <- do.call(rbind, YieldCovPots2_df)
YieldCovPots2_dfAll$Var1 <- factor(YieldCovPots2_dfAll$Var1, levels=Pots2CovVar, labels=c("Biomass"="Yield", 
                                   "SNO3"="NO3", "SNH4"="NH4", "SPO4"="PO4", "ResinP"="Resin P", 
                                   "WatSolP"="Soluble P", "TotalP"="Total P", "pH"="pH", "EC"="EC", 
                                   "OC"="% SOC"))
YieldCovPots2_dfAll$variable <- factor(YieldCovPots2_dfAll$variable, levels=Pots2CovVar, labels=c("Biomass"="Yield", 
                                   "SNO3"="NO3", "SNH4"="NH4", "SPO4"="PO4", "ResinP"="Resin P", "WatSolP"="Soluble P",
                                   "TotalP"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
YieldCovPots2_dfAll$treatment <- factor(YieldCovPots2_dfAll$treatment, 
                     levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", 
                              "Phosphorus"),
                     labels=c("Control 1", "Control 2", "Canola Meal", "Manure", "Willow", "Meat & BoneMeal - Coarse",
                              "Meat & Bonemeal - Fine", "TSP Fertilizer"))
YieldCovPots2_RmTrt <- c("Control 1", "Control 2")
YieldCovPots2_dfAll <- YieldCovPots2_dfAll[!YieldCovPots2_dfAll$treatment %in% YieldCovPots2_RmTrt, ]
write.csv(YieldCovPots2_dfAll, file="Pots2_YieldCov.csv")
# ggplot best option - brackets on both sides of the variable and plot code assigns and calls all in one
(YieldCovPots2Heat <- ggplot(YieldCovPots2_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "YlGnBu"), limits=c(-1.9, 5), breaks=seq(-1.9, 5, by=1)) +
    facet_wrap(~ treatment, nrow=3, scales="fixed") +
    geom_text(aes(label=sprintf("%.2f", Covariance), color = ifelse(Covariance > 2, "white", "black")), size=6.5) +
    scale_color_manual(values=c("black", "white"), guide=FALSE, labels=NULL)+
    theme(legend.title=element_text(size=20, face="bold"),
          legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=26, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=18, angle=45, hjust=1, colour = "black", face = "bold"),
          axis.text.y.left=element_text(size=18, angle=45, colour = "black", face = "bold"),
          panel.spacing.x=unit(1, "cm"))+
    labs(x="", y=""))
ggsave(YieldCovPots2Heat, file="Pots2_YieldCovHeat.jpg", width=20, height=20, dpi=150)



#####   Uptake  #####
UptakeCovVar <- c("Puptake", "SNO3", "SNH4", "SPO4", "ResinP", "WatSolP", "TotalP", "pH", "EC", "OC")
Pots2CovUptake <- subset(Pots2, select=c("Treatment", UptakeCovVar), 
                        na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
Pots2CovScaleUptake <- as.data.frame(scale(Pots2CovUptake[,-1]))
Pots2CovScaleUptake$Treatment <- Pots2CovUptake$Treatment
Pots2CovScaleUptakeSplit <- split(Pots2CovScaleUptake[, -ncol(Pots2CovScaleUptake)], Pots2CovScaleUptake$Treatment)
## calculate the covariance matrix for each treatment excluding missing data
UptakeCov_Pots2 <- lapply(Pots2CovScaleUptakeSplit, function(x) cov(x, use="pairwise.complete.obs"))
UptakeCovPots2Wb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(UptakeCov_Pots2)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(UptakeCov_Pots2)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(UptakeCovPots2Wb, sheet_name)
  writeData(UptakeCovPots2Wb, sheet=sheet_name, x=UptakeCov_Pots2[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(UptakeCovPots2Wb, "Pots2_Uptake_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
UptakeCovPots2_df <- lapply(seq_along(UptakeCov_Pots2), function(i) {
  cov_mat1h <- as.matrix(UptakeCov_Pots2[[i]])
  cov_mat1h <- setNames(cov_mat1h, UptakeCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(UptakeCov_Pots2)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
UptakeCovPots2_dfAll <- do.call(rbind, UptakeCovPots2_df)
UptakeCovPots2_dfAll$Var1 <- factor(UptakeCovPots2_dfAll$Var1, levels=UptakeCovVar, labels=c("Biomass"="Yield", 
                                    "SNO3"="NO3", "SNH4"="NH4", "SPO4"="PO4", "ResinP"="Resin P", 
                                    "WatSolP"="Soluble P", "TotalP"="Total P", "pH"="pH", "EC"="EC", 
                                    "OC"="% SOC"))
UptakeCovPots2_dfAll$variable <- factor(UptakeCovPots2_dfAll$variable, levels=UptakeCovVar, 
                                        labels= c("Biomass"="Yield", "SNO3"="NO3", "SNH4"="NH4", "SPO4"="PO4", 
                                                  "ResinP"="Resin P", "WatSolP"="Soluble P", 
                                                  "TotalP"="Total P", "pH"="pH", "EC"="EC", "OC"="% SOC"))
UptakeCovPots2_dfAll$treatment <- factor(UptakeCovPots2_dfAll$treatment, 
                         levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow", "MBMACoarse", 
                                  "MBMAFine", "Phosphorus"),
                         labels=c("Control 1", "Control 2", "Canola Meal", "Manure", "Willow", 
                                  "Meat & BoneMeal - Coarse", "Meat & Bonemeal - Fine", "TSP Fertilizer"))
YieldCovPots2_RmTrt <- c("Control 1", "Control 2")
UptakeCovPots2_dfAll <- UptakeCovPots2_dfAll[!UptakeCovPots2_dfAll$treatment %in% YieldCovPots2_RmTrt, ]
write.csv(UptakeCovPots2_dfAll, file="Pots2_UptakeCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(UptakeCovPots2Heat <- ggplot(UptakeCovPots2_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "PuBuGn"), limits=c(-2, 5), breaks=seq(-2, 5, by=1)) +
    facet_wrap(~ treatment, nrow=3, scales="fixed") +
    geom_text(aes(label=sprintf("%.2f", Covariance), color = ifelse(Covariance > 2, "white", "black")), size=6.5) +
    scale_color_manual(values=c("black", "white"), guide=FALSE, labels=NULL)+
    theme(legend.title=element_text(size=20, face="bold"),
          legend.key.size=unit(15,"mm"),
          legend.text=element_text(size=20), 
          strip.text=element_text(size=26, face="bold"),
          strip.placement="outside",
          strip.background=element_blank(),
          strip.text.y=element_text(angle=0, vjust=0.5),
          strip.text.x=element_text(vjust=1),
          axis.line=element_blank(),
          axis.text.x.bottom=element_text(size=18, angle=45, hjust=1, colour = "black", face = "bold"),
          axis.text.y.left=element_text(size=18, angle=45, colour = "black", face = "bold"),
          panel.spacing.x=unit(1, "cm"))+
    labs(x="", y=""))
ggsave(UptakeCovPots2Heat, file="Pots2_UptakeCovHeat.jpg", width=20, height=20, dpi=150)


####   Yield to N & P Uptake  ####
# use uptake for Pots 2 as recovery can't be calculated due to soil coming from the field & added urea
Pots2$Treatment <- as.factor(Pots2$Treatment)
Pots2$Biomass <- as.numeric(Pots2$Biomass)
Pots2$Nrecovery <- as.numeric(Pots2$Nrecovery)
Pots2$Precovery <- as.numeric(Pots2$Precovery)
Pots2ContourSub <- subset(Pots2, select = c(Block, Treatment, Biomass, Nuptake, Puptake))
View(Pots2ContourSub)
Pots2ContourSub$Treatment <- factor(Pots2ContourSub$Treatment, levels=c("Control1", "Control2", "CanolaMeal", 
                                    "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
                   labels=c("Control 1", "Control 2","Canola Meal", "Manure", "Willow", 
                            "Meat & BoneMeal - Coarse", "Meat & Bonemeal - Fine", "TSP Fertilizer"))
View(Pots2ContourSub)
Pots2ContourExcl <- na.exclude(Pots2ContourSub)
View(Pots2ContourExcl)

Pots2ContourMod <- glmmTMB(Biomass ~ Nuptake + Puptake + Treatment + (1|Block), data = Pots2ContourExcl, 
                          na.action=na.exclude)
summary(Pots2ContourMod)
Anova(Pots2ContourMod)
#Set up N & P uptake grids per soil
Pots2Nuptake_grid <- seq(min(Pots2ContourExcl$Nuptake, na.rm = TRUE), max(Pots2ContourExcl$Nuptake, na.rm = TRUE),
                          length.out = 100)
Pots2Puptake_grid <- seq(min(Pots2ContourExcl$Puptake, na.rm = TRUE), max(Pots2ContourExcl$Puptake, na.rm = TRUE),
                          length.out = 100)
# Set up expanded grids then assign yield - must include block as it was used in the model!!
Pots2Contour_grid <- expand.grid(Block=unique(Pots2ContourExcl$Block), Treatment = unique(Pots2ContourExcl$Treatment), 
                                Nuptake = Pots2Nuptake_grid, Puptake = Pots2Puptake_grid)
Pots2Contour_grid$Yield <- predict(Pots2ContourMod, newdata = Pots2Contour_grid)
Pots2Contour_grid <- Pots2Contour_grid[,-1] # remove block so it doesn't appear in the plot
View(Pots2Contour_grid)
# develop contour plot
(Pots2Contours <- ggplot(Pots2Contour_grid, aes(x = Nuptake, y = Puptake, z = Yield)) +
    geom_raster(aes(fill=Yield)) + #use rastar to get smooth lines
    geom_contour(aes(z=Yield), color='gray30', binwidth = 0.5) + #contour line, adjust binwidth depending on yield
    facet_wrap(~Treatment, nrow = 5) +
    scale_fill_gradientn(colors = brewer.pal(9, "BuPu")) +
    labs(x = "% N Uptake", y = "% P Uptake", fill = "Yield (g)") +
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
ggsave(Pots2Contours, file="Pots2_YieldContour.jpg", width=20, height=20, dpi=150)



#### Correlation & eigenvalues  ####
Pots2EigenMatrix <- Pots2[complete.cases(Pots2), c("Nuptake", "Puptake", "SNO3", "SNH4", "SPO4", "ResinP", 
                                                   "WatSolP","TotalP", "pH", "EC", "OC"),]
Pots2EigenCor <- cor(Pots2EigenMatrix)
Pots2EigenPCA <- PCA(Pots2EigenMatrix, scale.unit = TRUE, ncp = length(Pots2EigenMatrix)-1)
Pots2EigenPrin <-princomp(Pots2EigenCor)
summary(Pots2EigenPrin, digits=3)
round(Pots2EigenPrin$loadings[, 1:2], 3)


####  Correlate char P to residual soil P  ####
# set up new data frame 
Pots2CharPdf <- data.frame(Treatment=Pots2$Treatment,
                          SPO4=Pots2$SPO4,
                          CharPerc=Pots2$CharPerc)
Pots2CharExcl <- c("Control1", "Control2")
Pots2CharPdf <- subset(Pots2CharPdf, !Treatment %in% Pots2CharExcl)
print(Pots2CharPdf)
Pots2CharCor <- Pots2CharPdf %>%
  group_by(Treatment) %>%
  summarize(Correlation = cor(CharPerc, SPO4, use = "complete.obs"), .groups = "drop")
Pots2CharCor$Treatment <- factor(Pots2CharCor$Treatment, levels=c("CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
                                 labels=c("Canola\nMeal", "Manure", "Willow", "Meat and\nBoneMeal -\nCoarse", "Meat and\nBonemeal-\nFine", 
                                          "TSP\nFertilizer"))
print(Pots2CharCor)
# visualize correlation using heatmap
(Pots2CharHeatPlot <- ggplot(Pots2CharCor, aes(x=Treatment, y=0.5, fill=Correlation)) +
  geom_point(data=Pots2CharCor, aes(size=abs(Correlation)*20), shape=21) + #set size of correlation circles
  scale_size(range = c(30,45)) +
  scale_fill_gradientn(colors=brewer.pal(9, "PiYG"), limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
  geom_text(aes(label=sprintf("%.3f",Correlation)), size=7)+
  theme(plot.title=element_text(hjust=0.5, face="bold", size=22),
        legend.text=element_text(size=12),
        legend.title=element_text(size=16, face="bold"),
        legend.key.siz=unit(15,"mm"),
        axis.title.y=element_text(size=14, colour="black", face="bold"),
        axis.title.x=element_text(size=14, colour="black", face="bold"),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(), #no labels
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background=element_blank(),  # remove gray background
        panel.spacing.x=unit(1, "cm"),
        plot.margin=margin(5, 5, 5, 5))+
        guides(size = "none")+
  labs(x="", y="", title=expression(bold("Percentage P in char - Soil residual PO"[4]))))


## correlate P uptake to to %P in char
Pots2RecPdf <- data.frame(Treatment=Pots2$Treatment,
                         Puptake=Pots2$Puptake,
                         CharPerc=Pots2$CharPerc)
Pots2CharExcl <- c("Control1", "Control2")
Pots2RecPdf <- subset(Pots2RecPdf, !Treatment %in% Pots2CharExcl)
print(Pots2RecPdf)
Pots2RecCor <- Pots2RecPdf %>%
  group_by(Treatment) %>%
  summarize(Correlation = cor(Puptake, CharPerc, use = "complete.obs"), .groups = "drop")
Pots2RecCor$Treatment <- factor(Pots2RecCor$Treatment, levels=c("CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
                               labels=c("Canola\nMeal", "Manure", "Willow", "Meat and\nBoneMeal -\nCoarse", "Meat and\nBonemeal-\nFine", 
                                        "TSP\nFertilizer"))
print(Pots2RecCor)
# visualize correlation using heatmap
(Pots2RecHeatPlot <- ggplot(Pots2RecCor, aes(x=Treatment, y=0.5, fill=Correlation)) +
    geom_point(data=Pots2RecCor, aes(size=abs(Correlation)*20), shape=21) + #set size of correlation circles
    scale_size(range = c(30,45)) +
    scale_fill_gradientn(colors=brewer.pal(9, "PiYG"), limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.3f",Correlation)), size=7)+
    theme(plot.title=element_text(hjust=0.5, face="bold", size=22),
          legend.text=element_text(size=12),
          legend.title=element_text(size=16, face="bold"),
          legend.key.siz=unit(15,"mm"),
          axis.title.y=element_text(size=14, colour="black", face="bold"),
          axis.title.x=element_text(size=14, colour="black", face="bold"),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(), #no labels
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background=element_blank(),  # remove gray background
          panel.spacing.x=unit(1, "cm"),
          plot.margin=margin(5, 5, 5, 5))+
    guides(size = "none")+
    labs(x="", y="", title="P Uptake - Percentage P in char"))

## correlate P uptake to residual PO4
Pots2PO4Pdf <- data.frame(Treatment=Pots2$Treatment,
                         SPO4=Pots2$SPO4,
                         Puptake=Pots2$Puptake)
Pots2CharExcl <- c("Control1", "Control2")
Pots2PO4Pdf <- subset(Pots2PO4Pdf, !Treatment %in% Pots2CharExcl)
print(Pots2PO4Pdf)
Pots2PO4Cor <- Pots2PO4Pdf %>%
  group_by(Treatment) %>%
  summarize(Correlation = cor(Puptake, SPO4, use = "complete.obs"), .groups = "drop")
Pots2PO4Cor$Treatment <- factor(Pots2PO4Cor$Treatment, levels=c("CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
                               labels=c("Canola\nMeal", "Manure", "Willow", "Meat and\nBoneMeal -\nCoarse", "Meat and\nBonemeal-\nFine", 
                                        "TSP\nFertilizer"))
print(Pots2PO4Cor)
# visualize correlation using heatmap
(Pots2PO4HeatPlot <- ggplot(Pots2PO4Cor, aes(x=Treatment, y=0.5, fill=Correlation)) +
    geom_point(data=Pots2PO4Cor, aes(size=abs(Correlation)*20), shape=21) + #set size of correlation circles
    scale_size(range = c(30,45)) +
    scale_fill_gradientn(colors=brewer.pal(9, "PiYG"), limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.3f", Correlation)), size=7)+
    scale_x_discrete(position="bottom")+ #used for labels on the bottom
    theme(plot.title=element_text(hjust=0.5, face="bold", size=22),
          legend.text=element_text(size=12),
          legend.title=element_text(size=16, face="bold"),
          legend.key.siz=unit(15,"mm"),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(angle=45, size=19, colour="black", hjust=1), #keep only in the last one
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background=element_blank(),  # remove gray background
          panel.spacing.x=unit(1, "cm"),
          plot.margin=margin(5, 5, 5, 5))+
    guides(size = "none")+
    labs(x="", y="", title=expression(bold("P Uptake - Soil residual PO"[4]))))

## combined plot - combined legend and ggarrange uses ggpubr package, set legend in individual plots
(Pots2CharRecPO4_plot <-ggarrange(Pots2CharHeatPlot, Pots2RecHeatPlot, Pots2PO4HeatPlot, nrow=3, common.legend=TRUE, legend="right", 
                                 heights=c(0.8, 0.8, 1.1)))
ggsave(Pots2CharRecPO4_plot, file="Pots2_CharPO4Prec_combined.jpg", width=15, height=9, dpi=150)



####  Extract ANOVA tables  ####
ModP2Bio3<- glmmTMB(Biomass~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2YieldAN <- glmmTMB:::Anova.glmmTMB(ModP2Bio3, type="III")
P2YieldAN$RowNames <- row.names(P2YieldAN)
rownames(P2YieldAN) <- NULL

ModP2Nup3<- glmmTMB(Nuptake~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2NupAN <- glmmTMB:::Anova.glmmTMB(ModP2Nup3, type="III")
P2NupAN$RowNames <- row.names(P2NupAN)
rownames(P2NupAN) <- NULL

ModP2Pup4 <- lmer(Puptake~Treatment+(1|Block), data=Pots2, na.action=na.exclude)
P2PupAN <- anova(ModP2Pup4) 
P2PupAN$RowNames <- row.names(P2PupAN)
rownames(P2PupAN) <- NULL

ModP2SNO33<- glmmTMB(SNO3~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2SNO3AN <- glmmTMB:::Anova.glmmTMB(ModP2SNO33, type="III")
P2SNO3AN$RowNames <- row.names(P2SNO3AN)
rownames(P2SNO3AN) <- NULL

ModP2SNH43<- glmmTMB(SNH4~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2SNH4AN <- glmmTMB:::Anova.glmmTMB(ModP2SNH43, type="III")
P2SNH4AN$RowNames <- row.names(P2SNH4AN)
rownames(P2SNH4AN) <- NULL

ModP2SPO43<- glmmTMB(SPO4~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2SPO4AN <- glmmTMB:::Anova.glmmTMB(ModP2SPO43, type="III")
P2SPO4AN$RowNames <- row.names(P2SPO4AN)
rownames(P2SPO4AN) <- NULL

ModP2ResP2<- glmmTMB(log(ResinP)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2ResPAN <- glmmTMB:::Anova.glmmTMB(ModP2ResP2, type="III")
P2ResPAN$RowNames <- row.names(P2ResPAN)
rownames(P2ResPAN) <- NULL

ModP2WSP2<- glmmTMB(log(WatSolP)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2WSPAN <- glmmTMB:::Anova.glmmTMB(ModP2WSP2, type="III")
P2WSPAN$RowNames <- row.names(P2WSPAN)
rownames(P2WSPAN) <- NULL

ModP2TotalP2<- glmmTMB(log(TotalP)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2totPAN <- glmmTMB:::Anova.glmmTMB(ModP2TotalP2, type="III")
P2totPAN$RowNames <- row.names(P2totPAN)
rownames(P2totPAN) <- NULL

ModP2pH3<- glmmTMB(pH~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2pHAN <- glmmTMB:::Anova.glmmTMB(ModP2pH3, type="III")
P2pHAN$RowNames <- row.names(P2pHAN)
rownames(P2pHAN) <- NULL

ModP2EC5 <- lme(EC ~ Treatment, random=~1|Block, data=Pots2, na.action=na.exclude)
P2ecAN <- anova(ModP2EC5)
P2ecAN$RowNames <- row.names(P2ecAN)
rownames(P2ecAN) <- NULL

ModP2OC5 <- glmer(OC~Treatment+(1|Block),data=Pots2,family=Gamma(link="log"), na.action=na.exclude)
P2ocAN <- anova(ModP2OC5)
P2ocAN$RowNames <- row.names(P2ocAN)
rownames(P2ocAN) <- NULL

ModP2LNO32<- glmmTMB(sqrt(LNO3)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2LNO3AN <- glmmTMB:::Anova.glmmTMB(ModP2LNO32, type="III")
P2LNO3AN$RowNames <- row.names(P2LNO3AN)
rownames(P2LNO3AN) <- NULL

ModP2LNH42<- glmmTMB(log(LNH4)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2LNH4AN <- glmmTMB:::Anova.glmmTMB(ModP2LNH42, type="III")
P2LNH4AN$RowNames <- row.names(P2LNH4AN)
rownames(P2LNH4AN) <- NULL

ModP2LPO42<- glmmTMB(log(LPO4)~Treatment+(1|Block), data=Pots2, family=gaussian(), na.action=na.exclude)
P2LPO4AN <- glmmTMB:::Anova.glmmTMB(ModP2LPO42, type="III")
P2LPO4AN$RowNames <- row.names(P2LPO4AN)
rownames(P2LPO4AN) <- NULL

Pots2ANOVAtables <- list(P2YieldAN, P2NupAN, P2PupAN, P2SNO3AN, P2SNH4AN, P2SPO4AN, P2ResPAN, P2WSPAN, P2totPAN, 
                        P2pHAN, P2ecAN, P2ocAN, P2LNO3AN, P2LNH4AN, P2LPO4AN)
names(Pots2ANOVAtables) <- c("Yield", "Nuptake", "Puptake", "SoilNO3", "SoilNH4", "SoilPO4", 
                            "ResinP", "WaterSolP", "TotalP", "pH", "EC", "OC", "LeachNO3", "LeachNH4", "LeachPO4")
write_xlsx(Pots2ANOVAtables, path = "Pots2ANOVAtables.xlsx")

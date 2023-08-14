##### Loading data in to R ####
Field<-read.csv("Field.csv", fileEncoding="UTF-8-BOM")
View(Field)
FieldSplit<-read.csv("FieldSplitRaw.csv", fileEncoding="UTF-8-BOM")
Fieldraw <- read.csv("Fieldraw.csv", fileEncoding="UTF-8-BOM")

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
library(dunn.test)

##### Combining 0-30cm values   ####
# Calculate mean of soil data from incremental depths to combined depth (0-30cm)
Trt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus")
FieldSplit$Block <- factor(FieldSplit$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
FieldSplit$Treatment <- factor(FieldSplit$Treatment,levels = Trt_order)
FieldSplit$Plot <- factor(FieldSplit$Plot)
FieldSplit$NO3 <- as.numeric(as.character(FieldSplit$NO3))
FieldSplit$PO4 <- as.numeric(as.character(FieldSplit$PO4))
FieldSplit$WatSolP <- as.numeric(as.character(FieldSplit$WatSolP))
FieldSplit$ResinP <- as.numeric(as.character(FieldSplit$ResinP))
FieldSplit$pH <- as.numeric(as.character(FieldSplit$pH))
FieldSplit$EC <- as.numeric(as.character(FieldSplit$EC))
FieldSplit$OC <- as.numeric(as.character(FieldSplit$OC))
summary(Field)
str(Field) #displays the structure of the object
#  Settiing up a new data frame containing combined values
FieldResGroup <- FieldSplit %>%
  group_by(Plot, Treatment, Block)
FieldResid <- FieldResGroup %>%
  summarise(c(NO3c	= mean(NO3)), (PO4c=mean(PO4)), (WatSolPc=mean(WatSolP)), (ResinPc  = mean(ResinP)),
              (pHc = mean(pH)), (ECc = mean(EC)), (OCc = mean(OC)))
View(FieldResid)
write.csv(FieldResid, file="FieldResidMean.csv")


##### Summary and ordering of data   ####
#Check for missing values in a specific field
missing <- colSums(is.na(Field[,]))
missing <- colSums(is.na(Fieldraw[,]))
print(missing)
#Change columns in a dataframe to factors/categorical values, str displays 
Trt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus")
Field$Block <- factor(Field$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Field$Treatment <- factor(Field$Treatment,levels = Trt_order)
Field$LNO3 <- as.numeric(as.character(FieldSplit$LNO3))
Field$LNH4 <- as.numeric(as.character(FieldSplit$LNH3))
Field$LPO4 <- as.numeric(as.character(FieldSplit$LPO4))
summary(Field)
str(Field) #displays the structure of the object
# Summary data (means, SD, etc.) for each treatment and variable
FieldMean <- summary_by(.~Treatment, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) #overall means for the data set
View(FieldMean)
FieldSD <- summary_by(.~Treatment, data=Field,  FUN=function(x) sd(x, na.rm=TRUE)) #overall SD for the dataset
View(FieldSD)
StrawF_summary <- Field %>% #summary data specifically for Straw variable
  group_by(Treatment) %>%
  summarize(Mean=mean(FStraw), SD=sd(FStraw), SE=sd(FStraw)/sqrt(n()))
View(StrawF_summary) #summary data specifically for Grain variable
GrainF_summary <- Field %>%
  group_by(Treatment) %>%
  summarize(Mean=mean(FGrain), SD=sd(FGrain), SE=sd(FGrain)/sqrt(n()))
View(GrainF_summary)



#####   Check for outliers   ####
##Straw
ggplot(Fieldraw, aes(x = Treatment, y = FStraw, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Straw")
ggsave("OutliersField_CanStraw24.jpg", width = 10, height = 10, dpi = 200)
##Grain
ggplot(Fieldraw, aes(x = Treatment, y = FGrain, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Grain")
ggsave("OutliersField_CanGrain24.jpg", width = 10, height = 10, dpi = 200)
## Volumetric Moisture Content - Wet sample
ggplot(Fieldraw, aes(x = Treatment, y = MoistWet, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "MoistWet")
ggsave("OutliersField_VMCwet.jpg", width = 10, height = 10, dpi = 200)
## Volumetric Moisture Content - drysample
ggplot(Fieldraw, aes(x = Treatment, y = MoistDry, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "MoistDry")
ggsave("OutliersField_VMCdry.jpg", width = 10, height = 10, dpi = 200)
## Bulk Density - wet
ggplot(Fieldraw, aes(x = Treatment, y = BDwet, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "BDwet")
ggsave("OutliersField_BDwet.jpg", width = 10, height = 10, dpi = 200)
## Bulk Density - dry
ggplot(Fieldraw, aes(x = Treatment, y = BDdry, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "BDdry")
ggsave("OutliersField_BDdry.jpg", width = 10, height = 10, dpi = 200)
## Nuptake
ggplot(Fieldraw, aes(x = Treatment, y = Nuptake, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "N uptake")
ggsave("OutliersField_Nuptake.jpg", width = 10, height = 10, dpi = 200)
## N recovery
ggplot(Fieldraw, aes(x = Treatment, y = Nrecovery, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "N recovery")
ggsave("OutliersField_Nrecovery.jpg", width = 10, height = 10, dpi = 200)
## P uptake
ggplot(Fieldraw, aes(x = Treatment, y = Puptake, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "P uptake")
ggsave("OutliersField_Puptake.jpg", width = 10, height = 10, dpi = 200)
## P recovery
ggplot(Fieldraw, aes(x = Treatment, y = Precovery, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "P recovery")
ggsave("OutliersField_Precovery.jpg", width = 10, height = 10, dpi = 200)
## NO3
ggplot(Fieldraw, aes(x = Treatment, y = NO3, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "NO3")
ggsave("OutliersField_NO3.jpg", width = 10, height = 10, dpi = 200)
## PO4
ggplot(Fieldraw, aes(x = Treatment, y = PO4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "PO4")
ggsave("OutliersField_PO4.jpg", width = 10, height = 10, dpi = 200)
## WatSolP
ggplot(Fieldraw, aes(x = Treatment, y = WatSolP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Water Soluble P")
ggsave("OutliersField_WatSolP.jpg", width = 10, height = 10, dpi = 200)
## ResinP
ggplot(Fieldraw, aes(x = Treatment, y = ResinP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "ResinP")
ggsave("OutliersField_ResinP.jpg", width = 10, height = 10, dpi = 200)
## pH
ggplot(Fieldraw, aes(x = Treatment, y = pH, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "pH")
ggsave("OutliersField_pH.jpg", width = 10, height = 10, dpi = 200)
## EC
ggplot(Fieldraw, aes(x = Treatment, y = EC, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "EC")
ggsave("OutliersField_EC.jpg", width = 10, height = 10, dpi = 200)
## %OC
ggplot(Fieldraw, aes(x = Treatment, y = OC, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "OC")
ggsave("OutliersField_OC.jpg", width = 10, height = 10, dpi = 200)
## NO3Load	
ggplot(Fieldraw, aes(x = Treatment, y = NO3Load, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "NO3Load")
ggsave("OutliersField_NO3Load.jpg", width = 10, height = 10, dpi = 200)
## NH4Load	
ggplot(Fieldraw, aes(x = Treatment, y = NH4Load	, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "NH4Load	")
ggsave("OutliersField_NH4Load.jpg", width = 10, height = 10, dpi = 200)
## PO4Load	
ggplot(Fieldraw, aes(x = Treatment, y = PO4Load, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "PO4Load")
ggsave("OutliersField_PO4Load.jpg", width = 10, height = 10, dpi = 200) 
## ResinPO4	
ggplot(Fieldraw, aes(x = Treatment, y = ResinPO4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "ResinPO4")
ggsave("OutliersField_ResinPO4.jpg", width = 10, height = 10, dpi = 200)
## ResinNO3
ggplot(Fieldraw, aes(x = Treatment, y = ResinNO3, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "ResinNO3")
ggsave("OutliersField_ResinNO3.jpg", width = 10, height = 10, dpi = 200)











#####   STRAW   ########
FieldStraw_Mean <- summary_by(FStraw~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE))
FieldStraw_Mean <- as.numeric(FieldStraw_Mean$FStraw)
FieldStraw_skew <- skewness(FieldStraw_Mean)
FieldStraw_kur <- kurtosis(FieldStraw_Mean)
cat("Skewness:", FieldStraw_skew, "\n") # 0.6616705 
cat("Kurtosis:", FieldStraw_kur, "\n") # -0.7709748 
ggplot(Field, aes(x=Treatment, y=FStraw, fill=Treatment)) + geom_boxplot() + labs(x = "Treatment", y = "Straw")
hist(Field$FStraw) #  left skew
shapiro.test(Field$FStraw) # p=0.04875
leveneTest(FStraw~Treatment, data=Field)  # P=0.1466
# transform
hist(log(Field$FStraw)) #  left skew
shapiro.test(log(Field$FStraw)) # p=0.3666
leveneTest(log(FStraw)~Treatment, data=Field)  # P=0.1384
#ModFStraw1 aov
ModFStraw1<- aov(log(FStraw)~Treatment+Block, data=Field)
anova(ModFStraw1) 
summary(ModFStraw1)
boxplot(residuals(ModFStraw1)~Treatment, data=Field, main="Residuals by Treatment")
hist(resid(ModFStraw1))
shapiro.test(resid(ModFStraw1)) # p=0.7166
plot(fitted(ModFStraw1),resid(ModFStraw1),pch=16)  
qqnorm(resid(ModFStraw1)) # slight to medium left tail
qqline(resid(ModFStraw1))
ModFStraw1_tidy <- tidy(ModFStraw1)
ModFStraw1sum_sq_reg <- ModFStraw1_tidy$sumsq[1] 
ModFStraw1sum_sq_resid <- ModFStraw1_tidy$sumsq[2]
ModFStraw1sum_sq_reg / (ModFStraw1sum_sq_reg + ModFStraw1sum_sq_resid) #  0.1722999
#Lm Model for straw - same means and CLD, but dif upper and lower limits than for lmer
ModFStraw2 <- lm(log(FStraw)~Treatment+Block,data=Field)
anova(ModFStraw2)
summary(ModFStraw2)
hist(resid(ModFStraw2))  # data is normally distributed
shapiro.test(resid(ModFStraw2))  # p=0.7166
boxplot(residuals(ModFStraw2)~Treatment, data=Field, main="Residuals by Treatment")
plot(fitted(ModFStraw2),resid(ModFStraw2),pch=16)  
qqnorm(resid(ModFStraw2)) # slight tails
qqline(resid(ModFStraw2))
rsq(ModFStraw2) # 0.6258265
# lmer - model preferred due to blocking effect
ModFStraw3 <- lmer(log(FStraw)~Treatment+(1|Block),data=Field)
anova(ModFStraw3)
summary(ModFStraw3)
hist(resid(ModFStraw3)) 
shapiro.test(resid(ModFStraw3))  # 0.7146
plot(fitted(ModFStraw3),resid(ModFStraw3),pch=16)   
qqnorm(resid(ModFStraw3)) # medium left tail
qqline(resid(ModFStraw3))
rsq(ModFStraw3) # 0.476
# weighted lm model
ModFStrawvar <- tapply(log(Field$FStraw), Field$Treatment, var, na.rm=TRUE)
weightsFstraw <- 1 / ModFStrawvar
weightsFstraw_full <- rep(weightsFstraw, each = length(Field$FStraw) / length(weightsFstraw))
ModFStraw4 <- lm(FStraw ~ Treatment + Block, data=Field, weights=weightsFstraw_full) 
anova(ModFStraw3)
summary(ModFStraw3)
hist(resid(ModFStraw4)) # left skewed
shapiro.test(resid(ModFStraw4))  # 0.3059
plot(fitted(ModFStraw4),resid(ModFStraw4),pch=16)   # clusters forming
qqnorm(resid(ModFStraw4)) # slight tails
qqline(resid(ModFStraw4))
rsq(ModFStraw4) # 0.536
#ModFStraw4 - glmer model
ModFStraw5 <- glmer(FStraw~Treatment+(1|Block),data=Field,family=Gamma(link="log"))
anova(ModFStraw5)
summary(ModFStraw5)
shapiro.test(resid(ModFStraw5)) # p=0.5819
bf.test(Straw~Treatment, data=Field) # 0.7851604 
rsq(ModFStraw5) # 0.5733773
#Kruskal-Wallis on Straw
ModFStraw6 <- kruskal.test(FStraw~Treatment,data = Field) # non-parametric comparing equalness of the medians
print(ModFStraw6)  # chi squared p=0.8107
rsq(ModFStraw6)
#Comparing models - highest rsq and lowest AIC?BIC = ModFStraw2
#rsq values: ModFStraw1=0.173, ModFStraw2=0.625, ModFStraw3=0.463, ModFStraw4 =0.536, ModFStraw5=0.573
Fstraw_modlist <- list(ModFStraw1, ModFStraw2, ModFStraw3, ModFStraw4, ModFStraw5)
AIC_values <- sapply(Fstraw_modlist, AIC)
BIC_values <- sapply(Fstraw_modlist, BIC)
N_AB <- data.frame(Model=c("ModFStraw1", "ModFStraw2", "ModFStraw3", "ModFStraw4", "ModFStraw5"), AIC_values, BIC_values)
print(N_AB)
  #MModel AIC_values BIC_values
  #1 ModFStraw1  -3.099557   8.680981
  #2 ModFStraw2  -3.099557   8.680981
  #3 ModFStraw3  21.257225  30.681656
  #4 ModFStraw4 370.977642 382.758180
  #5 ModFStraw5 373.872434 383.296865
# emmeans 
ModFStrawem <- emmeans(ModFStraw2,~Treatment, alpha=0.1, type="response")
ModFStrawem_cld <- cld(ModFStrawem, Letters = letters, type="response") 
ModFStrawem_cld <- ModFStrawem_cld %>% rename(emmean = "response")
View(ModFStrawem_cld)
write.csv(ModFStrawem_cld, file="Field_Straw.csv")
# Developing visualizations
par(mar=c(5,6,4,2)+0.2) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFStrawem_cld, aes(x = Treatment, y = response)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=response+SE), size=4, vjust=-1) +
  labs(x = "Treatments", y = "Canola straw yield (kg/ha)") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kg P/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  scale_y_continuous(limits = c(-10, 70))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("LargePlots_straw.jpg", width = 12, height = 8, dpi = 600)




#####   GRAIN   ########
FieldGrain_Mean <- summary_by(FGrain~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE))
FieldGrain_Mean <- as.numeric(FieldGrain_Mean$FGrain)
FieldGrain_skew <- skewness(FieldGrain_Mean)
FieldGrain_kur <- kurtosis(FieldGrain_Mean)
cat("Skewness:", FieldGrain_skew, "\n") # 1.33558
cat("Kurtosis:", FieldGrain_kur, "\n") # 0.8658551 
hist(FieldGrain_Mean) # severe left skew
shapiro.test(FieldGrain_Mean) # p=0.0007133
leveneTest(FGrain~Treatment, data=Field) # 0.1344
# transform
hist(log(FieldGrain_Mean)) # severe left skew
shapiro.test(log(FieldGrain_Mean)) # p=0.1062
leveneTest(log(FGrain)~Treatment, data=Field) # 0.119
hist(sqrt(FieldGrain_Mean)) # severe left skew
shapiro.test(sqrt(FieldGrain_Mean)) # p=0.009866
leveneTest(sqrt(FGrain)~Treatment, data=Field) # 0.1182
#ModFGrain1 aov
ModFGrain1<- aov(log(FGrain)~Treatment+Block, data=Field) #Two-way anova for Grain
anova(ModFGrain1)
summary(ModFGrain1)
hist(resid(ModFGrain1))
shapiro.test(resid(ModFGrain1)) # p=0.6778
plot(fitted(ModFGrain1),resid(ModFGrain1),pch=16)  # V or W shape
qqnorm(resid(ModFGrain1)) # slight tails
qqline(resid(ModFGrain1))
ModFGrain1_tidy <- tidy(ModFGrain1)
ModFGrain1sum_sq_reg <- ModFGrain1_tidy$sumsq[1] 
ModFGrain1sum_sq_resid <- ModFGrain1_tidy$sumsq[2]
ModFGrain1sum_sq_reg / (ModFGrain1sum_sq_reg + ModFGrain1sum_sq_resid) #  0.149
#Lm Model for Grain - same means and CLD, but dif upper and lower limits than for lmer
ModFGrain2 <- lm(log(FGrain)~Treatment+Block,data=Field)
anova(ModFGrain2)
summary(ModFGrain2)
hist(resid(ModFGrain2)) 
shapiro.test(resid(ModFGrain2))  # p=0.6778
plot(fitted(ModFGrain2),resid(ModFGrain2),pch=16)  # V shape
qqnorm(resid(ModFGrain2)) # slight tails
qqline(resid(ModFGrain2))
rsq(ModFGrain2) # 0.6837072
# weighted lm model
ModFGrainvar <- tapply(log(Field$FGrain), Field$Treatment, var, na.rm=TRUE)
weightsFGrain <- 1 / ModFGrainvar
weightsFGrain_full <- rep(weightsFGrain, each = length(Field$FGrain) / length(weightsFGrain))
ModFGrain3 <- lm(FGrain ~ Treatment + Block, data=Field, weights=weightsFGrain_full) 
anova(ModFGrain3)
summary(ModFGrain3)
hist(resid(ModFGrain3)) 
shapiro.test(resid(ModFGrain3))  # p=0.04943
plot(fitted(ModFGrain3),resid(ModFGrain3),pch=16)   # fan shape
qqnorm(resid(ModFGrain3)) # right tail pronounced
qqline(resid(ModFGrain3))
rsq(ModFGrain3) # 0.505
#ModFGrain1 lmer 
ModFGrain4 <- lmer(log(FGrain)~Treatment+(1|Block),data=Field)
anova(ModFGrain4)
summary(ModFGrain4)
hist(resid(ModFGrain4))  
shapiro.test(resid(ModFGrain4))  # p= 0.3143
plot(fitted(ModFGrain4),resid(ModFGrain4),pch=16)  # V shape
qqnorm(resid(ModFGrain4)) # slight tails
qqline(resid(ModFGrain4))
rsq(ModFGrain4) # 0.547
#Comparing models - decent rsq and AIC?BIC = ModFGrain4
#rsq values: ModFGrain1=0.149, ModFGrain2=0.684, ModFGrain3=0.505, ModFGrain4 =0.547
FGrain_modlist <- list(ModFGrain1, ModFGrain2, ModFGrain3, ModFGrain4)
AIC_values <- sapply(FGrain_modlist, AIC)
BIC_values <- sapply(FGrain_modlist, BIC)
N_AB <- data.frame(Model=c("ModFGrain1", "ModFGrain2", "ModFGrain3", "ModFGrain4"),
                   AIC_values, BIC_values)
print(N_AB)
  #Model AIC_values BIC_values
  #1 ModFGrain1   22.47981   34.26035
  #2 ModFGrain2   22.47981   34.26035
  #3 ModFGrain3  333.53229  345.31283
  #4 ModFGrain4   41.29446   50.71889
#emmeans 
ModFGrainem <- emmeans(ModFGrain2,~Treatment, alpha=0.1, type = "response")
ModFGrainem_cld <- cld(ModFGrainem, Letters = letters, type = "response") 
ModFGrainem_cld <- ModFGrainem_cld %>% rename(emmean = "response")
View(ModFGrainem_cld)
write.csv(ModFGrainem_cld, file="Field_Grain.csv")
##Developing visualizations
par(mar=c(5,6,4,2)+0.2) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFGrainem_cld, aes(x = Treatment, y = response, fill=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=response+SE), size=4, vjust=-1) +
  labs(x = "Treatments", y = "Canola grain yield (g)") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  scale_y_continuous(limits = c(-10, 70))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("LargePlots_Grain.jpg", width = 12, height = 8, dpi = 600)


#####   BIOMASS   #####
ModFGrainem_cld$origin <- "FGrain"
ModFStrawem_cld$origin <- "FStraw"
BiomassFieldEm <- rbind(ModFGrainem_cld,ModFStrawem_cld)
BiomassFieldEm <- as.data.frame(BiomassFieldEm)
View(BiomassFieldEm)
# Plot option 3 - side by side
ggplot(BiomassFieldEm, aes(x=Treatment, y=emmean, pattern=origin)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE), size=6, vjust=-1, position = position_dodge(width = 0.9))+
  labs(x = "Treatment", y = "Canola grain and straw yield (kg/ha)", pattern="") +
  scale_pattern_manual(values = c("FGrain" = "stripe", "FStraw" = "crosshatch"), 
                       labels = c("Grain", "Straw"))+
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  scale_y_continuous(limits = c(0, 3000))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=20, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 24, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("LarePlots_biomass.jpg", width = 12, height = 8, dpi = 500)




#####   N UPTAKE   ########
FieldNup_Mean <- summary_by(Nuptake~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldNup_Mean <- as.numeric(FieldNup_Mean$Nuptake)
FieldNup_skew <- skewness(FieldNup_Mean,na.rm=TRUE)
FieldNup_kur <- kurtosis(FieldNup_Mean,na.rm=TRUE)
cat("Skewness:", FieldNup_skew, "\n") # 0.7926221 
cat("Kurtosis:", FieldNup_kur, "\n") # 1.19171 
shapiro.test(Field$Nuptake) # p=0.02798
hist(Field$Nuptake) #  missing pieces to right
leveneTest(Nuptake~Treatment, data=Field)  # 0.3655
# transform
shapiro.test(log(Field$Nuptake)) # p=0.09166
hist(log(Field$Nuptake)) #  
leveneTest(log(Nuptake)~Treatment, data=Field)  # 0.4053
#ModFieldNup1 - all SE values are identical
ModFieldNup1 <- aov(log(Nuptake)~Treatment+Block, data=Field)
anova(ModFieldNup1)
summary(ModFieldNup1)
hist(resid(ModFieldNup1))
shapiro.test(resid(ModFieldNup1))  # p=0.7751
plot(fitted(ModFieldNup1),resid(ModFieldNup1),pch=16) #slightly right  skewed but very random
qqnorm(resid(ModFieldNup1)) # slight right tail
qqline(resid(ModFieldNup1))
ModFieldNup1_tidy <- tidy(ModFieldNup1)
ModFieldNup1sum_sq_reg <- ModFieldNup1_tidy$sumsq[1] 
ModFieldNup1sum_sq_resid <- ModFieldNup1_tidy$sumsq[2]
ModFieldNup1sum_sq_reg / (ModFieldNup1sum_sq_reg + ModFieldNup1sum_sq_resid) # rsq =0.746
# lm model with weighted least squares
ModFieldNupvar <- tapply(log(Field$Nuptake), Field$Treatment, var) 
weightsFNup <- 1 / ModFieldNupvar 
weightsFNup_full <- rep(weightsFNup, each = length(Field$Nuptake) / length(weightsFNup))
ModFieldNup2 <- lm(Nuptake ~ Treatment + Block, data=Field, weights=weightsFNup_full) 
hist(resid(ModFieldNup2))  # slight left skew
shapiro.test(resid(ModFieldNup2))  # p=0.1785
plot(fitted(ModFieldNup2),resid(ModFieldNup2),pch=16) 
qqnorm(resid(ModFieldNup2)) #right tail somewhat longer
qqline(resid(ModFieldNup2))
rsq(ModFieldNup2)  # 0.684
#emmeans 
ModFieldNupem <- emmeans(ModFieldNup1,~Treatment)
ModFieldNupem_cld <- cld(ModFieldNupem, Letters = letters, type="response") 
View(ModFieldNupem_cld)
write.csv(ModFieldNupem_cld, file="Field_Nuptake.csv")
# Visualizations
par(mar=c(5,6,4,2)+0.2) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFieldNupem_cld, aes(x = Treatment, y = response, fill=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=response+SE), size=8, vjust=-1) +
  labs(x = "Treatments", y = "Canola Nitrogen uptake (ug)") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  scale_y_continuous(limits = c(-10, 70))+
  theme_bw() +
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"),
        legend.text=element_text(size=12))+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("LargePlots_Nuptake.jpg", width = 12, height = 8, dpi = 500)


#####   N RECOVERY   ########
FieldNrec_Mean <- summary_by(Nrecovery~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldNrec_Mean <- as.numeric(FieldNrec_Mean$Nrecovery)
FieldNrec_skew <- skewness(FieldNrec_Mean,na.rm=TRUE)
FieldNrec_kur <- kurtosis(FieldNrec_Mean,na.rm=TRUE)
cat("Skewness:", FieldNrec_skew, "\n") # 1.909018
cat("Kurtosis:", FieldNrec_kur, "\n") # 3.260107
shapiro.test(Field$Nrecovery) # p=0.0003128
hist(Field$Nrecovery) #  severe left skew
leveneTest(Nrecovery~Treatment, data=Field)  # 0.2388
# Nrecovery has missing values for Control1 
FNrec_out <- Field[complete.cases(Field$Nrecovery),] #set up a subset removing the missing data from column Nrecovery
View(FNrec_out)
leveneTest(Nrecovery~Treatment, data=FNrec_out) # 0.2388
hist(FNrec_out$Nrecovery)
shapiro.test(FNrec_out$Nrecovery) #0.0003128
#transform
shapiro.test(log(Field$Nrecovery)) # p=0.7052
hist(log(FNrec_out$Nrecovery)) #  severe left skew
leveneTest(log(Nrecovery)~Treatment, data=FNrec_out)  # 0.2388
#ModFieldNrec1 - all SE values are identical
ModFieldNrec1 <- aov(Nrecovery~Treatment+Block, data=FNrec_out)
anova(ModFieldNrec1)
summary(ModFieldNrec1)
hist(resid(ModFieldNrec1))
shapiro.test(resid(ModFieldNrec1))  # p=0.2044
plot(fitted(ModFieldNrec1),resid(ModFieldNrec1),pch=16) # not quite normal
qqnorm(resid(ModFieldNrec1)) # long tails
qqline(resid(ModFieldNrec1))
ModFieldNrec1_tidy <- tidy(ModFieldNrec1)
ModFieldNrec1sum_sq_reg <- ModFieldNrec1_tidy$sumsq[1] 
ModFieldNrec1sum_sq_resid <- ModFieldNrec1_tidy$sumsq[2]
ModFieldNrec1sum_sq_reg / (ModFieldNrec1sum_sq_reg + ModFieldNrec1sum_sq_resid) # rsq =0.475
# lm model with weighted least squares
ModFieldNrecvar <- tapply(FNrec_out$Nrecovery, FNrec_out$Treatment, var) 
weightsFNrec <- 1 / ModFieldNrecvar 
weightsFNrec_full <- rep(weightsFNrec, each = length(FNrec_out$Nrecovery) / length(weightsFNrec))
ModFieldNrec2 <- lm(Nrecovery ~ Treatment + Block, data=FNrec_out, weights=weightsFNrec_full) 
hist(resid(ModFieldNrec2))  # slight left skew
shapiro.test(resid(ModFieldNrec2))  # p=0.001184
plot(fitted(ModFieldNrec2),resid(ModFieldNrec2),pch=16) # towards the bottom of the graph
qqnorm(resid(ModFieldNrec2)) # very long right tail
qqline(resid(ModFieldNuField))
rsq(ModFieldNrec2)  #0.869
#emmeans 
ModFieldNrecem <- emmeans(ModFieldNrec1,~Treatment)
ModFieldNrecem_cld <- cld(ModFieldNrecem, Letters = letters, type="response") 
View(ModFieldNrecem_cld)
write.csv(ModFieldNrecem_cld, file="Field_Nrecovery.csv")
# Visualizations
par(mar=c(5,6,4,2)+0.2) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFieldNrecem_cld, aes(x = Treatment, y = emmean, fill=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE), size=4, vjust=-1) +
  labs(x = "Treatments", y = "Canola Nitrogen uptake (ug)") +
  scale_x_discrete(labels = c("Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("LargePlots_Nreocvery.jpg", width = 8, height = 8, dpi = 600)



#####   P UPTAKE   ########
FieldPup_Mean <- summary_by(Puptake~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldPup_Mean <- as.numeric(FieldPup_Mean$Puptake)
FieldPup_skew <- skewness(FieldPup_Mean,na.rm=TRUE)
FieldPup_kur <- kurtosis(FieldPup_Mean,na.rm=TRUE)
cat("Skewness:", FieldPup_skew, "\n") # 0.05881886
cat("Kurtosis:", FieldPup_kur, "\n") #  -1.003361 
shapiro.test(Field$Puptake) # p=0.8292
hist(Field$Puptake) 
leveneTest(Puptake~Treatment, data=Field)  # 0.08774
#ModFieldPup1
ModFieldPup1 <- aov(Puptake~Treatment+Block, data=Field)
anova(ModFieldPup1)
summary(ModFieldPup1)
hist(resid(ModFieldPup1))
shapiro.test(resid(ModFieldPup1))  # p=0.6408
plot(fitted(ModFieldPup1),resid(ModFieldPup1),pch=16) 
qqnorm(resid(ModFieldPup1)) # slight right tail
qqline(resid(ModFieldPup1))
ModFieldPup1_tidy <- tidy(ModFieldPup1)
ModFieldPup1sum_sq_reg <- ModFieldPup1_tidy$sumsq[1] 
ModFieldPup1sum_sq_resid <- ModFieldPup1_tidy$sumsq[2]
ModFieldPup1sum_sq_reg / (ModFieldPup1sum_sq_reg + ModFieldPup1sum_sq_resid) # 0.279
# lm model with weighted least squares
ModFieldPupvar <- tapply(Field$Puptake, Field$Treatment, var, na.rm=TRUE)
weightsFPup <- 1 / ModFieldPupvar
weightsFPup_full <- rep(weightsFPup, each = length(Field$Puptake) / length(weightsFPup))
ModFieldPup2 <- lm(Puptake ~ Treatment + Block, data=Field, weights=weightsFPup_full) 
hist(resid(ModFieldPup2))  
shapiro.test(resid(ModFieldPup2))  # p= 0.5431
plot(fitted(ModFieldPup2),resid(ModFieldPup2),pch=16) #slightly left  skewed but very random
qqnorm(resid(ModFieldPup2)) 
qqline(resid(ModFieldPup2))
rsq(ModFieldPup2)  # 0.512
#emmeans on lm model
ModFieldPupem <- emmeans(ModFieldPup2,~Treatment)
ModFieldPupem_cld <- cld(ModFieldPupem, Letters = letters, type="response") 
View(ModFieldPupem_cld)
write.csv(ModFieldPupem_cld, file="Field_Puptake.csv")
# Plotting the summary data
par(mar=c(5,6,4,2)+0.1) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFieldPupem_cld, aes(x = Treatment, y = emmean, fill=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE), size=4, vjust=-1) +
  labs(x = "Treatments", y = "Canola Nitrogen uptake (ug)") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Field_Puptake.jpg", width = 8, height = 8, dpi = 600)



#####   P RECOVERY   ########
FieldPrec_Mean <- summary_by(Precovery~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldPrec_Mean <- as.numeric(FieldPrec_Mean$Precovery)
FieldPrec_skew <- skewness(FieldPrec_Mean,na.rm=TRUE)
FieldPrec_kur <- kurtosis(FieldPrec_Mean,na.rm=TRUE)
cat("Skewness:", FieldPrec_skew, "\n") # 0.423411 
cat("Kurtosis:", FieldPrec_kur, "\n") # -1.044605 
shapiro.test(Field$Precovery) # p=0.5601
hist(Field$Precovery) 
leveneTest(Precovery~Treatment, data=Field)  # 0.2463
#ModFieldPrec1
ModFieldPrec1 <- aov(Precovery~Treatment+Block, data=Field)
anova(ModFieldPrec1)
summary(ModFieldPrec1)
hist(resid(ModFieldPrec1))
shapiro.test(resid(ModFieldPrec1))  # p=0.6397
plot(fitted(ModFieldPrec1),resid(ModFieldPrec1),pch=16) #slightly left skewed but very random
qqnorm(resid(ModFieldPrec1))
qqline(resid(ModFieldPrec1))
ModFieldPrec1_tidy <- tidy(ModFieldPrec1)
ModFieldPrec1sum_sq_reg <- ModFieldPrec1_tidy$sumsq[1] 
ModFieldPrec1sum_sq_resid <- ModFieldPrec1_tidy$sumsq[2]
ModFieldPrec1sum_sq_reg / (ModFieldPrec1sum_sq_reg + ModFieldPrec1sum_sq_resid) # 0.369
# lm model with weighted least squares
ModFieldPrecvar <- tapply(Field$Precovery, Field$Treatment, var, na.rm=TRUE)
weightsFPrec <- 1 / ModFieldPrecvar
weightsFPrec_full <- rep(weightsFPrec, each = length(Field$Precovery) / length(weightsFPrec))
ModFieldPrec2 <- lm(Precovery ~ Treatment + Block, data=Field, weights=weightsFPrec_full) 
hist(resid(ModFieldPrec2))  # slight left skew
shapiro.test(resid(ModFieldPrec2))  # p= 0.4504
plot(fitted(ModFieldPrec2),resid(ModFieldPrec2),pch=16) #slightly left  skewed but very random
qqnorm(resid(ModFieldPrec2)) #slight tails
qqline(resid(ModFieldPrec2))
rsq(ModFieldPrec2)  # 0.601
#emmeans on lm model
ModFieldPrecem <- emmeans(ModFieldPrec1,~Treatment)
ModFieldPrecem_cld <- cld(ModFieldPrecem, Letters = letters, type="response") 
View(ModFieldPrecem_cld)
write.csv(ModFieldPrecem_cld, file="Field_Precovery.csv")
# Plotting the summary data
par(mar=c(5,6,4,2)+0.1) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFieldPrecem_cld, aes(x = Treatment, y = emmean, fill=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean-SE), size=4, vjust=1) +
  labs(x = "Treatments", y = "Canola Nitrogen uptake (ug)") +
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Field_Precovery.jpg", width = 8, height = 8, dpi = 600)





#####   BULK DENSITY - WET   ########
FieldBDW_Mean <- summary_by(BDWet~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldBDW_Mean <- as.numeric(FieldBDW_Mean$BDWet)
FieldBDW_skew <- skewness(FieldBDW_Mean,na.rm=TRUE)
FieldBDW_kur <- kurtosis(FieldBDW_Mean,na.rm=TRUE)
cat("Skewness:", FieldBDW_skew, "\n") # -0.1449922 
cat("Kurtosis:", FieldBDW_kur, "\n") # -0.8374298
shapiro.test(Field$BDWet) # p=0.5904
hist(Field$BDWet) 
leveneTest(BDWet~Treatment, data=Field)  # 0.1659
#ModFieldPup1
ModFieldPup1 <- aov(Puptake~Treatment+Block, data=Field)
anova(ModFieldPup1)
summary(ModFieldPup1)
hist(resid(ModFieldPup1))
shapiro.test(resid(ModFieldPup1))  # p=0.6417
plot(fitted(ModFieldPup1),resid(ModFieldPup1),pch=16) #slightly left skewed but very random
qqnorm(resid(ModFieldPup1))
qqline(resid(ModFieldPup1))
ModFieldPup1_tidy <- tidy(ModFieldPup1)
ModFieldPup1sum_sq_reg <- ModFieldPup1_tidy$sumsq[1] 
ModFieldPup1sum_sq_resid <- ModFieldPup1_tidy$sumsq[2]
ModFieldPup1sum_sq_reg / (ModFieldPup1sum_sq_reg + ModFieldPup1sum_sq_resid) # 0.7522534
# lm model with weighted least squares
ModFieldPupvar <- tapply(Field$Puptake, Field$Treatment, var, na.rm=TRUE)
weightsPup <- 1 / ModFieldPupvar
weightsPup_full <- rep(weightsPup, each = length(Field$Puptake) / length(weightsPup))
ModFieldPuField <- lm(Puptake ~ Treatment + Block, data=Field, weights=weightsPup_full) 
hist(resid(ModFieldPuField))  # slight left skew
shapiro.test(resid(ModFieldPuField))  # p= 0.2411
plot(fitted(ModFieldPuField),resid(ModFieldPuField),pch=16) #slightly left  skewed but very random
qqnorm(resid(ModFieldPuField)) #left tail somewhat longer
qqline(resid(ModFieldPuField))
rsq(ModFieldPuField)  # 0.6712941
#emmeans on lm model
ModFieldemPup <- emmeans(ModFieldPuField,~Treatment, type="response")
ModFieldemPup_cld <- cld(ModFieldemPup, Letters = letters) 
View(ModFieldemPup_cld)
write.csv(ModFieldemPup_cld, file="Field_Puptake.csv")
# Plotting the summary data
par(mar=c(5,6,4,2)+0.1) #c(bottom, left, top, right) + 0.1 lines
ggplot(ModFieldemPup_cld, aes(x=Treatment, y=emmean)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=.group, y=emmean+SE), size=4, vjust=-1)+
  labs(x = "Treatment", y = "Wheat P uptake (ug)") +
  scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", "Meat and\nBone Meal -\nCoarse",
                              "Meat and\nBone Meal -\nFine", "Fertilizer\nPhosphorus"))+
  theme_bw() +
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_text(size = 14, face="bold"), axis.title.y = element_text(size = 14, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Field_Puptake.jpg", width = 8, height = 8, dpi = 600)




#####   BULK DENSITY - DRY   ########




#####   VOLUMETRIC MOISTURE - WET   ########




#####   VOLUMETRIC MOISTURE - DRY   ########






#####   SOIL NO3   ########
FieldSNO3_Mean <- summary_by(SNO3~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldSNO3_Mean <- as.numeric(FieldSNO3_Mean$SNO3)
FieldSNO3_skew <- skewness(FieldSNO3_Mean,na.rm=TRUE)
FieldSNO3_kur <- kurtosis(FieldSNO3_Mean,na.rm=TRUE)
cat("Skewness:", FieldSNO3_skew, "\n") # 2.721196 
cat("Kurtosis:", FieldSNO3_kur, "\n") # 9.18808 
shapiro.test(Field$SNO3) # p=2.965e-06
hist(Field$SNO3) #  extreme left skew
leveneTest(SNO3~Treatment, data=Field)  # P=0.3258
#trnasformations
shapiro.test(log(Field$SNO3)) # p=0.2487
hist(log(Field$SNO3)) #  left skew
leveneTest(log(SNO3)~Treatment, data=Field)  # P=0.2182
#ModFieldSNO31
ModFieldSNO31 <- aov(log(SNO3)~Treatment+Block, data=Field)
anova(ModFieldSNO31)
summary(ModFieldSNO31)
hist(resid(ModFieldSNO31))
shapiro.test(resid(ModFieldSNO31))  # p=0.07536
plot(fitted(ModFieldSNO31),resid(ModFieldSNO31),pch=16) 
qqnorm(resid(ModFieldSNO31)) # long right tail
qqline(resid(ModFieldSNO31))
ModFieldSNO31_tidy <- tidy(ModFieldSNO31)
ModFieldSNO31sum_sq_reg <- ModFieldSNO31_tidy$sumsq[1] 
ModFieldSNO31sum_sq_resid <- ModFieldSNO31_tidy$sumsq[2]
ModFieldSNO31sum_sq_reg / (ModFieldSNO31sum_sq_reg + ModFieldSNO31sum_sq_resid) # rsq=0.3888387
# lm model
ModFieldSNO32 <- lm(log(SNO3)~Treatment+Block, data=Field)
anova(ModFieldSNO32)
summary(ModFieldSNO32)
hist(resid(ModFieldSNO32))
shapiro.test(resid(ModFieldSNO32))  # p=0.07536
plot(fitted(ModFieldSNO32),resid(ModFieldSNO32),pch=16) 
qqnorm(resid(ModFieldSNO32)) # long right tail
qqline(resid(ModFieldSNO32))
rsq(ModFieldSNO32) # rsq=0.6791344
#emmeans 
ModFieldemSNO3 <- emmeans(ModFieldSNO32,~Treatment, type="response")
ModFieldemSNO3_cld <- cld(ModFieldemSNO3, Letters = letters) 
View(ModFieldemSNO3_cld)
write.csv(ModFieldemSNO3_cld, file="Field_SoilNO3.csv")



#####   SOIL PO4   ########
FieldSPO4_Mean <- summary_by(SPO4~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldSPO4_Mean <- as.numeric(FieldSPO4_Mean$SPO4)
FieldSPO4_skew <- skewness(FieldSPO4_Mean,na.rm=TRUE)
FieldSPO4_kur <- kurtosis(FieldSPO4_Mean,na.rm=TRUE)
cat("Skewness:", FieldSPO4_skew, "\n") # 0.7319427 
cat("Kurtosis:", FieldSPO4_kur, "\n") # -0.7178281 
shapiro.test(Field$SPO4) # p=0.007283
hist(Field$SPO4) #  left skew
leveneTest(SPO4~Treatment, data=Field)  # P=0.5364
# transform
shapiro.test(log(Field$SPO4)) # p=0.1813
hist(log(Field$SPO4)) 
leveneTest(log(SPO4)~Treatment, data=Field)  # P=0.8856
#ModFieldSPO41
ModFieldSPO41 <- aov(log(SPO4)~Treatment+Block, data=Field)
anova(ModFieldSPO41)
summary(ModFieldSPO41)
hist(resid(ModFieldSPO41))
shapiro.test(resid(ModFieldSPO41))  # p= 0.3196
plot(fitted(ModFieldSPO41),resid(ModFieldSPO41),pch=16) 
qqnorm(resid(ModFieldSPO41))
qqline(resid(ModFieldSPO41))
ModFieldSPO41_tidy <- tidy(ModFieldSPO41)
ModFieldSPO41sum_sq_reg <- ModFieldSPO41_tidy$sumsq[1] 
ModFieldSPO41sum_sq_resid <- ModFieldSPO41_tidy$sumsq[2]
ModFieldSPO41sum_sq_reg / (ModFieldSPO41sum_sq_reg + ModFieldSPO41sum_sq_resid) #0.8786474
#emmeans 
ModFieldemSPO4 <- emmeans(ModFieldSPO41,~Treatment, type="response")
ModFieldemSPO4_cld <- cld(ModFieldemSPO4, Letters = letters) 
View(ModFieldemSPO4_cld)
write.csv(ModFieldemSPO4_cld, file="Field_SoilPO4.csv")




#####   WATER SOLUBLE P   ########
FieldWSP_Mean <- summary_by(WatSolP~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldWSP_Mean <- as.numeric(FieldWSP_Mean$WatSolP)
FieldWSP_skew <- skewness(FieldWSP_Mean,na.rm=TRUE)
FieldWSP_kur <- kurtosis(FieldWSP_Mean,na.rm=TRUE)
cat("Skewness:", FieldWSP_skew, "\n") # 1.575096 
cat("Kurtosis:", FieldWSP_kur, "\n") # 3.261601 
shapiro.test(Field$WatSolP) # p=0.001116
hist(Field$WatSolP) #  left skew
leveneTest(WatSolP~Treatment, data=Field)  # P=0.3716
# transform
shapiro.test(log(Field$WatSolP)) # p=0.1204
hist(log(Field$WatSolP)) #  slight left skew
leveneTest(log(WatSolP)~Treatment, data=Field)  # P=0.4992
#ModFieldWSP1
ModFieldWSP1 <- aov(log(WatSolP)~Treatment+Block, data=Field)
anova(ModFieldWSP1)
summary(ModFieldWSP1)
hist(resid(ModFieldWSP1))
shapiro.test(resid(ModFieldWSP1))  # p=0.4645
plot(fitted(ModFieldWSP1),resid(ModFieldWSP1),pch=16) 
qqnorm(resid(ModFieldWSP1))
qqline(resid(ModFieldWSP1))
ModFieldWSP1_tidy <- tidy(ModFieldWSP1)
ModFieldWSP1sum_sq_reg <- ModFieldWSP1_tidy$sumsq[1] 
ModFieldWSP1sum_sq_resid <- ModFieldWSP1_tidy$sumsq[2]
ModFieldWSP1sum_sq_reg / (ModFieldWSP1sum_sq_reg + ModFieldWSP1sum_sq_resid) #0.9799464
#emmeans 
ModFieldemWSP <- emmeans(ModFieldWSP1,~Treatment, type="response")
ModFieldemWSP_cld <- cld(ModFieldemWSP, Letters = letters) 
View(ModFieldemWSP_cld)
write.csv(ModFieldemWSP_cld, file="Field_WatSolP.csv")



#####   SOIL RESIN P   ########
FieldResP_Mean <- summary_by(ResinP~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldResP_Mean <- as.numeric(FieldResP_Mean$ResinP)
FieldResP_skew <- skewness(FieldResP_Mean,na.rm=TRUE)
FieldResP_kur <- kurtosis(FieldResP_Mean,na.rm=TRUE)
cat("Skewness:", FieldResP_skew, "\n") # 1.404125 
cat("Kurtosis:", FieldResP_kur, "\n") # 2.710147  
shapiro.test(Field$ResinP) # p=0.001659
hist(Field$ResinP) #  left skew
leveneTest(ResinP~Treatment, data=Field)  # P=0.0674
shapiro.test(log(Field$ResinP))  #p=0.2711
leveneTest(log(ResinP)~Treatment, data=Field)  # p=0.0627
#ModFieldResP1
ModFieldResP1 <- aov(log(ResinP)~Treatment+Block, data=Field)
anova(ModFieldesP1)
summary(ModFieldesP1)
hist(resid(ModFieldesP1))
shapiro.test(resid(ModFieldesP1))  # p=0.4569
plot(fitted(ModFieldesP1),resid(ModFieldesP1),pch=16) 
qqnorm(resid(ModFieldesP1))
qqline(resid(ModFieldesP1))
ModFieldesP1_tidy <- tidy(ModFieldesP1)
ModFieldesP1sum_sq_reg <- ModFieldesP1_tidy$sumsq[1] 
ModFieldesP1sum_sq_resid <- ModFieldesP1_tidy$sumsq[2]
ModFieldesP1sum_sq_reg / (ModFieldesP1sum_sq_reg + ModFieldesP1sum_sq_resid) #0.539
#emmeans 
ModFieldemResP1 <- emmeans(ModFieldesP1,~Treatment, type="response")
ModFieldemResP1_cld <- cld(ModFieldemNrec, Letters = letters) 
View(ModFieldemResP1_cld)
write.csv(ModFieldemResP1_cld, file="Field_resinP.csv")



#####   pH   ########
FieldpH_Mean <- summary_by(pH~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldpH_Mean <- as.numeric(FieldpH_Mean$pH)
FieldpH_skew <- skewness(FieldpH_Mean,na.rm=TRUE)
FieldpH_kur <- kurtosis(FieldpH_Mean,na.rm=TRUE)
cat("Skewness:", FieldpH_skew, "\n") # 0.8153002 
cat("Kurtosis:", FieldpH_kur, "\n") # -0.06151628  
shapiro.test(Field$pH) # p=0.01752
hist(Field$pH) # left skew
leveneTest(pH~Treatment, data=Field)  # P=0.9357
#transform
shapiro.test(log(Field$pH)) # p=0.02644
hist(log(Field$pH)) # left skew
leveneTest(log(pH)~Treatment, data=Field)  # P=0.9321
shapiro.test(sqrt(Field$pH)) # p=0.02156
hist(sqrt(Field$pH)) # left skew
leveneTest(sqrt(pH)~Treatment, data=Field)  # P=0.934
#ModFieldpH1
ModFieldpH1 <- aov(pH~Treatment+Block, data=Field)
anova(ModFieldpH1)
summary(ModFieldpH1)
hist(resid(ModFieldpH1))
shapiro.test(resid(ModFieldpH1))  # p=0.1862
plot(fitted(ModFieldpH1),resid(ModFieldpH1),pch=16) # clusters to each side
qqnorm(resid(ModFieldpH1)) # medium tails
qqline(resid(ModFieldpH1))
ModFieldpH1_tidy <- tidy(ModFieldpH1)
ModFieldpH1sum_sq_reg <- ModFieldpH1_tidy$sumsq[1] 
ModFieldpH1sum_sq_resid <- ModFieldpH1_tidy$sumsq[2]
ModFieldpH1sum_sq_reg / (ModFieldpH1sum_sq_reg + ModFieldpH1sum_sq_resid) #0.05141096
# weighted lm model
ModFieldpHvar <- tapply(Field$pH, Field$Treatment, var, na.rm=TRUE)
weightspH <- 1 / ModFieldpHvar
weightspH_full <- rep(weightspH, each = length(Field$pH) / length(weightspH))
ModFieldpH2 <- lm(pH ~ Treatment + Block, data=Field, weights=weightspH_full) 
anova(ModFieldpH2)
summary(ModFieldpH2)
hist(resid(ModFieldpH2)) 
shapiro.test(resid(ModFieldpH2))  # p=0.4605
plot(fitted(ModFieldpH2),resid(ModFieldpH2),pch=16) 
qqnorm(resid(ModFieldpH2)) # tails, especially on right
qqline(resid(ModFieldpH2))
rsq(ModFieldpH2) # rsq=0.4528747
#emmeans 
ModFieldempH1 <- emmeans(ModFieldpH2,~Treatment, type="response")
ModFieldempH1_cld <- cld(ModFieldempH1, Letters = letters) 
View(ModFieldempH1_cld)
write.csv(ModFieldempH1_cld, file="Field_pH.csv")




#####   ELECTROCAL CONDUCTIVITY   ########
FieldEC_Mean <- summary_by(EC~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldEC_Mean <- as.numeric(FieldEC_Mean$EC)
FieldEC_skew <- skewness(FieldEC_Mean,na.rm=TRUE)
FieldEC_kur <- kurtosis(FieldEC_Mean,na.rm=TRUE)
cat("Skewness:", FieldEC_skew, "\n") # 1.293181 
cat("Kurtosis:", FieldEC_kur, "\n") # 0.7705139
hist(Field$EC) # slight left skew
shapiro.test(Field$EC) # p=0.0001406
leveneTest(EC~Treatment, data=Field)  # P=0.8016
# transform
shapiro.test(log(Field$EC))  # p=0.05701
leveneTest(log(EC)~Treatment, data=Field)  # p =  0.7405
shapiro.test(log10(Field$EC))  # p=0.05701
leveneTest(log10(EC)~Treatment, data=Field)  # p =  0.7405
shapiro.test(sqrt(Field$EC))  # p=0.003086
leveneTest(sqrt(EC)~Treatment, data=Field)  # p = 0.7786
#ModFieldEC1
ModFieldEC1 <- aov(log(EC)~Treatment+Block, data=Field)
anova(ModFieldEC1)
summary(ModFieldEC1)
hist(resid(ModFieldEC1))
shapiro.test(resid(ModFieldEC1))  # p=0.1893
plot(fitted(ModFieldEC1),resid(ModFieldEC1),pch=16) 
qqnorm(resid(ModFieldEC1)) # medium right tail
qqline(resid(ModFieldEC1))
ModFieldEC1_tidy <- tidy(ModFieldEC1)
ModFieldEC1sum_sq_reg <- ModFieldEC1_tidy$sumsq[1] 
ModFieldEC1sum_sq_resid <- ModFieldEC1_tidy$sumsq[2]
ModFieldEC1sum_sq_reg / (ModFieldEC1sum_sq_reg + ModFieldEC1sum_sq_resid) # 0.3595186
# weighted lm model
ModFieldECvar <- tapply(Field$EC, Field$Treatment, var, na.rm=TRUE)
weightsEC <- 1 / ModFieldECvar
weightsEC_full <- rep(weightsEC, each = length(Field$EC) / length(weightsEC))
ModFieldEC2 <- lm(EC ~ Treatment + Block, data=Field, weights=weightsEC_full) 
anova(ModFieldEC2)
summary(ModFieldEC2)
hist(resid(ModFieldEC2)) # left skew
shapiro.test(resid(ModFieldEC2))  # p=0.002185
plot(fitted(ModFieldEC2),resid(ModFieldEC2),pch=16) # cluster towards middel bottom
qqnorm(resid(ModFieldEC2)) # big right tail
qqline(resid(ModFieldEC2))
rsq(ModFieldEC2) # rsq=0.5976484
#emmeans 
ModFieldemEC1 <- emmeans(ModFieldEC2,~Treatment, type="response")
ModFieldemEC1_cld <- cld(ModFieldemEC1, Letters = letters) 
View(ModFieldemEC1_cld)
write.csv(ModFieldemEC1_cld, file="Field_EC.csv")




#####   ORGANIC CARBON   ########
FieldOC_Mean <- summary_by(OC~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldOC_Mean <- as.numeric(FieldOC_Mean$OC)
FieldOC_skew <- skewness(FieldOC_Mean,na.rm=TRUE)
FieldOC_kur <- kurtosis(FieldOC_Mean,na.rm=TRUE)
cat("Skewness:", FieldOC_skew, "\n") # 2.122192 
cat("Kurtosis:", FieldOC_kur, "\n") # 4.694283 
shapiro.test(Field$OC) # p=5.96e-06
hist(Field$OC) #  heavy left skew
leveneTest(OC~Treatment, data=Field)  # P=1.097e-05
#Transforming
shapiro.test(log(Field$OC)) # p=0.0002123
leveneTest(log(OC)~Treatment, data=Field)  # P=0.0008862
shapiro.test(log10(Field$OC)) # p=0.0002123
leveneTest(log10(OC)~Treatment, data=Field)  # P=0.0008862
shapiro.test(sqrt(Field$OC)) # p=3.431e-05
leveneTest(sqrt(OC)~Treatment, data=Field)  # P=0.000106 
#ModFieldOC1
ModFieldOC1 <- aov(log(OC)~Treatment+Block, data=Field)
anova(ModFieldOC1)
summary(ModFieldOC1)
hist(resid(ModFieldOC1))
shapiro.test(resid(ModFieldOC1))  # p=0.3491
plot(fitted(ModFieldOC1),resid(ModFieldOC1),pch=16) # heavy left cluster
qqnorm(resid(ModFieldOC1)) # slight left tail
qqline(resid(ModFieldOC1))
ModFieldOC1_tidy <- tidy(ModFieldOC1)
ModFieldOC1sum_sq_reg <- ModFieldOC1_tidy$sumsq[1] 
ModFieldOC1sum_sq_resid <- ModFieldOC1_tidy$sumsq[2]
ModFieldOC1sum_sq_reg / (ModFieldOC1sum_sq_reg + ModFieldOC1sum_sq_resid) # 0.9227297
# lm model with weighted least squares
ModFieldNupvar <- tapply(log(Field$Nuptake), Field$Treatment, var) 
weightsFNup <- 1 / ModFieldNupvar 
weightsFNup_full <- rep(weightsFNup, each = length(Field$Nuptake) / length(weightsFNup))
ModFieldNup2 <- lm(Nuptake ~ Treatment + Block, data=Field, weights=weightsFNup_full) 
hist(resid(ModFieldNup2))  # slight left skew
shapiro.test(resid(ModFieldNup2))  # p=0.1785
plot(fitted(ModFieldNup2),resid(ModFieldNup2),pch=16) 
qqnorm(resid(ModFieldNup2)) #right tail somewhat longer
qqline(resid(ModFieldNup2))
rsq(ModFieldNup2)  # 0.684
#emmeans 
ModFieldemOC1 <- emmeans(ModFieldOC1,~Treatment, type="response")
ModFieldemOC1_cld <- cld(ModFieldemNrec, Letters = letters) 
View(ModFieldemOC1_cld)
write.csv(ModFieldemOC1_cld, file="Field_OC.csv")




#####   NO3 LOAD - SNOWMELT   ########
FieldLNO3_Mean <- summary_by(LNO3~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldLNO3_Mean <- as.numeric(FieldLNO3_Mean$LNO3)
FieldLNO3_skew <- skewness(FieldLNO3_Mean,na.rm=TRUE)
FieldLNO3_kur <- kurtosis(FieldLNO3_Mean,na.rm=TRUE)
cat("Skewness:", FieldLNO3_skew, "\n") #0.5753607 
cat("Kurtosis:", FieldLNO3_kur, "\n") #-0.808033 
shapiro.test(Field$LNO3) # p=0.1175
hist(Field$LNO3) #  left skew
leveneTest(LNO3~factor(Treatment), data=Field)  # P=0.5404
#ModFieldLNO31
ModFieldLNO3a <- aov(LNO3~Treatment+Block, data=Field)
anova(ModFieldLNO3a)
summary(ModFieldLNO3a, level=0.1)
hist(resid(ModFieldLNO3a))
shapiro.test(resid(ModFieldLNO3a))  # p= 0.5017
plot(fitted(ModFieldLNO3a),resid(ModFieldLNO3a),pch=16) # slight left cluster
qqnorm(resid(ModFieldLNO3a)) # slight right tail
qqline(resid(ModFieldLNO3a))
ModFieldLNO31_tidy <- tidy(ModFieldLNO3a)
ModFieldLNO31sum_sq_reg <- ModFieldLNO31_tidy$sumsq[1] 
ModFieldLNO31sum_sq_resid <- ModFieldLNO31_tidy$sumsq[2]
ModFieldLNO31sum_sq_reg / (ModFieldLNO31sum_sq_reg + ModFieldLNO31sum_sq_resid) # 0.863
# lm model with weighted least squares
ModFieldLNO3var <- tapply(Field$LNO3, Field$Treatment, var, na.rm=TRUE) 
weightsFLNO3 <- 1 / ModFieldLNO3var 
weightsFLNO3_full <- rep(weightsFLNO3, each = length(Field$LNO3) / length(weightsFLNO3))
ModFieldLNO3b <- lm(LNO3 ~ Treatment + Block, data=Field, weights=weightsFLNO3_full) 
hist(resid(ModFieldLNO3b))  # slight left skew
shapiro.test(resid(ModFieldLNO3b))  # p=0.3215
plot(fitted(ModFieldLNO3b),resid(ModFieldLNO3b),pch=16) 
qqnorm(resid(ModFieldLNO3b)) #right tail somewhat longer
qqline(resid(ModFieldLNO3b))
rsq(ModFieldLNO3b)  # 0.655
# weighted aov
LNO3_subset <- subset(Field, !is.na(LNO3))
FLNO3weights <-  1/resid(ModFieldLNO3a, subset=LNO3_subset)^2
ModFieldLNO3c <- aov(LNO3~Treatment+Block, data=LNO3_subset, weights= FLNO3weights)
anova(ModFieldLNO3c)
summary(ModFieldLNO3c)
hist(resid(ModFieldLNO3c))
shapiro.test(resid(ModFieldLNO3c))  # p=0.2846
plot(fitted(ModFieldLNO3c),resid(ModFieldLNO3c),pch=16) 
qqnorm(resid(ModFieldLNO3c))
qqline(resid(ModFieldLNO3c))
ModFieldLNO3c_tidy <- tidy(ModFieldLNO3c)
ModFieldLNO3csum_sq_reg <- ModFieldLNO3c_tidy$sumsq[1] 
ModFieldLNO3csum_sq_resid <- ModFieldLNO3c_tidy$sumsq[2]
ModFieldLNO3csum_sq_reg / (ModFieldLNO3csum_sq_reg + ModFieldLNO3csum_sq_resid) #0.0.899
#emmeans 
ModFieldemLNO3 <- emmeans(ModFieldLNO3c,~Treatment) 
ModFieldemLNO3_cld <- cld(ModFieldemLNO3, Letters = letters, type="response") 
View(ModFieldemLNO3_cld)
write.csv(ModFieldemLNO3_cld, file="Field_NO3Load.csv")




#####   NH4 LOAD - SNOWMELT   ########
FieldLNH4_Mean <- summary_by(LNH4~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldLNH4_Mean <- as.numeric(FieldLNH4_Mean$LNH4)
FieldLNH4_skew <- skewness(FieldLNH4_Mean,na.rm=TRUE)
FieldLNH4_kur <- kurtosis(FieldLNH4_Mean,na.rm=TRUE)
cat("Skewness:", FieldLNH4_skew, "\n") # 1.924935 /  2.028875
cat("Kurtosis:", FieldLNH4_kur, "\n") # 3.370188    /  3.518249 
shapiro.test(Field$LNH4) # p= 2.222e-05   // 6.638e-06
hist(Field$LNH4) #  heavy left skew
leveneTest(LNH4~factor(Treatment), data=Field)  # P=0.004341  / 0.0006837
# transform
shapiro.test(na.omit(log(Field$LNH4))) # p=0.6202
hist(log(Field$LNH4)) 
leveneTest(log(LNH4)~factor(Treatment), data=Field)  # P=0.4345
shapiro.test(sqrt(Field$LNH4)) # p=0.02159
hist(sqrt(Field$LNH4)) #  slight left skew
leveneTest(sqrt(LNH4)~factor(Treatment), data=Field)  # P=0.02392
#ModFieldLNH41
LNH4_subset <- subset(Field, !is.na(LNH4))
ModFieldLNH4a <- aov(log(LNH4)~Treatment+Block, data=Field)
anova(ModFieldLNH4a)
summary(ModFieldLNH4a)
hist(resid(ModFieldLNH4a)) # slight right skew
shapiro.test(resid(ModFieldLNH4a))  # p=0.05745
plot(fitted(ModFieldLNH4a),resid(ModFieldLNH4a),pch=16) # random but along upper half
qqnorm(resid(ModFieldLNH4a)) # slight tails
qqline(resid(ModFieldLNH4a))
ModFieldLNH41_tidy <- tidy(ModFieldLNH4a)
ModFieldLNH41sum_sq_reg <- ModFieldLNH41_tidy$sumsq[1] 
ModFieldLNH41sum_sq_resid <- ModFieldLNH41_tidy$sumsq[2]
ModFieldLNH41sum_sq_reg / (ModFieldLNH41sum_sq_reg + ModFieldLNH41sum_sq_resid) # 0.401
# lm model with weighted least squares
ModFieldLNH4var <- tapply(log(Field$LNH4), Field$Treatment, var, na.rm=TRUE) 
weightsFLNH4 <- 1 / ModFieldLNH4var 
weightsFLNH4_full <- rep(weightsFLNH4, each = length(Field$LNH4) / length(weightsFLNH4))
ModFieldLNH4b <- lm(LNH4 ~ Treatment + Block, data=Field, weights=weightsFLNH4_full) 
anova(ModFieldLNH4b)
summary(ModFieldLNH4b)
hist(resid(ModFieldLNH4b))  # slight right skew
shapiro.test(resid(ModFieldLNH4b))  # p=0.6344
plot(fitted(ModFieldLNH4b),resid(ModFieldLNH4b),pch=16) 
qqnorm(resid(ModFieldLNH4b)) 
qqline(resid(ModFieldLNH4b))
rsq(ModFieldLNH4b)  # 0.525
# weighted aov
LNH4_subset <- subset(Field, !is.na(LNH4))
FLNH4weights <-  1/resid(ModFieldLNH4a, subset=LNH4_subset)^2
ModFieldLNH4c <- aov(LNH4~Treatment+Block, data=LNH4_subset, weights= FLNH4weights)
anova(ModFieldLNH4c)
summary(ModFieldLNH4c)
hist(resid(ModFieldLNH4c))
shapiro.test(resid(ModFieldLNH4c))  # p=0.1769
plot(fitted(ModFieldLNH4c),resid(ModFieldLNH4c),pch=16) 
qqnorm(resid(ModFieldLNH4c))
qqline(resid(ModFieldLNH4c))
leveneTest(resid(ModFieldLNH4c))
ModFieldLNH4c_tidy <- tidy(ModFieldLNH4c)
ModFieldLNH4csum_sq_reg <- ModFieldLNH4c_tidy$sumsq[1] 
ModFieldLNH4csum_sq_resid <- ModFieldLNH4c_tidy$sumsq[2]
ModFieldLNH4csum_sq_reg / (ModFieldLNH4csum_sq_reg + ModFieldLNH4csum_sq_resid) #0.753
# AIC& BIC
FLNH4_modlist <- list(ModFieldLNH4a, ModFieldLNH4b, ModFieldLNH4c)
AIC_values <- sapply(FLNH4_modlist, AIC)
BIC_values <- sapply(FLNH4_modlist, BIC)
FLNH4AB <- data.frame(Model=c("ModFieldLNH4a", "ModFieldLNH4b", "ModFieldLNH4c"), AIC_values, BIC_values)
print(FLNH4AB)
#Model AIC_values BIC_values
#1 ModFieldLNH4a   82.06943   93.42437
#2 ModFieldLNH4b  -45.00245  -33.64751
#3 ModFieldLNH4c  -50.35223  -38.99728

#emmeans 
ModFieldemLNH4 <- emmeans(ModFieldLNH4c, ~Treatment, alpha=0.1)
ModFieldemLNH4_cld <- cld(ModFieldemLNH4, Letters = letters, type="response") 
View(ModFieldemLNH4_cld)
ModFieldemLNH4$contrasts
summary(ModFieldemLNH4)
levels(ModFieldLNH4c)
levels(ModFieldemLNH4)
plot(ModFieldLNH4c)
plot(ModFieldLNH4a)
emmip(ModFieldLNH4c, ~Treatment)
emmip(ModFieldLNH4a, ~Treatment)

ModFieldemLNH4a <- TukeyHSD(ModFieldLNH4c)
View(ModFieldemLNH4a)
ModFieldemLNH4_cld2 <- cld(ModFieldemLNH4a$diff, alpha = 0.05, Letters = letters) 
View(ModFieldemLNH4_cld2)
summary(ModFieldemLNH4_cld)
ModFieldemLNH4_pairs <- pairs(ModFieldemLNH4)
ModFieldemLNH4_pairs
write.csv(ModFieldemLNH4_cld, file="Field_LNH4.csv")

# Perform the Sheffe's test
LNH4contrasts <- glht(ModFieldLNH4c, linfct = as.list(mcp(Treatment = "Tukey")))
LNH4_sheffes <- summary(LNH4contrasts, test = adjusted(type = "single-step"))
print(LNH4_sheffes)
LNH4contrasts <- glht(ModFieldLNH4c, linfct = as.list(mcp(Treatment = "Tukey")))$test$pvalues
LNH4pval <- LNH4_sheffes$tTable[, "p value"]
LNH4F_cld <- cld(LNH4pval, alpha = 0.05, Letters = letters)
print(LNH4F_cld)



#####   PO4 LOAD - SNOWMELT   ########
FieldLPO4_Mean <- summary_by(LPO4~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldLPO4_Mean <- as.numeric(FieldLPO4_Mean$LPO4)
FieldLPO4_skew <- skewness(FieldLPO4_Mean,na.rm=TRUE)
FieldLPO4_kur <- kurtosis(FieldLPO4_Mean,na.rm=TRUE)
cat("Skewness:", FieldLPO4_skew, "\n") # 2.72015
cat("Kurtosis:", FieldLPO4_kur, "\n") # 7.623074
shapiro.test(Field$LPO4) # p=6.935e-07
hist(Field$LPO4) # heavy left skew
leveneTest(LPO4~factor(Treatment), data=Field)  # P=0.1006
# transform - can't seem to log transfrom data, possibly linked to missing values or extrenmely low values
shapiro.test(log(Field$LPO4)) # p=0.8156
hist(log(Field$LPO4)) #  right skew
leveneTest(log(LPO4)~Treatment, data=Field)  # P= 0.4527
shapiro.test(sqrt(Field$LPO4)) # p=0.008712
hist(sqrt(Field$LPO4)) #  left skew
leveneTest(sqrt(LPO4)~Treatment, data=Field)  # P= 0.08256
#ModFieldLPO41
ModFieldLPO4a <- aov(log(LPO4)~Treatment+Block, data=Field)
anova(ModFieldLPO4a)
summary(ModFieldLPO4a)
hist(resid(ModFieldLPO4a))
shapiro.test(resid(ModFieldLPO4a))  # p=0.9206
plot(fitted(ModFieldLPO4a),resid(ModFieldLPO4a),pch=16) 
qqnorm(resid(ModFieldLPO4a)) 
qqline(resid(ModFieldLPO4a))
ModFieldLPO41_tidy <- tidy(ModFieldLPO4a)
ModFieldLPO41sum_sq_reg <- ModFieldLPO41_tidy$sumsq[1] 
ModFieldLPO41sum_sq_resid <- ModFieldLPO41_tidy$sumsq[2]
ModFieldLPO41sum_sq_reg / (ModFieldLPO41sum_sq_reg + ModFieldLPO41sum_sq_resid) #0.2796
# lm model with weighted least squares
ModFieldLPO4var <- tapply(log(Field$LPO4), Field$Treatment, var, na.rm=TRUE) 
weightsFLPO4 <- 1 / ModFieldLPO4var 
weightsFLPO4_full <- rep(weightsFLPO4, each = length(Field$LPO4) / length(weightsFLPO4))
ModFieldLPO4b <- lm(LPO4 ~ Treatment + Block, data=Field, weights=weightsFLPO4_full) 
anova(ModFieldLPO4b)
hist(resid(ModFieldLPO4b))  
shapiro.test(resid(ModFieldLPO4b))  # p=0.01606
plot(fitted(ModFieldLPO4b),resid(ModFieldLPO4b),pch=16) #clustered to left
qqnorm(resid(ModFieldLPO4b)) # long tails, especially on right
qqline(resid(ModFieldLPO4b))
rsq(ModFieldLPO4b)  # 0.3706
# normal lm model
ModFieldLPO4c <- lm(log(LPO4) ~ Treatment + Block, data=Field) 
anova(ModFieldLPO4c)
hist(resid(ModFieldLPO4c))
shapiro.test(resid(ModFieldLPO4c))  # p= 0.9206
plot(fitted(ModFieldLPO4c),resid(ModFieldLPO4c),pch=16) 
qqnorm(resid(ModFieldLPO4c))
qqline(resid(ModFieldLPO4c))
rsq(ModFieldLPO4c)  # 0.4699
# weighted aov
LPO4_subset <- subset(Field, !is.na(LPO4))
FLPO4weights <-  1/resid(ModFieldLPO4a, subset=LPO4_subset)^2
ModFieldLPO4d <- aov(LPO4~Treatment+Block, data=LPO4_subset, weights= FLPO4weights)
anova(ModFieldLPO4d)
summary(ModFieldLPO4d)
hist(resid(ModFieldLPO4d))
shapiro.test(resid(ModFieldLPO4d))  # p=0.1769
plot(fitted(ModFieldLPO4d),resid(ModFieldLPO4d),pch=16) 
qqnorm(resid(ModFieldLPO4d))
qqline(resid(ModFieldLPO4d))
ModFieldLPO4c_tidy <- tidy(ModFieldLPO4d)
ModFieldLPO4csum_sq_reg <- ModFieldLPO4c_tidy$sumsq[1] 
ModFieldLPO4csum_sq_resid <- ModFieldLPO4c_tidy$sumsq[2]
ModFieldLPO4csum_sq_reg / (ModFieldLPO4csum_sq_reg + ModFieldLPO4csum_sq_resid) #0.0328
# one-way Kruskal-Wallis
LPO4_subset <- subset(Field, !is.na(LPO4))
ModFieldLPO4e = kruskal.test(LPO4 ~Treatment, data=LPO4_subset)
ModFPO4_Dunn <- dunn.test(LPO4_subset$LPO4, LPO4_subset$Treatment, method = "bonferroni")
print(ModFieldLPO4e)
print(ModFPO4_Dunn)
# AIC& BIC
FLPO4_modlist <- list(ModFieldLPO4a, ModFieldLPO4b, ModFieldLPO4c, ModFieldLPO4d)
AIC_values <- sapply(FLPO4_modlist, AIC)
BIC_values <- sapply(FLPO4_modlist, BIC)
FLPO4AB <- data.frame(Model=c("ModFieldLPO4a", "ModFieldLPO4b", "ModFieldLPO4c","ModFieldLPO4d"), AIC_values, BIC_values)
print(FLPO4AB)
#Model AIC_values BIC_values
#1 ModFieldLPO4a   74.77800   85.22322
#2 ModFieldLPO4b  -80.78077  -70.33555
#3 ModFieldLPO4c   74.77800   85.22322
#4 ModFieldLPO4d -102.88242  -92.43720

#emmeans 
ModFieldemLPO4 <- emmeans(ModFieldLPO4c,~Treatment)
ModFieldemLPO4_cld <- cld(ModFieldemLPO4, Letters = letters, type="response") 
ModFieldemLPO4_cld <- ModFieldemLPO4_cld %>% rename(emmean = "response")
View(ModFieldemLPO4_cld)
write.csv(ModFieldemLPO4_cld, file="Field_PO4Load.csv")

# Plotting Snowmelt NO3, PO4 and NH4 load
#create and combine data frames for the three emmeans functions
emNO3 <- as.data.frame(ModFieldemLNO3_cld)
emNH4 <- as.data.frame(ModFieldemLNH4_cld)
emPO4 <- as.data.frame(ModFieldemLPO4_cld)
em_labels <- list("EM1" = "NO3", "EM2" = "NH4", "EM3" = "PO4")
em_all <- bind_rows(list(EM1 = emNO3, EM2 = emNH4, EM3 = emPO4), .id = "EM") 
em_all$EM <- factor(em_all$EM, levels = names(em_labels), labels = unlist(em_labels))
View(em_all)
# define function to calculate position adjustment for secondary axis
ggplot(em_all, aes(x=Treatment, y=emmean, pattern=EM)) +
  geom_bar_pattern(stat = "identity", position = position_dodge2(padding=0.2), colour="black", fill="white",
                    pattern_density=0.005, pattern_spacing=0.01)+
  scale_pattern_manual(values = c("NO3" = "stripe", "NH4" = "crosshatch", "PO4" = "wave"))+
  facet_wrap(~ EM, scales = "free_y", ) +
  labs(x = "Treatment", y="Nutrient load in runoff (kg/ha)") +
  scale_x_discrete(labels = c("Control1", "Control2", "Biochar\n25kgP/ha", "Biochar 10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  theme_bw() +
  theme(legend.title = element_blank() , legend.key.size=unit(10,"mm"), legend.text=element_text(size=12),
        strip.text.x.top = element_text(size = 18, face = "bold"))+
  theme(plot.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 24, face="bold")) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
  ggsave("LargePlots_Snowmelt.jpg", width = 12, height = 8, dpi = 150)
  
  #geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +  
  
 


#####   RESIN PO4 - SNOWMELT   ########
FieldResPO4_Mean <- summary_by(ResinPO4~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldResPO4_Mean <- as.numeric(FieldResPO4_Mean$ResinPO4)
FieldResPO4_skew <- skewness(FieldResPO4_Mean,na.rm=TRUE)
FieldResPO4_kur <- kurtosis(FieldResPO4_Mean,na.rm=TRUE)
cat("Skewness:", FieldResPO4_skew, "\n") # 1.686603 
cat("Kurtosis:", FieldResPO4_kur, "\n") # 2.578513 
shapiro.test(Field$ResinPO4) # p=0.0003421
hist(Field$ResinPO4) #  left skew
leveneTest(ResinPO4~Treatment, data=Field)  # P=0.0674
#transform
shapiro.test(log(Field$ResinPO4))  #p=0.8348
hist(log(Field$ResinPO4)) 
leveneTest(log(ResinPO4)~Treatment, data=Field)  # p=0.6948
#ModFieldResP1
ModFieldResPO41 <- aov(log(ResinPO4)~Treatment+Block, data=Field)
anova(ModFieldResPO41)
summary(ModFieldResPO41)
hist(resid(ModFieldResPO41))
shapiro.test(resid(ModFieldResPO41))  # p=0.7298
plot(fitted(ModFieldResPO41),resid(ModFieldResPO41),pch=16) 
qqnorm(resid(ModFieldResPO41)) # medium left tail
qqline(resid(ModFieldResPO41))
ModFieldResPO41_tidy <- tidy(ModFieldResPO41)
ModFieldResPO41sum_sq_reg <- ModFieldResPO41_tidy$sumsq[1] 
ModFieldResPO41sum_sq_resid <- ModFieldResPO41_tidy$sumsq[2]
ModFieldResPO41sum_sq_reg / (ModFieldResPO41sum_sq_reg + ModFieldResPO41sum_sq_resid) #0.616
#emmeans 
ModFieldemResPO41 <- emmeans(ModFieldResPO41,~Treatment)
ModFieldemResPO41_cld <- cld(ModFieldemResPO41, Letters = letters, type="response") 
View(ModFieldemResPO41_cld)
write.csv(ModFieldemResPO41_cld, file="Field_ResinPO4.csv")





#####   RESIN NO3 - SNOWMELT   ########
FieldResinNO3_Mean <- summary_by(ResinNO3~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
FieldResinNO3_Mean <- as.numeric(FieldResinNO3_Mean$ResinNO3)
FieldResinNO3_skew <- skewness(FieldResinNO3_Mean,na.rm=TRUE)
FieldResinNO3_kur <- kurtosis(FieldResinNO3_Mean,na.rm=TRUE)
cat("Skewness:", FieldResinNO3_skew, "\n") # 1.817733
cat("Kurtosis:", FieldResinNO3_kur, "\n") # 3.409184 
shapiro.test(Field$ResinNO3) # p=0.0002299
hist(Field$ResinNO3) #  left skew
leveneTest(ResinNO3~Treatment, data=Field)  # P=0.1291
# Transform
shapiro.test(log(Field$ResinNO3))  #p=0.7096
hist(log(Field$ResinNO3))
leveneTest(log(ResinNO3)~Treatment, data=Field)  # p=0.4265
#ModFieldResNO31
ModFieldResNO31 <- aov(log(ResinNO3)~Treatment+Block, data=Field)
anova(ModFieldResNO31)
summary(ModFieldResNO31)
hist(resid(ModFieldResNO31))
shapiro.test(resid(ModFieldResNO31))  # p=0.9754
plot(fitted(ModFieldResNO31),resid(ModFieldResNO31),pch=16) 
qqnorm(resid(ModFieldResNO31))
qqline(resid(ModFieldResNO31))
ModFieldResNO3_tidy <- tidy(ModFieldResNO31)
ModFieldResNO3sum_sq_reg <- ModFieldResNO3_tidy$sumsq[1] 
ModFieldResNO3sum_sq_resid <- ModFieldResNO3_tidy$sumsq[2]
ModFieldResNO3sum_sq_reg / (ModFieldResNO3sum_sq_reg + ModFieldResNO3sum_sq_resid) #0.854
#emmeans 
ModFieldResNO3em <- emmeans(ModFieldResNO31,~Treatment)
ModFieldResNO3em_cld <- cld(ModFieldResNO3em, Letters = letters, type="response") 
View(ModFieldResNO3em_cld)
write.csv(ModFieldResNO3em_cld, file="Field_ResinNO3.csv")



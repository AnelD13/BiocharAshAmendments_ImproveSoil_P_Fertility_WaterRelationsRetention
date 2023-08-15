##### Loading data in to R ####
Rows<-read.csv("Rows.csv", fileEncoding="UTF-8-BOM")
View(Rows)
Rowsraw<-read.csv("Rowsraw.csv", fileEncoding="UTF-8-BOM")

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
library(broom)
library(multcomp)
library(multcompView)
library(emmeans)
library(lsmeans)
library(e1071)
library(rsq)
library(mum)
library(writexl)
library(glmmTMB)

##### Summary and ordering of data   ####
#Check for missing values in a specific field
missing <- colSums(is.na(Rows[,]))
missing <- colSums(is.na(Rowsraw[,]))
print(missing)

#Change columns in a dataframe: set order that treatments appear in any analysis and in figures
# set to factors/categorical values, str displays 
Rows$Treatment <- factor(Rows$Treatment,levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow",
                                                 "MBMACoarse", "MBMAFine", "Phosphorus"))
Rows$Block <- factor(Rows$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Rows$Biomass <- as.numeric(as.character(Rows$Biomass)) #only set if issues happen when running analysis
summary(Rows)
str(Rows) #displays the structure of the object
View(Rows) #view the object in a separate window (e.g. as a table)
RowsMean <- summary_by(.~Treatment, data=Rows, FUN=mean, na.rm=TRUE)
View(RowsMean)

#####   Check for outliers   ####
## Biomass
ggplot(Rowsraw, aes(x = Treatment, y = Biomass, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Biomass")
ggsave("OutliersRows_Biomass.jpg", width = 10, height = 10, dpi = 150)
## Nuptake
ggplot(Rowsraw, aes(x = Treatment, y = Nuptake, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "N uptake")
ggsave("OutliersRows_Nuptake.jpg", width = 10, height = 10, dpi = 150)
## N recovery
ggplot(Rowsraw, aes(x = Treatment, y = Nrecovery, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "N recovery")
ggsave("OutliersRows_Nrecovery.jpg", width = 10, height = 10, dpi = 150)
## P uptake
ggplot(Rowsraw, aes(x = Treatment, y = Puptake, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "P uptake")
ggsave("OutliersRows_Puptake.jpg", width = 10, height = 10, dpi = 150)
## P recovery
ggplot(Rowsraw, aes(x = Treatment, y = Precovery, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "P recovery")
ggsave("OutliersRows_Precovery.jpg", width = 10, height = 10, dpi = 150)
## NO3
ggplot(Rowsraw, aes(x = Treatment, y = NO3, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "NO3")
ggsave("OutliersRows_NO3.jpg", width = 10, height = 10, dpi = 150)
## PO4
ggplot(Rowsraw, aes(x = Treatment, y = PO4, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "PO4")
ggsave("OutliersRows_PO4.jpg", width = 10, height = 10, dpi = 150)
## WatSolP
ggplot(Rowsraw, aes(x = Treatment, y = WatSolP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "Water Soluble P")
ggsave("OutliersRows_WatSolP.jpg", width = 10, height = 10, dpi = 150)
## ResinP
ggplot(Rowsraw, aes(x = Treatment, y = ResinP, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "ResinP")
ggsave("OutliersRows_ResinP.jpg", width = 10, height = 10, dpi = 150)
## pH
ggplot(Rowsraw, aes(x = Treatment, y = pH, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "pH")
ggsave("OutliersRows_pH.jpg", width = 10, height = 10, dpi = 150)
## EC
ggplot(Rowsraw, aes(x = Treatment, y = EC, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "EC")
ggsave("OutliersRows_EC.jpg", width = 10, height = 10, dpi = 150)
## %OC
ggplot(Rowsraw, aes(x = Treatment, y = OC, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(x = "Treatment", y = "OC")
ggsave("OutliersRows_OC.jpg", width = 10, height = 10, dpi = 150)




# Summary data (means, SD, etc.) for each treatment and variable
RowsMean <- summary_by(.~Treatment, data=Rows, FUN=mean, na.rm=TRUE) #observed means for the data set
RowsSD <- summary_by(.~Treatment, data=Rows, FUN=sd) #overall SD for the dataset
View(RowsMean)
View(RowsSD)




####  BIOMASS   ######
# Skewness & kurtosis on biomass
Bio_Mean <- summary_by(Biomass~Treatment+Block, data=Rows, FUN=mean) 
Bio_Mean <- as.numeric(Bio_Mean$Biomass)
Bio_skew <- skewness(Bio_Mean,na.rm=TRUE)
Bio_kur <- kurtosis(Bio_Mean,na.rm=TRUE)
cat("Skewness:", Bio_skew, "\n") ## data is not very skewed: -0.21
cat("Kurtosis:", Bio_kur, "\n") ## data has low kurtosis: -0.61
shapiro.test(Rows$Biomass) # p=0.5637
hist(Rows$Biomass) #  pretty normal
leveneTest(Biomass~Treatment, data=Rows)  # data has equal variance: 0.6632
#ModRBio1 ANOVA - perfectly predicts response variable (no variation in residuals) - not a good fit
ModRBio1 <- aov(Biomass~Treatment+Block, data=Rows)
anova(ModRBio1)
summary(ModRBio1)
hist(resid(ModRBio1))
shapiro.test(resid(ModRBio1))  # p=0.1615
plot(fitted(ModRBio1),resid(ModRBio1),pch=16)
qqnorm(resid(ModRBio1))
qqline(resid(ModRBio1))
ModRBio1_tidy <- tidy(ModRBio1)
ModRBio1sum_sq_reg <- ModRBio1_tidy$sumsq[1] #use summary stats to determine the sum squares regression
ModRBio1sum_sq_resid <- ModRBio1_tidy$sumsq[2]  # use the summary stats to determine the sum squares residuals
ModRBio1sum_sq_reg / (ModRBio1sum_sq_reg + ModRBio1sum_sq_resid) #calculate the R squared value: 0.753
#ModRBio2 (lm) simple linear model - same issue as ANOVA
ModRBio2<-lm(Biomass~Treatment+Block,data=Rows)
hist(resid(ModRBio2))
shapiro.test(resid(ModRBio2)) # p=0.1615
summary(ModRBio2)$adj.r.squared # -0.2366
# ModRBio3 glmm - convergence issues
ModRBio3<- glmmTMB(Biomass~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude,
                  control=glmmTMBControl(optimizer=optim, optArgs=list(parallel = TRUE, nthreads = 100)))
glmmTMB:::Anova.glmmTMB(ModRBio3, type="III")
summary(ModRbio3)
performance::r2(ModRbio3) # 0.942
shapiro.test(resid(ModRbio3)) # p=0.5151
plot(fitted(ModRbio3),resid(ModRbio3),pch=16) # 2 clusters left and right, slight upper shift
qqnorm(resid(ModRbio3)) # moderate right tail
qqline(resid(ModRbio3))
#ModRBio4
ModRBio4 <- lmer(Biomass~Treatment+(1|Block), data=Rows, na.action=na.exclude) # singularity issue
rsq(ModRBio4)  # 0.0938
anova(ModRBio4) 
summary(ModRBio4)
shapiro.test(resid(ModRBio4)) # p=0.1226
plot(fitted(ModRBio4),resid(ModRBio4),pch=16) # slightly staggered to right
qqnorm(resid(ModRBio4)) # moderate left tail
qqline(resid(ModRBio4))
# ModRBio5 lme model
ModRBio5 <- lme(Biomass ~ Treatment, random=~1|Block, data=Rows)
summary(ModRBio5)
anova(ModRBio5)
shapiro.test(resid(ModRBio5)) # p= 0.1226
plot(fitted(ModRBio5),resid(ModRBio5),pch=16) # clustered to left, equal around 0
qqnorm(resid(ModRBio5)) # moderate tails
qqline(resid(ModRBio5))
rsq(ModRBio5) # 0.0938
#ModRBio6 glmer
ModRBio6 <- glmer(Biomass~Treatment+(1|Block),data=Rows,family=Gamma(link="log"))
anova(ModRBio6)
summary(ModRBio6)
shapiro.test(resid(ModRBio6)) # p=0.2459
bf.test(Biomass~Treatment, data=Rows) # p=0.917, variances equal
rsq(ModRBio6) # r=0.088
plot(fitted(ModRBio6),resid(ModRBio6),pch=16) # slight right cluster
qqnorm(resid(ModRBio6)) # heavy left tail, small right tail
qqline(resid(ModRBio6))

#AIC and BIC values
RowsBio_modlist <- list(ModRBio1, ModRBio2, ModRBio3, ModRBio4, ModRBio5, ModRBio6)
AIC_values <- sapply(RowsBio_modlist, AIC)
BIC_values <- sapply(RowsBio_modlist, BIC)
PrecRowsBio <- data.frame(Model=c("ModRBio1", "ModRBio2", "ModRBio3", "ModRBio4", "ModRBio5", "ModRBio6"),
                          AIC_values, BIC_values)
print(PrecRowsBio)
#Model AIC_values BIC_values
#1 ModRBio1   422.9009   440.4897
##2 ModRBio2   422.9009   440.4897
#3 ModRbio3         NA         NA
#4 ModRBio4   339.0548   353.7122
#5 ModRBio5   339.0548   350.8354
#6 ModRBio6   425.8654   440.5227

#emmeans on ModRBio5 - no issues running emmeans or model
ModRemBio <- emmeans(ModRBio5,~Treatment, type="response")
ModRemBio_cld <- cld(ModRemBio, Letters=trimws(letters), reversed = TRUE) 
View(ModRemBio_cld)
write.csv(ModRemBio_cld, file="Rows_Biomass.csv")

# Plotting the summary data
(RowsBioPlot <- ggplot(ModRemBio_cld, aes(x=Treatment, y=emmean)) +
    geom_bar_pattern(stat="identity", width=0.5, position=position_dodge2(padding=0.5), colour="black", fill="white", 
                     pattern_density=0.05, pattern_spacing=0.01)+
    geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE), size=9, vjust=-0.7)+
    labs(x = "Treatment", y = "Canola biomass (g)") +
    scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", 
                                "Meat and Bone\nMeal - Coarse", "Meat and Bone\nMeal - Fine", 
                                "Fertilizer\nPhosphorus"))+
    scale_y_continuous(limits = c(0, 750))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(RowsBioPlot, file="Rows_biomass.jpg", width = 8, height = 8, dpi = 150)



#####   N UPTAKE   ##########
RowNup_Mean <- summary_by(Nuptake~Treatment+Block, data=Rows, FUN=mean) 
RowNup_Mean <- as.numeric(RowNup_Mean$Nuptake)
RowNup_skew <- skewness(RowNup_Mean,na.rm=TRUE)
RowNup_kur <- kurtosis(RowNup_Mean,na.rm=TRUE)
cat("Skewness:", RowNup_skew, "\n") ## data is not very skewed: -0.1011
cat("Kurtosis:", RowNup_kur, "\n") ## data has low kurtosis: -0.4737
shapiro.test(Rows$Nuptake) # p=0.8318
hist(Rows$Nuptake) #  pretty normal
leveneTest(Nuptake~Treatment, data=Rows)  # data has equal variance: 0.321
#ModRNup1
ModRNup1 <- aov(Nuptake~Treatment+Block, data=Rows)
anova(ModRNup1)
summary(ModRNup1)
hist(resid(ModRNup1))
shapiro.test(resid(ModRNup1))  # p=0.121
plot(fitted(ModRNup1),resid(ModRNup1),pch=16) #slightly rights skewed but very random
qqnorm(resid(ModRNup1))
qqline(resid(ModRNup1))
ModRNup1_tidy <- tidy(ModRNup1)
ModRNup1sum_sq_reg <- ModRNup1_tidy$sumsq[1] 
ModRNup1sum_sq_resid <- ModRNup1_tidy$sumsq[2]
ModRNup1sum_sq_reg / (ModRNup1sum_sq_reg + ModRNup1sum_sq_resid) #calculate the R squared value: 0.58
# ModRNup2 glmm
ModRNup2<- glmmTMB(Nuptake~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRNup2, type="III")
summary(ModRNup2)
performance::r2(ModRNup2) # 0.872
shapiro.test(resid(ModRNup2)) # p=0.1033
plot(fitted(ModRNup2),resid(ModRNup2),pch=16) # skewed towards the right, little bit heavier below the line
qqnorm(resid(ModRNup2)) # moderate tails
qqline(resid(ModRNup2))
#ModRNup3 lmer
ModRNup3 <- lmer(Nuptake~Treatment+(1|Block), data=Rows, na.action=na.exclude) # singularity issue
anova(ModRNup3) 
summary(ModRNup3)
shapiro.test(resid(ModRNup3)) # p=0.5875
plot(fitted(ModRNup3),resid(ModRNup3),pch=16) # slightly staggered to right
qqnorm(resid(ModRNup3)) # almost no tails
qqline(resid(ModRNup3))
rsq(ModRNup3)  # 0.146
# ModRNup4 lme model
ModRNup4 <- lme(Nuptake ~ Treatment, random=~1|Block, data=Rows, na.action=na.exclude)
summary(ModRNup4)
anova(ModRNup4)
shapiro.test(resid(ModRNup4)) # p= 0.1226
plot(fitted(ModRNup4),resid(ModRNup4),pch=16) # clustered to right, equal around 0
qqnorm(resid(ModRNup4)) # almost no tails
qqline(resid(ModRNup4))
r.squaredGLMM(ModRNup4)
#ModRNup5 glmer - results in infinite df values in emmeans
ModRNup5 <- glmer(Nuptake~Treatment+(1|Block),data=Rows,family=Gamma(link="log"))
anova(ModRNup5)
summary(ModRNup5)
shapiro.test(resid(ModRNup5)) # p=0.782
bf.test(Nuptake~Treatment, data=Rows) # p=0.759, variances equal
performance::r2(ModRNup5) # 0.22
plot(fitted(ModRNup5),resid(ModRNup5),pch=16) # slight right cluster
qqnorm(resid(ModRNup5)) # light tails
qqline(resid(ModRNup5))


#AIC and BIC values
RowsNup_modlist <- list(ModRNup2, ModRNup3, ModRNup4, ModRNup5)
AIC_values <- sapply(RowsNup_modlist, AIC)
BIC_values <- sapply(RowsNup_modlist, BIC)
PrecRowsNup <- data.frame(Model=c("ModRNup2", "ModRNup3", "ModRNup4", "ModRNup5"), AIC_values, BIC_values)
print(PrecRowsNup)
#Model AIC_values BIC_values
#1 ModRNup2   203.4601   217.8000
#2 ModRNup3   173.7836   188.1235
#3 ModRNup4   173.7836   185.1385
#4 ModRNup5   207.4792   221.8191

#emmeans on glmm model - no convergence or singularity issues, works fine in emmeans, df looks good
ModRemNup <- emmeans(ModRNup2,~Treatment, type="response")
ModRemNup_cld <- cld(ModRemNup, Letters=trimws(letters), reversed = TRUE) 
View(ModRemNup_cld)
write.csv(ModRemNup_cld, file="Rows_Nuptake.csv")

# Plotting the summary data
(RowsNupPlot <- ggplot(ModRemNup_cld, aes(x=Treatment, y=emmean)) +
  geom_bar_pattern(stat = "identity", width=0.5, position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=trimws(.group), y=emmean+SE+1), size=9)+
  labs(x = "Treatment", y = "N uptake (kg N/ha)") +
  scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", 
                              "Meat and Bone\nMeal - Coarse", "Meat and Bone\nMeal - Fine", 
                              "Fertilizer\nPhosphorus"))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(RowsNupPlot, file="Rows_Nuptake.jpg", width = 8, height = 8, dpi = 150)


#####   N RECOVERY   ##########
RowNrec_Mean <- summary_by(Nrecovery~Treatment+Block, data=Rows, FUN=mean) 
RowNrec_Mean <- as.numeric(RowNrec_Mean$Nrecovery)
RowNrec_skew <- skewness(RowNrec_Mean,na.rm=TRUE)
RowNrec_kur <- kurtosis(RowNrec_Mean,na.rm=TRUE)
cat("Skewness:", RowNrec_skew, "\n") ## 0.46977
cat("Kurtosis:", RowNrec_kur, "\n") ##  0.28355
shapiro.test(Rows$Nrecovery) # p=0.5476
hist(Rows$Nrecovery) #  pretty normal
leveneTest(Nrecovery~Treatment, data=Rows)  # p=0.02449 
#ModRNrec1
ModRNrec1 <- aov(Nrecovery~Treatment+Block, data=Rows)
anova(ModRNrec1)
summary(ModRNrec1)
hist(resid(ModRNrec1))
shapiro.test(resid(ModRNrec1))  # p=0.8853
plot(fitted(ModRNrec1),resid(ModRNrec1),pch=16) # random
qqnorm(resid(ModRNrec1))
qqline(resid(ModRNrec1))
ModRNrec1_tidy <- tidy(ModRNrec1)
ModRNrec1sum_sq_reg <- ModRNrec1_tidy$sumsq[1] 
ModRNrec1sum_sq_resid <- ModRNrec1_tidy$sumsq[2]
ModRNrec1sum_sq_reg / (ModRNrec1sum_sq_reg + ModRNrec1sum_sq_resid) #calculate the R squared value: 0.907
# ModRNrec2 glmm
ModRNrec2<- glmmTMB(Nrecovery~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRNrec2, type="III")
summary(ModRNrec2)
performance::r2(ModRNrec2) # Na - may have singularity issues
shapiro.test(resid(ModRNrec2)) # p=0.8896
plot(fitted(ModRNrec2),resid(ModRNrec2),pch=16) # normal
qqnorm(resid(ModRNrec2)) # almostt no tails
qqline(resid(ModRNrec2))
#ModRNrec3 lmer
ModRNrec3 <- lmer(Nrecovery~Treatment+(1|Block), data=Rows, na.action=na.exclude) # singularity issue
anova(ModRNrec3) 
summary(ModRNrec3)
shapiro.test(resid(ModRNrec3)) # p=0.8896
plot(fitted(ModRNrec3),resid(ModRNrec3),pch=16) # normal
qqnorm(resid(ModRNrec3)) # slight tails
qqline(resid(ModRNrec3))
rsq(ModRNrec3)  # 0.2529
# ModRNrec4 lme model
ModRNrec4 <- lme(Nrecovery ~ Treatment, random=~1|Block, data=Rows, na.action=na.exclude)
summary(ModRNrec4)
anova(ModRNrec4)
shapiro.test(resid(ModRNrec4)) # p= 0.8896
plot(fitted(ModRNrec4),resid(ModRNrec4),pch=16) # normal
qqnorm(resid(ModRNrec4)) # slight tails
qqline(resid(ModRNrec4))
rsq(ModRNrec4)  ## issues with function

#AIC and BIC values
RowsNrec_modlist <- list(ModRNrec2, ModRNrec3, ModRNrec4)
AIC_values <- sapply(RowsNrec_modlist, AIC)
BIC_values <- sapply(RowsNrec_modlist, BIC)
PrecRowsNRec <- data.frame(Model=c("ModRNrec2", "ModRNrec3", "ModRNrec4"),AIC_values, BIC_values)
print(PrecRowsNRec)
#      Model AIC_values BIC_values
#1 ModRNrec2   150.6296   161.9525
#2 ModRNrec3   130.0098   141.3327
#3 ModRNrec4   130.0098   138.5098

#emmeans on glmm model - df looks decent (17 vs 19 or 3)
ModRemNrec <- emmeans(ModRNrec4,~Treatment, type="response")
ModRemNrec_cld <- cld(ModRemNrec, Letters=trimws(letters), reversed = TRUE) 
View(ModRemNrec_cld)
write.csv(ModRemNrec_cld, file="Rows_Nrecovery.csv")


# Plotting the summary data
(RowsNrecPlot <- ggplot(ModRemNrec_cld, aes(x=Treatment, y=emmean)) +
    geom_bar_pattern(stat = "identity", width=0.5, position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE+0.5), size=9)+
    scale_y_continuous(limits=c(-1.5, 8.5))+
    labs(x = "Treatment", y = "% Nitrogen recovery") +
    scale_x_discrete(labels = c("Control2", "Canola\nMeal", "Manure", "Willow", "Meat and Bone\nMeal - Coarse",
                                "Meat and Bone\nMeal - Fine", "Fertilizer\nPhosphorus"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(RowsNrecPlot, file="Rows_Nrecovery.jpg", width = 8, height = 8, dpi = 150)



#####   P UPTAKE   ##########
RowPup_Mean <- summary_by(Puptake~Treatment+Block, data=Rows, FUN=mean) 
RowPup_Mean <- as.numeric(RowPup_Mean$Puptake)
RowPup_skew <- skewness(RowPup_Mean,na.rm=TRUE)
RowPup_kur <- kurtosis(RowPup_Mean,na.rm=TRUE)
cat("Skewness:", RowPup_skew, "\n") ## 0.2244
cat("Kurtosis:", RowPup_kur, "\n") ## -0.4404
shapiro.test(Rows$Puptake) # p=0.7449
hist(Rows$Puptake) #  pretty normal
leveneTest(Puptake~Treatment, data=Rows)  # data has equal variance: 0.2183
#ModRPup1
ModRPup1 <- aov(Puptake~Treatment+Block, data=Rows)
anova(ModRPup1)
summary(ModRPup1)
hist(resid(ModRPup1))
shapiro.test(resid(ModRPup1))  # p=0.04649
plot(fitted(ModRPup1),resid(ModRPup1),pch=16) #slightly rights skewed but very random
qqnorm(resid(ModRPup1))
qqline(resid(ModRPup1))
ModRPup1_tidy <- tidy(ModRPup1)
ModRPup1sum_sq_reg <- ModRPup1_tidy$sumsq[1] 
ModRPup1sum_sq_resid <- ModRPup1_tidy$sumsq[2]
ModRPup1sum_sq_reg / (ModRPup1sum_sq_reg + ModRPup1sum_sq_resid) #calculate the R squared value: 0.35
# ModRPup2 glmm
ModRPup2<- glmmTMB(Puptake~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRPup2, type="III")
summary(ModRPup2)
performance::r2(ModRPup2) # 0.251
shapiro.test(resid(ModRPup2)) # p=0.0465
plot(fitted(ModRPup2),resid(ModRPup2),pch=16) # normal
qqnorm(resid(ModRPup2)) # slight tails
qqline(resid(ModRPup2))
#ModRPup3 lmer
ModRPup3 <- lmer(Puptake~Treatment+(1|Block), data=Rows, na.action=na.exclude) 
anova(ModRPup3) 
summary(ModRPup3)
shapiro.test(resid(ModRPup3)) # p=0.0469
plot(fitted(ModRPup3),resid(ModRPup3),pch=16) # normal
qqnorm(resid(ModRPup3)) # slight tails
qqline(resid(ModRPup3))
rsq(ModRPup3)  # 0.205
# ModRPup4 lme model
ModRPup4 <- lme(Puptake ~ Treatment, random=~1|Block, data=Rows, na.action=na.exclude)
summary(ModRPup4)
anova(ModRPup4)
shapiro.test(resid(ModRPup4)) # p= 0.0469
plot(fitted(ModRPup4),resid(ModRPup4),pch=16) # normal
qqnorm(resid(ModRPup4)) # slight tails
qqline(resid(ModRPup4))
rsq(ModRPup4)  ## issues with function

#AIC and BIC values
RowsPup_modlist <- list(ModRPup2, ModRPup3, ModRPup4)
AIC_values <- sapply(RowsPup_modlist, AIC)
BIC_values <- sapply(RowsPup_modlist, BIC)
PrecRowsPup <- data.frame(Model=c("ModRPup2", "ModRPup3", "ModRPup4"),AIC_values, BIC_values)
print(PrecRowsPup)
#Model AIC_values BIC_values
#1 ModRPup2   46.99998   61.33986
#2 ModRPup3   57.70818   72.04806
#3 ModRPup4   57.70818   69.06313

#emmeans on glmm model- best rsq, lowest AIC
ModRemPup <- emmeans(ModRPup2,~Treatment, type="response")
ModRemPup_cld <- cld(ModRemPup, Letters=trimws(letters), reversed = TRUE) 
View(ModRemPup_cld)
write.csv(ModRemPup_cld, file="Rows_Puptake.csv")

# Plotting the summary data
(RowsPupPlot <- ggplot(ModRemPup_cld, aes(x=Treatment, y=emmean)) +
  geom_bar_pattern(stat = "identity", width=0.5, position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label=trimws(.group), y=emmean+SE), size=9, position = position_dodge2(width = 0.9), vjust=-1)+
  scale_y_continuous(limits=c(0, 2))+
  labs(x = "Treatment", y = "P uptake (kg N/ha)") +
  scale_x_discrete(labels = c("Control1", "Control2", "Canola\nMeal", "Manure", "Willow", 
                              "Meat and Bone\nMeal - Coarse", "Meat and Bone\nMeal - Fine", 
                              "Fertilizer\nPhosphorus"))+
  theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
        legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
        axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
        axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background = element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(RowsPupPlot, file="Rows_Puptake.jpg", width = 8, height = 8, dpi = 150)




#####   P RECOVERY   ##########
RowPrec_Mean <- summary_by(Precovery~Treatment+Block, data=Rows, FUN=mean) 
RowPrec_Mean <- as.numeric(RowPrec_Mean$Precovery)
RowPrec_skew <- skewness(RowPrec_Mean,na.rm=TRUE)
RowPrec_kur <- kurtosis(RowPrec_Mean,na.rm=TRUE)
cat("Skewness:", RowPrec_skew, "\n") ## -0.003673714
cat("Kurtosis:", RowPrec_kur, "\n") ##  -0.1997805 
shapiro.test(Rows$Precovery) # p=0.9829
hist(Rows$Precovery) #  pretty normal
leveneTest(Precovery~Treatment, data=Rows)  # p=0.02322 
# ModRPrec1
ModRPrec1 <- aov(Precovery~Treatment+Block, data=Rows)
anova(ModRPrec1)
summary(ModRPrec1)
hist(resid(ModRPrec1))
shapiro.test(resid(ModRPrec1))  # p= 0.2238
plot(fitted(ModRPrec1),resid(ModRPrec1),pch=16) # random
qqnorm(resid(ModRPrec1)) #slight tails
qqline(resid(ModRPrec1))
ModRPrec1_tidy <- tidy(ModRPrec1)
ModRPrec1sum_sq_reg <- ModRPrec1_tidy$sumsq[1] 
ModRPrec1sum_sq_resid <- ModRPrec1_tidy$sumsq[2]
ModRPrec1sum_sq_reg / (ModRPrec1sum_sq_reg + ModRPrec1sum_sq_resid) #calculate the R squared value: 0.266
# ModRPrec2 glmm
ModRPrec2<- glmmTMB(Precovery~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRPrec2, type="III")
summary(ModRPrec2)
shapiro.test(resid(ModRPrec2)) # p=0.5276
plot(fitted(ModRPrec2),resid(ModRPrec2),pch=16) # normal
qqnorm(resid(ModRPrec2)) # slight tails
qqline(resid(ModRPrec2))
performance::r2(ModRPrec2) # 0.267
#ModRPrec3 lmer
ModRPrec3 <- lmer(Precovery~Treatment+(1|Block), data=Rows, na.action=na.exclude) 
anova(ModRPrec3) 
summary(ModRPrec3)
shapiro.test(resid(ModRPrec3)) # p=0.5304
plot(fitted(ModRPrec3),resid(ModRPrec3),pch=16) # normal
qqnorm(resid(ModRPrec3)) # slight tails
qqline(resid(ModRPrec3))
rsq(ModRPrec3)  # 0.214
# ModRPrec4 lme model
ModRPrec4 <- lme(Precovery ~ Treatment, random=~1|Block, data=Rows, na.action=na.exclude)
anova(ModRPrec4)
summary(ModRPrec4)
shapiro.test(resid(ModRPrec4)) # p= 0.5304
plot(fitted(ModRPrec4),resid(ModRPrec4),pch=16) # normal
qqnorm(resid(ModRPrec4)) # slight tails
qqline(resid(ModRPrec4))
rsq(ModRPrec4)  ## issues with function

#AIC and BIC values
RowsPrec_modlist <- list(ModRPrec2, ModRPrec3, ModRPrec4)
AIC_values <- sapply(RowsPrec_modlist, AIC)
BIC_values <- sapply(RowsPrec_modlist, BIC)
PrecRowsPrec <- data.frame(Model=c("ModRPrec2", "ModRPrec3", "ModRPrec4"),AIC_values, BIC_values)
print(PrecRowsPrec)
#Model AIC_values BIC_values
#1 ModRPrec2   96.21132  105.29527
#2 ModRPrec3   88.46554   97.54950
#3 ModRPrec4   88.46554   95.13125

#emmeans on glmm model - df looks fine, highest rsq
ModRemPrec <- emmeans(ModRPrec2,~Treatment, type="response")
ModRemPrec_cld <- cld(ModRemPrec, Letters=trimws(letters), reversed = TRUE) 
View(ModRemPrec_cld)
write.csv(ModRemPrec_cld, file="Rows_Precovery.csv")

# Plotting the summary data
(RowsPrecPlot <- ggplot(ModRemPrec_cld, aes(x=Treatment, y=emmean)) +
    geom_bar_pattern(stat = "identity", width=0.5, position = position_dodge2(padding=0.2), colour="black", fill="white", 
                   pattern_density=0.05, pattern_spacing=0.01)+
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2, position = position_dodge(width = 0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE+0.3), size=9,)+
    scale_y_continuous(limits=c(-2,3))+
    labs(x = "Treatment", y = "% Phosphorus recovery") +
    scale_x_discrete(labels = c("Canola\nMeal", "Manure", "Willow", "Meat and Bone\nMeal - Coarse",
                                "Meat and Bone\nMeal - Fine", "Fertilizer\nPhosphorus"))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title = element_text(size = 20, face = "bold"), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 18, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
ggsave(RowsPrecPlot, file="Rows_Precovery.jpg", width = 8, height = 8, dpi = 150)



#####   NO3   ##########
RowNO3_Mean <- summary_by(NO3~Treatment+Block, data=Rows, FUN=mean) 
RowNO3_Mean <- as.numeric(RowNO3_Mean$NO3)
RowNO3_skew <- skewness(RowNO3_Mean,na.rm=TRUE)
RowNO3_kur <- kurtosis(RowNO3_Mean,na.rm=TRUE)
cat("Skewness:", RowNO3_skew, "\n") ## 0.3278409 
cat("Kurtosis:", RowNO3_kur, "\n") ## -1.129021 
shapiro.test(Rows$NO3) # p=0.07604
hist(Rows$NO3) #  not quite normal - it almost flatlines
leveneTest(NO3~Treatment, data=Rows)  # P=0.3681
#ModRNO31
ModRNO31 <- aov(NO3~Treatment+Block, data=Rows)
anova(ModRNO31)
summary(ModRNO31)
hist(resid(ModRNO31))
shapiro.test(resid(ModRNO31))  # p=0.7973
plot(fitted(ModRNO31),resid(ModRNO31),pch=16) 
qqnorm(resid(ModRNO31))
qqline(resid(ModRNO31))
ModRNO31_tidy <- tidy(ModRNO31)
ModRNO31sum_sq_reg <- ModRNO31_tidy$sumsq[1] 
ModRNO31sum_sq_resid <- ModRNO31_tidy$sumsq[2]
ModRNO31sum_sq_reg / (ModRNO31sum_sq_reg + ModRNO31sum_sq_resid) #0.76
# ModRNO3 glmm
ModRNO32<- glmmTMB(NO3~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRNO32, type="III")
summary(ModRNO32)
performance::r2(ModRNO32) # 0.625
shapiro.test(resid(ModRNO32)) # p=0.6206
plot(fitted(ModRNO32),resid(ModRNO32),pch=16) # normal
qqnorm(resid(ModRNO32)) # almost no tails
qqline(resid(ModRNO32))

#emmeans on aov model
ModRemNO3 <- emmeans(ModRNO32,~Treatment, type="response")
ModRemNO3_cld <- cld(ModRemNO3, Letters=trimws(letters), reversed = TRUE) 
View(ModRemNO3_cld)
write.csv(ModRemNO3_cld, file="Rows_NO3.csv")



#####   PO4   ##########
RowPO4_Mean <- summary_by(PO4~Treatment+Block, data=Rows, FUN=mean) 
RowPO4_Mean <- as.numeric(RowPO4_Mean$PO4)
RowPO4_skew <- skewness(RowPO4_Mean,na.rm=TRUE)
RowPO4_kur <- kurtosis(RowPO4_Mean,na.rm=TRUE)
cat("Skewness:", RowPO4_skew, "\n") ## 0.5379324 
cat("Kurtosis:", RowPO4_kur, "\n") ## 0.7483132
shapiro.test(Rows$PO4) # p=0.3863
hist(Rows$PO4) #  left skew
leveneTest(PO4~Treatment, data=Rows)  # P=0.7706
#ModRPO41
ModRPO41 <- aov(PO4~Treatment+Block, data=Rows)
anova(ModRPO41)
summary(ModRPO41)
hist(resid(ModRPO41))
shapiro.test(resid(ModRPO41))  # p=0.6394
plot(fitted(ModRPO41),resid(ModRPO41),pch=16) 
qqnorm(resid(ModRPO41))
qqline(resid(ModRPO41))
ModRPO41_tidy <- tidy(ModRPO41)
ModRPO41sum_sq_reg <- ModRPO41_tidy$sumsq[1] 
ModRPO41sum_sq_resid <- ModRPO41_tidy$sumsq[2]
ModRPO41sum_sq_reg / (ModRPO41sum_sq_reg + ModRPO41sum_sq_resid) #0.5651
# ModRPO42 glmm - possible singularity issue related to random effect
ModRPO42<- glmmTMB(PO4~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRPO42, type="III")
summary(ModRPO42)
performance::r2(ModRPO42) # 0.625
shapiro.test(resid(ModRPO42)) # p=0.2893
plot(fitted(ModRPO42),resid(ModRPO42),pch=16) # slightly below the center line
qqnorm(resid(ModRPO42)) # moderate-heavy right tail
qqline(resid(ModRPO42))

#emmeans on glmm model
ModRemPO4 <- emmeans(ModRPO42,~Treatment, type="response")
ModRemPO4_cld <- cld(ModRemPO4, Letters=trimws(letters), reversed = TRUE) 
View(ModRemPO4_cld)
write.csv(ModRemPO4_cld, file="Rows_PO4.csv")



#####   RESIN P   ##########
RowResP_Mean <- summary_by(ResinP~Treatment+Block, data=Rows, FUN=mean) 
RowResP_Mean <- as.numeric(RowResP_Mean$ResinP)
RowResP_skew <- skewness(RowResP_Mean,na.rm=TRUE)
RowResP_kur <- kurtosis(RowResP_Mean,na.rm=TRUE)
cat("Skewness:", RowResP_skew, "\n") ## 1.404125 
cat("Kurtosis:", RowResP_kur, "\n") ## 2.710147  
shapiro.test(Rows$ResinP) # p=0.001659
hist(Rows$ResinP) #  left skew
leveneTest(ResinP~Treatment, data=Rows)  # P=0.0674
shapiro.test(log(Rows$ResinP))  #p=0.2711
leveneTest(log(ResinP)~Treatment, data=Rows)  # p=0.0627
#ModResP1
ModResP1 <- aov(log(ResinP)~Treatment+Block, data=Rows)
anova(ModResP1)
summary(ModResP1)
hist(resid(ModResP1))
shapiro.test(resid(ModResP1))  # p=0.4569
plot(fitted(ModResP1),resid(ModResP1),pch=16) 
qqnorm(resid(ModResP1))
qqline(resid(ModResP1))
ModResP1_tidy <- tidy(ModResP1)
ModResP1sum_sq_reg <- ModResP1_tidy$sumsq[1] 
ModResP1sum_sq_resid <- ModResP1_tidy$sumsq[2]
ModResP1sum_sq_reg / (ModResP1sum_sq_reg + ModResP1sum_sq_resid) #0.539
# ModResP2 glmm 
ModResP2<- glmmTMB(ResinP~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModResP2, type="III")
summary(ModResP2)
performance::r2(ModResP2) # 0.245
shapiro.test(resid(ModResP2)) # p=0.261
plot(fitted(ModResP2),resid(ModResP2),pch=16) # slightly below the center line
qqnorm(resid(ModResP2)) # moderate-heavy right tail, small left tail
qqline(resid(ModResP2))

#emmeans on aov model
ModREMResP1 <- emmeans(ModResP2,~Treatment, type="response")
ModREMResP1_cld <- cld(ModRemNup, Letters=trimws(letters), reversed = TRUE) 
View(ModREMResP1_cld)
write.csv(ModREMResP1_cld, file="Rows_resinP.csv")




#####   WATER SOLUBLE P   ##########
RowWSP_Mean <- summary_by(WatSolP~Treatment+Block, data=Rows, FUN=mean) 
RowWSP_Mean <- as.numeric(RowWSP_Mean$WatSolP)
RowWSP_skew <- skewness(RowWSP_Mean,na.rm=TRUE)
RowWSP_kur <- kurtosis(RowWSP_Mean,na.rm=TRUE)
cat("Skewness:", RowWSP_skew, "\n") ## 0.775885 
cat("Kurtosis:", RowWSP_kur, "\n") ## 0.1735754  
shapiro.test(Rows$WatSolP) # p=0.08332
hist(Rows$WatSolP) #  left skew
leveneTest(WatSolP~Treatment, data=Rows)  # P=0.919
#ModRWSP1
ModRWSP1 <- aov(WatSolP~Treatment+Block, data=Rows)
anova(ModRWSP1)
summary(ModRWSP1)
hist(resid(ModRWSP1))
shapiro.test(resid(ModRWSP1))  # p=0.06907
plot(fitted(ModRWSP1),resid(ModRWSP1),pch=16) #slight left cluster
qqnorm(resid(ModRWSP1))
qqline(resid(ModRWSP1))
ModRWSP1_tidy <- tidy(ModRWSP1)
ModRWSP1sum_sq_reg <- ModRWSP1_tidy$sumsq[1] 
ModRWSP1sum_sq_resid <- ModRWSP1_tidy$sumsq[2]
ModRWSP1sum_sq_reg / (ModRWSP1sum_sq_reg + ModRWSP1sum_sq_resid) #0.756
# ModRWSP2 glmm - possible singularity issue
ModRWSP2<- glmmTMB(WatSolP~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRWSP2, type="III")
summary(ModRWSP2)
performance::r2(ModRWSP2) # NA
shapiro.test(resid(ModRWSP2)) # p=0.167
plot(fitted(ModRWSP2),resid(ModRWSP2),pch=16) # slightly above the center line
qqnorm(resid(ModRWSP2)) # heavy tails
qqline(resid(ModRWSP2))

#emmeans on aov model
ModRemWSP <- emmeans(ModRWSP2,~Treatment, type="response")
ModRemWSP_cld <- cld(ModRemWSP, Letters=trimws(letters), reversed = TRUE) 
View(ModRemWSP_cld)
write.csv(ModRemWSP_cld, file="Rows_WatSolP.csv")




#####   PH   ##########
RowpH_Mean <- summary_by(pH~Treatment+Block, data=Rows, FUN=mean) 
RowpH_Mean <- as.numeric(RowpH_Mean$pH)
RowpH_skew <- skewness(RowpH_Mean,na.rm=TRUE)
RowpH_kur <- kurtosis(RowpH_Mean,na.rm=TRUE)
cat("Skewness:", RowpH_skew, "\n") ## -0.05430351 
cat("Kurtosis:", RowpH_kur, "\n") ## -1.116367 
shapiro.test(Rows$pH) # p=0.3393
hist(Rows$pH) 
leveneTest(pH~Treatment, data=Rows)  # P=0.9392
#ModRpH1
ModRpH1 <- aov(pH~Treatment+Block, data=Rows)
anova(ModRpH1)
summary(ModRpH1)
hist(resid(ModRpH1))
shapiro.test(resid(ModRpH1))  # p=0.5233
plot(fitted(ModRpH1),resid(ModRpH1),pch=16) # clusters to each side
qqnorm(resid(ModRpH1))
qqline(resid(ModRpH1))
ModRpH1_tidy <- tidy(ModRpH1)
ModRpH1sum_sq_reg <- ModRpH1_tidy$sumsq[1] 
ModRpH1sum_sq_resid <- ModRpH1_tidy$sumsq[2]
ModRpH1sum_sq_reg / (ModRpH1sum_sq_reg + ModRpH1sum_sq_resid) #0.1980843
# ModRpH2 glmm
ModRpH2<- glmmTMB(pH~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModRpH2, type="III")
summary(ModRpH2)
performance::r2(ModRpH2) # 0.589
shapiro.test(resid(ModRpH2)) # p=0.789
plot(fitted(ModRpH2),resid(ModRpH2),pch=16) # normal
qqnorm(resid(ModRpH2)) # slight tails
qqline(resid(ModRpH2))

#emmeans on aov model
ModRempH1 <- emmeans(ModRpH2,~Treatment, type="response")
ModRempH1_cld <- cld(ModRempH1, Letters=trimws(letters), reversed = TRUE) 
View(ModRempH1_cld)
write.csv(ModRempH1_cld, file="Rows_pH.csv")



#####   ELECTRICAL CONDUCTIVITY   ##########
RowEC_Mean <- summary_by(EC~Treatment+Block, data=Rows, FUN=mean) 
RowEC_Mean <- as.numeric(RowEC_Mean$EC)
RowEC_skew <- skewness(RowEC_Mean,na.rm=TRUE)
RowEC_kur <- kurtosis(RowEC_Mean,na.rm=TRUE)
cat("Skewness:", RowEC_skew, "\n") ## 0.01260481 
cat("Kurtosis:", RowEC_kur, "\n") ## -0.7270955
hist(Rows$EC)
leveneTest(EC~Treatment, data=Rows)  # P=0.0005556
shapiro.test(Rows$EC) # p=0.376
leveneTest(log(EC)~Treatment, data=Rows)  # p = 0.003358
shapiro.test(log(Rows$EC))  # p=0.07704
leveneTest(log10(EC)~Treatment, data=Rows)  # p = 0.003358
shapiro.test(log10(Rows$EC))  # p=0.07704
leveneTest(sqrt(EC)~Treatment, data=Rows)  # p = 0.001312 
shapiro.test(sqrt(Rows$EC))  # p=0.2373
leveneTest(1/(EC)~Treatment, data=Rows)  # p = 0.01592  (Reciprocal Transformation)
shapiro.test(1/(Rows$EC))  # 0.002539
#ModREC1
ModREC1 <- aov(sqrt(EC)~Treatment+Block, data=Rows)
anova(ModREC1)
summary(ModREC1)
hist(resid(ModREC1))
shapiro.test(resid(ModREC1))  # p=0.3185
plot(fitted(ModREC1),resid(ModREC1),pch=16) 
qqnorm(resid(ModREC1)) # slight tails, especially on th eupper end
qqline(resid(ModREC1))
ModREC1_tidy <- tidy(ModREC1)
ModREC1sum_sq_reg <- ModREC1_tidy$sumsq[1] 
ModREC1sum_sq_resid <- ModREC1_tidy$sumsq[2]
ModREC1sum_sq_reg / (ModREC1sum_sq_reg + ModREC1sum_sq_resid) #0.808
# ModREC2 glmm
ModREC2<- glmmTMB(EC~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModREC2, type="III")
summary(ModREC2)
performance::r2(ModREC2) # 0.496
shapiro.test(resid(ModREC2)) # p=0.5597
plot(fitted(ModREC2),resid(ModREC2),pch=16) # normal
qqnorm(resid(ModREC2)) # moderate-heavy tails
qqline(resid(ModREC2))

#emmeans on aov model
ModRemEC1 <- emmeans(ModREC2,~Treatment, type="response")
ModRemEC1_cld <- cld(ModRemEC1, Letters=trimws(letters), reversed = TRUE) 
View(ModRemEC1_cld)
write.csv(ModRemEC1_cld, file="Rows_EC.csv")




#####   ORGANIC CARBON   ##########
RowOC_Mean <- summary_by(OC~Treatment, data=Rows, FUN=mean) 
View(RowOC_Mean)
RowOC_Mean <- as.numeric(RowOC_Mean$OC)
RowOC_skew <- skewness(RowOC_Mean,na.rm=TRUE)
RowOC_kur <- kurtosis(RowOC_Mean,na.rm=TRUE)
cat("Skewness:", RowOC_skew, "\n") # 0.6676426  
cat("Kurtosis:", RowOC_kur, "\n") # -0.9402171  
shapiro.test(Rows$OC) # p=0.1426
plot(Rows$OC)
hist(Rows$OC) #  slight left skew
leveneTest(OC~Treatment, data=Rows)  # p=0.009993
#ModROC1
ModROC1 <- aov(OC~Treatment+Block, data=Rows)
anova(ModROC1)
summary(ModROC1)
hist(resid(ModROC1))
shapiro.test(resid(ModROC1))  # p=0.6056
plot(fitted(ModROC1),resid(ModROC1),pch=16) 
qqnorm(resid(ModROC1))
qqline(resid(ModROC1))
ModROC1_tidy <- tidy(ModROC1)
ModROC1sum_sq_reg <- ModROC1_tidy$sumsq[1] 
ModROC1sum_sq_resid <- ModROC1_tidy$sumsq[2]
ModROC1sum_sq_reg / (ModROC1sum_sq_reg + ModROC1sum_sq_resid) #0.906
#test variances
var(Rows$OC) # 0.01820393
OCresVar <- var(resid(ModROC1))  # 0.01155131
Rows$Treatment <- as.factor(Rows$Treatment)
Rows$Block <- as.factor(Rows$Block)
OCresVar2 <- with(Rows, aggregate(resid(ModROC1)^2, by=list(as.factor(Treatment)), var))
OCresVar2<- OCresVar2 %>% rename(Treatment = "Group.1")
OCresVar2<- OCresVar2 %>% rename(Variance="x")
View(OCresVar2)
# weighted aov
OCweights <-  1/resid(ModROC1)^2
ModROC2 <- aov(OC~Treatment+Block, data=Rows, weights= OCweights)
anova(ModROC2)
summary(ModROC2)
hist(resid(ModROC2))
shapiro.test(resid(ModROC2))  # p=0.6846
plot(fitted(ModROC2),resid(ModROC2),pch=16) 
qqnorm(resid(ModROC2))
qqline(resid(ModROC2))
ModROC2_tidy <- tidy(ModROC2)
ModROC2sum_sq_reg <- ModROC2_tidy$sumsq[1] 
ModROC2sum_sq_resid <- ModROC2_tidy$sumsq[2]
ModROC2sum_sq_reg / (ModROC2sum_sq_reg + ModROC2sum_sq_resid) #0.6617664
# ModROC3 glmm - possible singulary issues
ModROC3<- glmmTMB(OC~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModROC3, type="III")
summary(ModROC3)
performance::r2(ModROC3) # NA
shapiro.test(resid(ModROC3)) # p=0.654
plot(fitted(ModROC3),resid(ModROC3),pch=16) # normal, slight upper left shift
qqnorm(resid(ModROC3)) # slight-moderate tails
qqline(resid(ModROC3))
#ModROC4 lmer - singularuty
ModROC4 <- lmer(OC~Treatment+(1|Block), data=Rows, na.action=na.exclude) 
anova(ModROC4) 
summary(ModROC4)
shapiro.test(resid(ModROC4)) # p=0.6541
plot(fitted(ModROC4),resid(ModROC4),pch=16) # normal, slight upper left shift
qqnorm(resid(ModROC4)) # slight tails
qqline(resid(ModROC4))
rsq(ModROC4)  # 0.331
# ModROC5 lme model
ModROC5 <- lme(OC ~ Treatment, random=~1|Block, data=Rows, na.action=na.exclude)
summary(ModROC5)
anova(ModROC5)
shapiro.test(resid(ModROC5)) # p= 0.654
plot(fitted(ModROC5),resid(ModROC5),pch=16) # normal, slight upper left shift
qqnorm(resid(ModROC5)) # slight-moderate tails
qqline(resid(ModROC5))
rsq(ModROC5)  ## 0.331

#AIC and BIC values
RowsOC_modlist <- list(ModROC3, ModROC4, ModROC5)
AIC_values <- sapply(RowsOC_modlist, AIC)
BIC_values <- sapply(RowsOC_modlist, BIC)
PrecRowsOC <- data.frame(Model=c("ModROC3", "ModROC4", "ModROC5"),AIC_values, BIC_values)
print(PrecRowsOC)
#Model  AIC_values BIC_values
#1 ModROC3 -31.2802314  -16.62287
#2 ModROC4  -0.4654489   14.19191
#3 ModROC5  -0.4654489   11.31509

#emmeans on lme  model - no singularity issues
ModRemOC1 <- emmeans(ModROC5,~Treatment, type="response")
ModRemOC1_cld <- cld(ModRemOC1, Letters=trimws(letters), reversed = TRUE)
View(ModRemOC1_cld)
write.csv(ModRemOC1_cld, file="Rows_OC.csv")



####  Covariance heat maps  ####
#####   Yield  #####
RowsCovVar <- c("Biomass", "NO3", "PO4", "WatSolP", "ResinP", "pH", "EC", "OC")
RowsCovYield <- subset(Rows, select=c("Treatment", RowsCovVar), 
                      na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
RowsCovScaleYield <- as.data.frame(scale(RowsCovYield[,-1])) #remove treatment
RowsCovScaleYield$Treatment <- RowsCovYield$Treatment
RowsCovYieldSplit <- split(RowsCovScaleYield[, -ncol(RowsCovScaleYield)], RowsCovScaleYield$Treatment)
YieldCov_Rows <- lapply(RowsCovYieldSplit, function(x) cov(x, use="pairwise.complete.obs"))
YieldCovRowsWb <- createWorkbook() 
for (i in seq_along(YieldCov_Rows)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(YieldCov_Rows)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(YieldCovRowsWb, sheet_name)
  writeData(YieldCovRowsWb, sheet=sheet_name, x=YieldCov_Rows[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(YieldCovRowsWb, "Rows_Yield_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
YieldCovRows_df <- lapply(seq_along(YieldCov_Rows), function(i) {
  cov_mat1h <- as.matrix(YieldCov_Rows[[i]])
  cov_mat1h <- setNames(cov_mat1h, YieldCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(YieldCov_Rows)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
YieldCovRows_dfAll <- do.call(rbind, YieldCovRows_df)
YieldCovRows_dfAll$Var1 <- factor(YieldCovRows_dfAll$Var1, levels=RowsCovVar, labels=c("Biomass"="Yield", "NO3"="NO3", 
                  "PO4"="PO4", "WatSolP"="Water Soluble P", "ResinP"="Resin P","pH"="pH", "EC"="EC", "OC"="% SOC"))
YieldCovRows_dfAll$variable <- factor(YieldCovRows_dfAll$variable, levels=RowsCovVar, labels=
                  c("Biomass"="Yield", "NO3"="NO3", "PO4"="PO4", "WatSolP"="Water Soluble P", "ResinP"="Resin P",
                    "pH"="pH", "EC"="EC", "OC"="% SOC"))
YieldCovRows_dfAll$treatment <- factor(YieldCovRows_dfAll$treatment, 
           levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
           labels=c("Control 1", "Control 2", "Canola Meal", "Manure", "Willow", "Meat & BoneMeal - Coarse",
                    "Meat & Bonemeal - Fine", "Phosphorus Fertilizer"))
write.csv(YieldCovRows_dfAll, file="Rows_YieldCov.csv")
# ggplot best option - brackets on both sides of the variable and plot code assigns and calls all in one
(YieldCovRowsHeat <- ggplot(YieldCovRows_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "YlGnBu"), limits=c(-2.8, 4.3), breaks=seq(-2.8, 4.3, by=1)) +
    facet_wrap(~ treatment, nrow=3, scales="fixed") +
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
ggsave(YieldCovRowsHeat, file="Rows_YieldCovHeat.jpg", width=20, height=20, dpi=150)



#####   Uptake  #####
UptakeCovVar <- c("Puptake", "NO3", "PO4", "WatSolP", "ResinP", "pH", "EC", "OC")
RowsCovUptake <- subset(Rows, select=c("Treatment", UptakeCovVar), 
                        na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
RowsCovScaleUptake <- as.data.frame(scale(RowsCovUptake[,-1]))
RowsCovScaleUptake$Treatment <- RowsCovUptake$Treatment
RowsCovScaleUptakeSplit <- split(RowsCovScaleUptake[, -ncol(RowsCovScaleUptake)], RowsCovScaleUptake$Treatment)
## calculate the covariance matrix for each treatment excluding missing data
UptakeCov_Rows <- lapply(RowsCovScaleUptakeSplit, function(x) cov(x, use="pairwise.complete.obs"))
UptakeCovRowsWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(UptakeCov_Rows)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(UptakeCov_Rows)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(UptakeCovRowsWb, sheet_name)
  writeData(UptakeCovRowsWb, sheet=sheet_name, x=UptakeCov_Rows[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(UptakeCovRowsWb, "Rows_Uptake_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
UptakeCovRows_df <- lapply(seq_along(UptakeCov_Rows), function(i) {
  cov_mat1h <- as.matrix(UptakeCov_Rows[[i]])
  cov_mat1h <- setNames(cov_mat1h, UptakeCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(UptakeCov_Rows)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
UptakeCovRows_dfAll <- do.call(rbind, UptakeCovRows_df)
UptakeCovRows_dfAll$Var1 <- factor(UptakeCovRows_dfAll$Var1, levels=UptakeCovVar, labels=c("Biomass"="Yield", 
                  "NO3"="NO3", "PO4"="PO4", "WatSolP"="Water Soluble P", "ResinP"="Resin P","pH"="pH", "EC"="EC", 
                  "OC"="% SOC"))
UptakeCovRows_dfAll$variable <- factor(UptakeCovRows_dfAll$variable, levels=UptakeCovVar, labels= c("Biomass"="Yield", 
                  "NO3"="NO3", "PO4"="PO4", "WatSolP"="Water Soluble P", "ResinP"="Resin P","pH"="pH", "EC"="EC", 
                  "OC"="% SOC"))
UptakeCovRows_dfAll$treatment <- factor(UptakeCovRows_dfAll$treatment, 
            levels=c("Control1", "Control2", "CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
            labels=c("Control 1", "Control 2", "Canola Meal", "Manure", "Willow", "Meat & BoneMeal - Coarse",
                     "Meat & Bonemeal - Fine", "Phosphorus Fertilizer"))
write.csv(UptakeCovRows_dfAll, file="Rows_UptakeCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(UptakeCovRowsHeat <- ggplot(UptakeCovRows_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "PuBuGn"), limits=c(-2.8, 4.3), breaks=seq(-2.8, 4.3, by=1)) +
    facet_wrap(~ treatment, nrow=3, ncol=3, scales="fixed") +
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
ggsave(UptakeCovRowsHeat, file="Rows_UptakeCovHeat.jpg", width=20, height=20, dpi=150)

#####   P Recovery  #####
RecoveryCovVar <- c("Precovery", "NO3", "PO4", "WatSolP", "ResinP", "pH", "EC", "OC")
RowsCovRecovery <- subset(Rows, select=c("Treatment", RecoveryCovVar), 
                        na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
RowsCovScaleRecovery <- as.data.frame(scale(RowsCovRecovery[,-1]))
RowsCovScaleRecovery$Treatment <- RowsCovRecovery$Treatment
RowsCovScaleRecoverySplit <- split(RowsCovScaleRecovery[, -ncol(RowsCovScaleRecovery)], RowsCovScaleRecovery$Treatment)
RemoveControls <- c("Control1", "Control2")
RowsCovScaleRecoverySplit <- RowsCovScaleRecoverySplit[!(names(RowsCovScaleRecoverySplit) %in% RemoveControls)]
## calculate the covariance matrix for each treatment excluding missing data
RecoveryCov_Rows <- lapply(RowsCovScaleRecoverySplit, function(x) cov(x, use="pairwise.complete.obs"))
RecoveryCovRowsWb <- createWorkbook() # create workbook to save in xlsx
for (i in seq_along(RecoveryCov_Rows)) { # for loop to bring all matrices into separate worksheets
  treatment_name <- names(RecoveryCov_Rows)[i] # make sure that treatment names are used and not repeat first treatment
  sheet_name <- paste0(treatment_name)
  addWorksheet(RecoveryCovRowsWb, sheet_name)
  writeData(RecoveryCovRowsWb, sheet=sheet_name, x=RecoveryCov_Rows[[i]], startRow=1, startCol=1, rowNames=TRUE)
}
saveWorkbook(RecoveryCovRowsWb, "Rows_Recovery_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
RecoveryCovRows_df <- lapply(seq_along(RecoveryCov_Rows), function(i) {
  cov_mat1h <- as.matrix(RecoveryCov_Rows[[i]])
  cov_mat1h <- setNames(cov_mat1h, RecoveryCovVar)
  cov_df1h <- as.data.frame(cov_mat1h)
  cov_df1h$Var1 <- rownames(cov_df1h)
  cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
  cov_df1h_long$treatment <- names(RecoveryCov_Rows)[i]
  return(cov_df1h_long)
})
# Combine all dataframes into one and set the variable names as factors and in the correct order
RecoveryCovRows_dfAll <- do.call(rbind, RecoveryCovRows_df)
RecoveryCovRows_dfAll$Var1 <- factor(RecoveryCovRows_dfAll$Var1, levels=RecoveryCovVar, labels= c("Biomass"="Yield", 
                      "NO3"="NO3", "PO4"="PO4", "WatSolP"="Water Soluble P", "ResinP"="Resin P","pH"="pH", "EC"="EC", 
                      "OC"="% SOC"))
RecoveryCovRows_dfAll$variable <- factor(RecoveryCovRows_dfAll$variable, levels=RecoveryCovVar, labels=c("Biomass"="Yield", 
                      "NO3"="NO3", "PO4"="PO4", "WatSolP"="Water Soluble P", "ResinP"="Resin P",
                      "pH"="pH", "EC"="EC", "OC"="% SOC"))
RecoveryCovRows_dfAll$treatment <- factor(RecoveryCovRows_dfAll$treatment, 
                 levels=c("CanolaMeal", "Manure", "Willow", "MBMACoarse", "MBMAFine", "Phosphorus"),
                 labels=c("Canola Meal", "Manure", "Willow", "Meat & BoneMeal - Coarse",
                          "Meat & Bonemeal - Fine", "Phosphorus Fertilizer"))
write.csv(RecoveryCovRows_dfAll, file="Rows_RecoveryCov.csv")
# Generate the heatmap for each treatment and facet wrap them
(RecoveryCovRowsHeat <- ggplot(RecoveryCovRows_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
    geom_tile() +
    scale_fill_gradientn(colors=brewer.pal(9, "YlOrRd"), limits=c(-2.8, 4.3), breaks=seq(-2.8, 4.3, by=1)) +
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
ggsave(RecoveryCovRowsHeat, file="Rows_RecoveryCovHeat.jpg", width=20, height=15, dpi=150)


####   Yield to N & P Recovery  ####
Rows$Treatment <- as.factor(Rows$Treatment)
Rows$Biomass <- as.numeric(Rows$Biomass)
Rows$Nrecovery <- as.numeric(Rows$Nrecovery)
Rows$Precovery <- as.numeric(Rows$Precovery)
RowsContourSub <- subset(Rows, Treatment != "Control1" & Treatment != "Control2", select = c(Block, Treatment, 
                        Biomass, Nrecovery, Precovery))
View(RowsContourSub)
RowsContourSub$Treatment <- factor(RowsContourSub$Treatment, levels=c("CanolaMeal", "Manure", "Willow", 
                                    "MBMACoarse", "MBMAFine", "Phosphorus"),
               labels=c("Canola Meal", "Manure", "Willow", "Meat & BoneMeal - Coarse", "Meat & Bonemeal - Fine", 
                        "Phosphorus Fertilizer"))
View(RowsContourSub)
RowsContourExcl <- na.exclude(RowsContourSub)
View(RowsContourExcl)

RowsContourMod <- glmmTMB(Biomass ~ Nrecovery + Precovery + Treatment + (1|Block), data = RowsContourExcl, 
                           na.action=na.exclude)
summary(RowsContourMod)
Anova(RowsContourMod)
#Set up N & P recovery grids per soil
RowsNrecovery_grid <- seq(min(RowsContourExcl$Nrecovery, na.rm = TRUE), max(RowsContourExcl$Nrecovery, na.rm = TRUE),
                         length.out = 100)
RowsPrecovery_grid <- seq(min(RowsContourExcl$Precovery, na.rm = TRUE), max(RowsContourExcl$Precovery, na.rm = TRUE),
                         length.out = 100)
# Set up expanded grids then assign yield - must include block as it was used in the model!!
RowsContour_grid <- expand.grid(Block=unique(RowsContourExcl$Block), Treatment = unique(RowsContourExcl$Treatment), 
                                Nrecovery = RowsNrecovery_grid, Precovery = RowsPrecovery_grid)
RowsContour_grid$Yield <- predict(RowsContourMod, newdata = RowsContour_grid)
RowsContour_grid <- RowsContour_grid[,-1] # remove block so it doesn't appear in the plot
View(RowsContour_grid)
# develop contour plot
(RowsContours <- ggplot(RowsContour_grid, aes(x = Nrecovery, y = Precovery, z = Yield)) +
    geom_raster(aes(fill=Yield)) + #use rastar to get smooth lines
    geom_contour(aes(z=Yield), color='gray30', binwidth = 20) + #contour line, adjust binwidth depending on yield
    facet_wrap(~Treatment, nrow = 5) +
    scale_fill_gradientn(colors = brewer.pal(9, "BuPu")) +
    labs(x = "% N Recovery", y = "% P Recovery", fill = "Yield\n(kg/ha)") +
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
ggsave(RowsContours, file="Rows_YieldContour.jpg", width=20, height=20, dpi=150)


####  Extract ANOVA tables  ####
ModRBio5 <- lme(Biomass ~ Treatment, random=~1|Block, data=Rows)
RYieldAN <- anova(ModRBio5)
RYieldAN$RowNames <- row.names(RYieldAN)
rownames(RYieldAN) <- NULL

ModRNup2<- glmmTMB(Nuptake~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RNupAN <- glmmTMB:::Anova.glmmTMB(ModRNup2, type="III")
RNupAN$RowNames <- row.names(RNupAN)
rownames(RNupAN) <- NULL

ModRNrec2<- glmmTMB(Nrecovery~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RNrecAN <- glmmTMB:::Anova.glmmTMB(ModRNrec2, type="III")
RNrecAN$RowNames <- row.names(RNrecAN)
rownames(RNrecAN) <- NULL

ModRPup2<- glmmTMB(Puptake~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RPupAN <- glmmTMB:::Anova.glmmTMB(ModRPup2, type="III")
RPupAN$RowNames <- row.names(RPupAN)
rownames(RPupAN) <- NULL

ModRPrec2<- glmmTMB(Precovery~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RPrecAN <- glmmTMB:::Anova.glmmTMB(ModRPrec2, type="III")
RPrecAN$RowNames <- row.names(RPrecAN)
rownames(RPrecAN) <- NULL

ModRNO32<- glmmTMB(NO3~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RNO3AN <- glmmTMB:::Anova.glmmTMB(ModRNO32, type="III")
RNO3AN$RowNames <- row.names(RNO3AN)
rownames(RNO3AN) <- NULL

ModRPO42<- glmmTMB(PO4~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RPO4AN <- glmmTMB:::Anova.glmmTMB(ModRPO42, type="III")
RPO4AN$RowNames <- row.names(RPO4AN)
rownames(RPO4AN) <- NULL

ModResP2<- glmmTMB(ResinP~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RResPAN <- glmmTMB:::Anova.glmmTMB(ModResP2, type="III")
RResPAN$RowNames <- row.names(RResPAN)
rownames(RResPAN) <- NULL

ModRWSP2<- glmmTMB(WatSolP~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RWSPAN <- glmmTMB:::Anova.glmmTMB(ModRWSP2, type="III")
RWSPAN$RowNames <- row.names(RWSPAN)
rownames(RWSPAN) <- NULL

ModRpH2<- glmmTMB(pH~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RpHAN <- glmmTMB:::Anova.glmmTMB(ModRpH2, type="III")
RpHAN$RowNames <- row.names(RpHAN)
rownames(RpHAN) <- NULL

ModREC2<- glmmTMB(EC~Treatment+(1|Block), data=Rows, family=gaussian(), na.action=na.exclude)
RecAN <- glmmTMB:::Anova.glmmTMB(ModREC2, type="III")
RecAN$RowNames <- row.names(RecAN)
rownames(RecAN) <- NULL

ModROC5 <- lme(OC ~ Treatment, random=~1|Block, data=Rows, na.action=na.exclude)
RocAN <- anova(ModROC5)
RocAN$RowNames <- row.names(RocAN)
rownames(RocAN) <- NULL


RowsANOVAtables <- list(RYieldAN, RNupAN, RNrecAN, RPupAN, RPrecAN, RNO3AN, RPO4AN, RResPAN, RWSPAN,
                         RpHAN, RecAN, RocAN)
names(RowsANOVAtables) <- c("Yield", "Nuptake", "Nrecovery", "Puptake","Precovery", "NO3", "PO4", 
                             "ResinP", "WaterSolP", "pH", "EC", "OC")
write_xlsx(RowsANOVAtables, path = "RowsANOVAtables.xlsx")


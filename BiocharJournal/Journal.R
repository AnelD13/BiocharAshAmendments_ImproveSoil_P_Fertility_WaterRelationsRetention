# Loading data in to R & Summaries ----
## outliers have already been removed
Pots1<-read.csv("PotsJ.csv", fileEncoding="UTF-8-BOM")
Field<-read.csv("FieldJ.csv", fileEncoding="UTF-8-BOM")
Infil<-read.csv("InfiltrationJ.csv", fileEncoding="UTF-8-BOM") # full dataset with 8-12 observations per plot
Infilsub <- read.csv("InfilJ.csv", fileEncoding="UTF-8-BOM") # #combined data with 20 observations total

## Loading libraries ----
library(summarytools) # more concise summaries 
library(stats)
library(sjstats)
library(lme4) # lmer model
library(lmerTest) # provides ANOVA and summary capabilities
library(nlme) # lme model
library(glmmTMB)
library(doBy) # working with grouped data, specifically the do and by functions
library(ggplot2)
library(ggpattern)
library(ggtext) # for fixing markdown text in ggplot
library(ggforce) # specialized plots
library(ggrepel) # repel labels on plots (WHC)
library(ggExtra) # add marginal layers to plot
library(ggcorrplot) # correlations plots
library(ggh4x) # customize ggplot facets
library(lemon) # for facet_rep_wrap
library(plotrix) # various plot options, I used it for ablines
library(cowplot) # combine ggplot output into a single graph
library(patchwork) #similar to cowplot
library(gridExtra) # Functions for grid based plots
library(ggpubr) # make ggplots publication ready
library(car) # general package with lots of uses, including Anova
library(afex) # used to bring raw data into ggplot
library(onewaytests) # for one way ANOVAs
library(plyr) # breaking larger sets into smaller sets and afterward back together
library(dplyr) # general package with lots of uses
library(tidyr) # tidies up data
library(magrittr) # pipe operator %>%
library(readr) # easy way to read csv and other 'rectangular' files
library(broom) # cleans up messy output into tibbles
library(multcomp) # simultaneous test and confidence intervals
library(multcompView) # for correlations
library(DataVisualizations) # various type of analysis through visualisation
library(wCorr) # for calculating correlations
library(corrr)
library(emmeans) # estimated marginal means in place of lsmeans
library(e1071) #skewness and kurtosis
library(rsq)
library(pgirmess) # reading, writing and transforming spatial and seasonal data
library(robustbase) # Tools allowing analysis with robust methods
library(MASS)
library(RColorBrewer) # colour package
library(viridis) # colour package
library(reshape2) # for long/wide data frame changes
library(metaSEM) # for structural equation modellling
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses 
library(openxlsx)  # write xls or slsx files
library(writexl) #  as for openxlsx; specify path="xx.csv"

--
### p-value interpretation: p >0.05, no significant difference, fail to reject null hypothesis
--

  
# CONTROLLED ENVIONMENT STUDY ----
## Summary and ordering of data ----
Pots1$Soil <- factor(Pots1$Soil, levels=c("Haverhill", "Oxbow"))
    
# used for 'levels'
Pots1Trt_order <- as.factor(c("Control", "CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha",
                    "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha",  "Willow10tha", "CanolaMeal10thaTSP",
                    "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate")) # general
Pots1Trt_sub <- as.factor(c("CanolaMeal50kgha", "CanolaHull50kgha", "Manure50kgha", "Willow50kgha",
                  "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha",  "Willow10tha", "CanolaMeal10thaTSP",
                  "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP", "TripleSuperPhosphate")) # no control
CharCon <- as.factor(c("Control", "CanolaMeal10tha", "CanolaHull10tha", "Manure10tha", "Willow10tha", "CanolaMeal10thaTSP",
             "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP","TripleSuperPhosphate")) # split plot 10t/ha
TrtVar <- as.factor(c("Control","CanolaMeal50kgha","CanolaHull50kgha","Manure50kgha","Willow50kgha",
                  "TripleSuperPhosphate")) # split plot 25kg P/ha
Reduced_trtVar <- as.factor(c("CanolaHull50kgha","CanolaMeal50kgha","Manure50kgha","Willow50kgha", "TripleSuperPhosphate"))
Reduced_charcon <- as.factor(c("CanolaMeal10tha", "CanolaHull10tha", "Manure10tha",  "Willow10tha", "CanolaMeal10thaTSP", 
                               "CanolaHull10thaTSP", "Manure10thaTSP", "Willow10thaTSP","TripleSuperPhosphate"))

    
# used for 'labels'
PotsLabel_Main <- as.factor(c("Control", "Canola Meal 25mg P/ha", "Canola Hull 50kg P/ha", 
                    "Manure 50kg P/ha", "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", 
                    "Manure 10t/ha", "Willow 10t/ha", "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", 
                    "Manure 10t/ha & TSP", "Willow 10t/ha & TSP", "TSP Fertilizer")) # everything included
PotsLabel_Sub <- as.factor(c("Canola Meal 50kg P/ha", "Canola Hull 50kg P/ha", 
                   "Manure 50kg P/ha", "Willow 50kg P/ha", "Canola Meal 10t/ha", "Canola Hull 10t/ha", 
                   "Manure 10t/ha", "Willow 10t/ha", "Canola Meal 10t/ha & TSP", "Canola Hull 10t/ha & TSP", 
                   "Manure 10t/ha & TSP", "Willow 10t/ha & TSP", "TSP Fertilizer")) # no control
PotsLabDash_main <- as.factor(c("Control", "Canola Meal\n50kg P/ha", "Canola Hull\n50kg P/ha", "Manure\n50kg P/ha", 
                         "Willow\n50kg P/ha", "Canola Meal\n10t/ha", "Canola Hull\n10t/ha", "Manure\n10t/ha", "Willow\n10t/ha", 
                         "Canola Meal\n10t/ha & TSP", "Canola Hull\n10t/ha & TSP", "Manure\n10t/ha & TSP", 
                         "Willow\n10t/ha & TSP", "TSP\nFertilizer"))
PotsLabDash_sub <- as.factor(c("Canola Meal\n50kg P/ha", "Canola Hull\n50kg P/ha", "Manure\n50kg P/ha", 
                   "Willow\n50kg P/ha", "Canola Meal\n10t/ha", "Canola Hull\n10t/ha", "Manure\n10t/ha", "Willow\n10t/ha", 
                   "Canola Meal\n10t/ha & TSP", "Canola Hull\n10t/ha & TSP", "Manure\n10t/ha & TSP", 
                   "Willow\n10t/ha & TSP", "TSP\nFertilizer"))
PotsTrtVar <- as.factor(c("Control", "Canola Meal", "Canola Hull", "Manure", "Willow", "TSP\nFertilizer"))
PotsCharCon <- as.factor(c("Control", "Canola Meal", "Canola Hull", "Manure", "Willow",
                 "Canola Meal\n& TSP", "Canola Hull\n& TSP", "Manure\n& TSP", "Willow\n& TSP", 
                 "TSP\nFertilizer"))
PotsRedTrtVar <- as.factor(c("Canola Meal", "Canola Hull", "Manure", "Willow", "TSP\nFertilizer"))
PotsRedCarcon <- as.factor(c("Canola Meal", "Canola Hull", "Manure", "Willow", "Canola Meal\n& TSP", 
                             "Canola Hull\n& TSP","Manure\n& TSP", "Willow\n& TSP", "TSP\nFertilizer"))

# function to determine kurtosis and skewness
SkewKur <- function(data, variable_names) { #uses 'moments' instead of 'e1071' which results in much higher kurtosis
  stats <- data %>%
    summarise_at(vars(all_of(variable_names)), list(skewness = ~ skewness(., na.rm = TRUE), kurtosis = ~ kurtosis(., na.rm = TRUE)))
  return(stats)
}


## PLANT ANALYSIS ----
### Yield ----
(PotsYieldStats <- SkewKur(Pots1, "Yield"))
      # skewness   kurtosis
      # -0.3157451 -0.5957768
shapiro.test(Pots1$Yield) # p=0.0101
hist(Pots1$Yield) # mostly normal
leveneTest(Yield~Treatment*Soil, data=Pots1)  # p=0.038
# Pots1YieldMod1 glmm - singularity issue
Pots1YieldMod1 <- glmmTMB(Yield~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Pots1YieldMod1, type="III") # signficant differences
summary(Pots1YieldMod1)
performance::r2(Pots1YieldMod1) # NA, singularity issue
# Pots1YieldMod2 Lmer
Pots1YieldMod2<-lmer(Yield~Treatment*Soil+(1|Soil),data=Pots1)
Anova(Pots1YieldMod2, type="III", alpha=0.001)
summary(Pots1YieldMod2)
rsq(Pots1YieldMod2) # 0.86
#Pots1YieldMod3 - lme
Pots1YieldMod3<-lme(Yield~Treatment*Soil,random=~1|Soil, data=Pots1, na.action=na.exclude)
Anova(Pots1YieldMod3, type="III")
summary(Pots1YieldMod3)
rsq(Pots1YieldMod3) # NA
#Pots1YieldMod4 - glmer
Pots1YieldMod4 <- glmer(Yield~Treatment*Soil+(1|Soil),data=Pots1,family=gaussian(link="log"))
Anova(Pots1YieldMod4, type="III")
summary(Pots1YieldMod4)
rsq(Pots1YieldMod4) # NA

#Compare models - doesn't work for certain types of models
PotsYield_modlist <- list(Pots1YieldMod1, Pots1YieldMod2, Pots1YieldMod3, Pots1YieldMod4)
AIC_values <- sapply(PotsYield_modlist, AIC)
BIC_values <- sapply(PotsYield_modlist, BIC)
(PotsYieldAB <- data.frame(Model=c("Pots1YieldMod1", "Pots1YieldMod2", "Pots1YieldMod3", "Pots1YieldMod4"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 Pots1YieldMod1   384.5886   465.3290 - singularity issues
#2 Pots1YieldMod2   363.2100   443.9505 - preferred model, rsq 0.86
#3 Pots1YieldMod3   363.2100   435.0435 - 0 df
#4 Pots1YieldMod4   386.5886   467.3290 - infinite df

#emmeans for each soil separately
(ModPots1em_Yield <- emmeans(Pots1YieldMod2,~Treatment|Soil, infer = TRUE))
(ModPots1cld_Yield <- cld(ModPots1em_Yield, Letters=trimws(letters), reversed=TRUE, by="Soil", alpha=0.001))
write_xlsx(ModPots1cld_Yield, path="Pots1_Yield.xlsx")

##Developing visualizations
##select specific treatments and use those in the graph - repeat for all subsets
#Plotting dry weight using constant P and variable biochar rates
Yield_subVar <- ModPots1cld_Yield %>%
  filter(Treatment %in% TrtVar)
(Yield50kg <- ggplot(Yield_subVar, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean + SE + 0.5),
                  size=5, position=position_dodge(width=0.9))+
    labs(x="", y="Biomass yield (g)")+
    scale_x_discrete(limits = as.character(TrtVar), labels=PotsTrtVar)+
    scale_y_continuous(limits =  c(0,15))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=17, face="bold", colour="black"),
          axis.text.y=element_text(size=17, face="bold", colour="black"),
          axis.title.x=element_blank(), #text(size=22, face="bold", margin=margin(b=15)), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.4,0.4,0.4,0.7), "cm")))
#Plotting constant biochar rates with var P rates
Yield_subCon <- ModPots1cld_Yield %>%
  filter(Treatment %in% CharCon)
(Yield10tha <- ggplot(Yield_subCon, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean + SE + 0.5),
              size=5, position=position_dodge(width=0.9))+
    labs(x="", y="Biomass yield (g)") +
    scale_x_discrete(limits=as.character(CharCon), labels=PotsCharCon)+
    scale_y_continuous(limits =  c(0,15))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=17, face="bold", colour="black"),
          axis.text.y=element_blank(), #text(size=18, face="bold", colour="black"),
          axis.title.x=element_blank(), #text(size=22, face="bold", margin=margin(b=15)), 
          axis.title.y=element_blank(), #text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")))


### P uptake ----
(PotsPupStats <- SkewKur(Pots1, "Puptake"))
      # skewness   kurtosis
      # 0.006739401 -0.7595677
shapiro.test(Pots1$Puptake)  # p=0.011
hist(Pots1$Puptake) # slight left skew
leveneTest(Puptake ~ Treatment*Soil, data=Pots1) # p=0.31
shapiro.test(log(Pots1$Puptake))  # worsens normality
shapiro.test(sqrt(Pots1$Puptake))  # worsens normality
# Pots1PupMod1 glmm <-  singularity issues
Pots1PupMod1 <- glmmTMB(Puptake~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(Pots1PupMod1, type="III", alpha=0.001)
summary(Pots1PupMod1)
performance::r2(Pots1PupMod1) # NA
# Pots1YieldMod2 Lmer - model failed to converge
Pots1PupMod2<-lmer(Puptake~Treatment*Soil+(1|Soil),data=Pots1) 
Anova(Pots1PupMod2, type="III")
summary(Pots1PupMod2)
rsq(Pots1PupMod2) # 0.92
#Mod1d - lme
Pots1PupMod3<-lme(Puptake~Treatment*Soil,random=~1|Soil, data=Pots1, na.action=na.exclude)
Anova(Pots1PupMod3, type="III")
summary(Pots1PupMod3)
rsq(Pots1PupMod3) # NA
#Pots1YieldMod4 - glmer - singularity issues
Pots1PupMod4 <- glmer(Yield~Treatment*Soil+(1|Soil),data=Pots1,family=gaussian(link="log"))
Anova(Pots1YieldMod4, type="III")
summary(Pots1YieldMod4)
rsq(Pots1YieldMod4) # NA

#Compare models - doesn't work for certain types of models
PotsPup_modlist <- list(Pots1PupMod1, Pots1PupMod2, Pots1PupMod3, Pots1PupMod4)
AIC_values <- sapply(PotsPup_modlist , AIC)
BIC_values <- sapply(PotsPup_modlist , BIC)
(PotsPupAB <- data.frame(Model=c("Pots1PupMod1", "Pots1PupMod2", "Pots1PupMod3", "Pots1PupMod4"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 Pots1PupMod1   500.9396   581.9540 - 80 df, singularity issues due to too few random terms, chosen as best model
#2 Pots1PupMod2   451.0297   532.0441 - 85.3 df, convergence issues
#3 Pots1PupMod3   451.0297   523.2313 - zero df, NAN's produced
#4 Pots1PupMod4   386.5886   467.3290 - infinite df

# emmeans
(ModPots1em_Pup <- emmeans(Pots1PupMod1,~Treatment|Soil, infer = TRUE))
(ModPots1cld_Pup <- cld(ModPots1em_Pup, Letters=trimws(letters), reversed=TRUE, by="Soil", alpha=0.001))
write_xlsx(ModPots1cld_Pup, path="Pots1_P_uptake.xlsx")

## Visualizations
Pup_subVar <- ModPots1cld_Pup %>% filter(Treatment %in% TrtVar)
(Pup50kgha <- ggplot(Pup_subVar, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), width=0.2, position=position_dodge(width=0.9)) +
  geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+0.8),
            size=5, position=position_dodge(width=0.9))+
  labs(x="", y=bquote(bold("Phosphorus uptake (mg kg"^-1*~"soil)"))) +
  scale_x_discrete(limits = as.character(TrtVar), labels=PotsTrtVar)+
    scale_y_continuous(limits =  c(0,30))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
        axis.text.x=element_text(angle=65, hjust=1, size=17, face="bold", colour="black"),
        axis.text.y=element_text(size=18, face="bold", colour="black"),
        axis.title.x=element_blank(), #text(size=22, face="bold", margin=margin(b=15)), 
        axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
        panel.background=element_blank(),
        panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
        plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")))
#Plotting constant biochar rates with var P rates
Pup_subCon <- ModPots1cld_Pup %>% filter(Treatment %in% CharCon)
(Pup10tha <- ggplot(Pup_subCon, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                 width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+1.5),
              size=5, position=position_dodge(width=0.9))+
    labs(x="", y=bquote(bold("Phosphorus uptake (mg kg"^-1*~"soil)")))+
    scale_x_discrete(limits = as.character(CharCon), labels=PotsCharCon)+
    scale_y_continuous(limits =  c(0,30))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=17, face="bold", colour="black"),
          axis.text.y=element_blank(), #text(size=18, face="bold", colour="black"),
          axis.title.x=element_blank(), #text(size=22, face="bold", margin=margin(b=15)),  
          axis.title.y=element_blank(), #text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")))



### P Recovery ----
(PotsPrecStats <- SkewKur(Pots1, "Precovery"))
      # skewness kurtosis
      # 1.253337 2.097512
shapiro.test(Pots1$Precovery) # p=2.4e-6
hist(Pots1$Precovery) # left skewed
leveneTest(Precovery ~ Treatment*Soil, data=Pots1)  # p=1.6e-6
    ## cannot log or sqrt transform as there are negative values in the dataset
    #cannot use glmer with gamma distribution due to negative values
#Pots1PrecMod1 - applying Yeo-Johnson transformation
Pots1Prec_YJ <- yjPower(Pots1$Precovery, 0.5,jacobian.adjusted=TRUE)
Pots1PrecMod1 <- lmer(Pots1Prec_YJ ~ Treatment*Soil + (1|Soil), data=Pots1, na.action=na.omit) # convergence issues
Anova(Pots1PrecMod1, ytpe="III")
summary(Pots1PrecMod1)
rsq(Pots1PrecMod1) # 0.84
# Pots1PrecMod2 - lme
Pots1PrecMod2 <- lme(Precovery ~ Treatment*Soil, random=~1|Soil, data=Pots1, na.action=na.omit)
summary(Pots1PrecMod2)
Anova(Pots1PrecMod2, type="III")
rsq(Pots1PrecMod2) # 0.87
# Pots1PrecMod3 - glmm - singularity issues
Pots1PrecMod3 <- glmmTMB(Precovery~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.omit)
glmmTMB:::Anova.glmmTMB(Pots1PrecMod3, type="III")
summary(Pots1PrecMod3)
performance::r2(Pots1PrecMod3) # NA
# couldn't get glmer to work

#AIC and BIC values
Pots1Prec_modlist <- list(Pots1PrecMod1, Pots1PrecMod2, Pots1PrecMod3)
AIC_values <- sapply(Pots1Prec_modlist, AIC)
BIC_values <- sapply(Pots1Prec_modlist, BIC)
(PrecAB <- data.frame(Model=c("Pots1PrecMod1", "Pots1PrecMod2", "Pots1PrecMod3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 Pots1PrecMod1   628.1842   701.1290 - more df than samples
#2 Pots1PrecMod2   627.4533   691.9672 - df 1 NAN's produced
#3 Pots1PrecMod3   750.9711   823.9159 - singularity issues but seems to be the most appropriate model

#run emmeans
(ModPots1em_Prec<- emmeans(Pots1PrecMod3,~Treatment|Soil, infer = TRUE))
(ModPots1cld_Prec <- cld(ModPots1em_Prec, Letters=trimws(letters), reversed=TRUE, by="Soil", alpha=0.001))
write_xlsx(ModPots1cld_Prec, path="Pots1_P_recovery.xlsx")
##Developing visualizations
Prec_subVar <- ModPots1cld_Prec %>%
  filter(Treatment %in% Reduced_trtVar)
(Prec50kg <- ggplot(Prec_subVar, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+5),
              size=5, position=position_dodge(width=0.9))+
    labs(x=bquote(bold("Treatments at 50kg P ha"^-1)), y="Phosphorus recovery (%)") +
    scale_x_discrete(limits=as.character(Reduced_trtVar), labels=PotsRedTrtVar)+
    scale_y_continuous(limits =  c(0,105))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=17, face="bold", colour="black"),
          axis.text.y=element_text(size=18, face="bold", colour="black"),
          axis.title.x=element_text(size=22, face="bold", margin=margin(t=5)), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.4,0.4,0.4,0.7), "cm")))
#Plotting constant biochar rates with var P rates
Prec_charCon <- 
Prec_subCon <- ModPots1cld_Prec %>%
  filter(Treatment %in% Reduced_charcon)
(Prec10tha <- ggplot(Prec_subCon, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+5),
                  size=5, position=position_dodge(width=0.9))+
    labs(x=bquote(bold("Treatments at 10t biochar ha"^-1)), y="Phosphorus recovery (%)") +
    scale_x_discrete(limits=as.character(Reduced_charcon),labels=PotsRedCarcon)+
    scale_y_continuous(limits =  c(0,105))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=17, face="bold", colour="black"),
          axis.text.y=element_blank(), #text(size=18, face="bold", colour="black"),
          axis.title.x=element_text(size=22, face="bold", margin=margin(t=5)),
          axis.title.y=element_blank(), #text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm")))


### Combined plots ----
(PotsPlant_plot <- ggarrange(Yield50kg, Yield10tha, Pup50kgha, Pup10tha, Prec50kg, Prec10tha, nrow=3, ncol=2, 
                             common.legend=TRUE, legend="top",
                             labels = "AUTO", label.x = c(0.16, 0.03, 0.17, 0.03, 0.18, 0.03),
                             font.label=list(size=24, face="bold")))
ggsave(PotsPlant_plot, file="Fig1.Chamber_Plants.jpg", height=16, width=14, dpi=250)
ggsave(PotsPlant_plot, file="Fig1.Chamber_Plants.tiff", height=16, width=14, dpi=150)




### Phosphorus use efficiency ----
(PotsPUEstats <- SkewKur(Pots1, "PUE"))
      #skewness kurtosis
      #1 1.566646  3.15303
shapiro.test(Pots1$PUE)  #p=1.372e-08
hist(Pots1$PUE) #  left skewed
leveneTest(PUE ~ Treatment*Soil, data=Pots1)  # 0.059
    # can't log or sqrt transform due to negative values
# glmm mod
Post1PUEmod1 <- glmmTMB(PUE~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # singularity issues
glmmTMB:::Anova.glmmTMB(Post1PUEmod1, type="III")
summary(Post1PUEmod1)
performance::r2(Post1PUEmod1) # NA
# lmer mod
Post1PUEmod2 <- lmer(PUE ~ Treatment*Soil + (1|Soil), data=Pots1, na.action=na.omit) # convergence issues
Anova(Post1PUEmod2, type="III")
summary(Post1PUEmod2)
rsq(Post1PUEmod2) # 0.76
# lme mod
Post1PUEmod3 <- lme(PUE ~ Treatment*Soil, random=~1|Soil, data=Pots1, na.action=na.omit)
summary(Post1PUEmod3)
Anova(Post1PUEmod3, type="III")
rsq(Post1PUEmod3) # 0.77

#AIC and BIC values
Pots1PUE_modlist <- list(Post1PUEmod1, Post1PUEmod2, Post1PUEmod3)
AIC_values <- sapply(Pots1PUE_modlist, AIC)
BIC_values <- sapply(Pots1PUE_modlist, BIC)
(PUE_cAB <- data.frame(Model=c("Post1PUEmod1", "Post1PUEmod2", "Post1PUEmod3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 Post1PUEmod1  1222.8891   1296.932 - 76 df, singularity issues
#2 Post1PUEmod2   989.6497   1063.693 - 85.7 df, convergence issues
#3 Post1PUEmod3   989.6497   1055.638 - zero df, NAN's produced

(ModPots1em_PUE<- emmeans(Post1PUEmod2,~Treatment|Soil, infer=TRUE))
(ModPots1cld_PUE <- cld(ModPots1em_PUE, Letters=trimws(letters), reversed=TRUE, by="Soil", alpha=0.001))
write_xlsx(ModPots1cld_PUE, path="Pots1_PUE.xlsx")
##Developing visualizations
PUE_subVar <- ModPots1cld_PUE %>%
  filter(Treatment %in% Reduced_trtVar)
(PUE50kg <- ggplot(PUE_subVar, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+25),
              size=5, position=position_dodge(width=0.9))+
    labs(x=bquote(bold("Treatments at 25mg P kg"^-1)), y=bquote(bold("Phosphorus Use Efficiency (g g"^-1*")")))+
    scale_x_discrete(limits=as.character(Reduced_trtVar), labels=PotsRedTrtVar)+
    scale_y_continuous(limits =  c(0,630))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=18, face="bold", colour="black"),
          axis.text.y=element_text(size=18, face="bold", colour="black"),
          axis.title.x=element_text(size=22, face="bold", margin=margin(t=5)), 
          axis.title.y=element_text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.6,0.4,0.4,0.4), "cm")))
#Plotting constant biochar rates with var P rates
PUE_subCon <- ModPots1cld_PUE %>%
  filter(Treatment %in% Reduced_charcon)
(PUE10tha <- ggplot(PUE_subCon, aes(x=Treatment, y=emmean, fill=Soil)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), colour="black")+
    scale_fill_manual(values=c("Haverhill"="grey45", "Oxbow"="black"), labels=c("Haverhill", "Oxbow"))+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=ifelse(Soil == "Haverhill", trimws(.group), toupper(trimws(.group))), y=emmean+SE+25),
              size=5, position=position_dodge(width=0.9))+
    labs(x=bquote(bold("Treatments at 10t biochar ha"^-1)), y="") +
    scale_x_discrete(limits=as.character(Reduced_charcon),labels=PotsRedCarcon)+
    scale_y_continuous(limits =  c(0,630))+
    theme(legend.position="top", legend.justification="center", legend.key.size=unit(10,"mm"), 
          legend.title=element_text(size=24, face="bold"), legend.text=element_text(size=22),
          axis.text.x=element_text(angle=65, hjust=1, size=18, face="bold", colour="black"),
          axis.text.y=element_blank(), #text(size=18, face="bold", colour="black"),
          axis.title.x=element_text(size=22, face="bold", margin=margin(t=5)), 
          axis.title.y=element_blank(), #text(size=22, face="bold", margin=margin(r=15)),
          panel.background=element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black"),
          plot.margin = unit(c(0.6,0.4,0.4,0.4), "cm")))




## SOIL ANALYSIS ----
### Soil PO4 ----
(PotsPO4Stats <- SkewKur(Pots1, "PO4"))
      # skewness kurtosis
      # 1.070656  1.51419
shapiro.test(Pots1$PO4) # p=2.5e-08
hist(Pots1$PO4) # severe left skewed
leveneTest(PO4 ~ Treatment*Soil, data=Pots1)  # p=7.9e-07
# Transform data
leveneTest(log(PO4) ~ Treatment*Soil, data=Pots1)  # p=0.079
leveneTest(log10(PO4) ~ Treatment*Soil, data=Pots1)  #  p=0.079
leveneTest(sqrt(PO4) ~ Treatment*Soil, data=Pots1)  # p=0.0015
shapiro.test(log(Pots1$PO4)) # p=7.2e-07
shapiro.test(log10(Pots1$PO4)) # p=7.2e-07
shapiro.test(sqrt(Pots1$PO4)) # p=1.01e-06
hist((log(Pots1$PO4))) # slight left skew
hist((log10(Pots1$PO4))) # two peaks
hist((sqrt(Pots1$PO4))) #left skew
# lmer
Post1PO4mod1 <-lmer(PO4~Treatment*Soil+(1|Soil),data=Pots1) # convergence issues
Anova(Post1PO4mod1, type="III")
summary(Post1PO4mod1)
rsq(Post1PO4mod1) # 0.93
# lmer log transform 
Post1PO4mod2 <-lmer(log(PO4)~Treatment*Soil+(1|Soil),data=Pots1) # convergence issues
Anova(Post1PO4mod2, type="III")
rsq(Post1PO4mod2) # 0.96
#log10 lmer - results similar as for natural log
Post1PO4mod3 <-lmer(log10(PO4)~Treatment*Soil+(1|Soil),data=Pots1) #  convergence issues
Anova(Post1PO4mod3, type="III")
summary(Post1PO4mod3)
rsq(Post1PO4mod3) # 0.96
# lmer with sqrt transformation
Post1PO4mod4 <- lmer(sqrt(PO4)~Treatment*Soil+(1|Soil),data=Pots1) #  convergence issues
Anova(Post1PO4mod4, type="III")
summary(Post1PO4mod4)
rsq(Post1PO4mod4) # 0.95
# glmer with gamma distribution
Post1PO4mod5 <- glmer(PO4~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log")) # singularity issues
Anova(Post1PO4mod5, type="III")
summary(Post1PO4mod5)
rsq(Post1PO4mod5) # NA
# glmer using gamma distribution with log transformation
Post1PO4mod6 <- glmer(log(PO4)~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="log")) # singularity issues
Anova(Post1PO4mod6, type="III")
summary(Post1PO4mod6)
rsq.glmm(Post1PO4mod6) # NA
# glmm with log transform
Post1PO4mod7 <- glmmTMB(log(PO4)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # singularity issues
glmmTMB:::Anova.glmmTMB(Post1PO4mod7, type="III")
summary(Post1PO4mod7)
performance::r2(Post1PO4mod7) # NA

# run AIC/BIC to test for best model fit
Pots1modlist_PO4 <- list(Post1PO4mod1, Post1PO4mod2, Post1PO4mod3, Post1PO4mod4, Post1PO4mod5, Post1PO4mod6, Post1PO4mod7)
AIC_values <- sapply(Pots1modlist_PO4, AIC)
BIC_values <- sapply(Pots1modlist_PO4, BIC)
(Pots1PO4AB <- data.frame(Model=c("Post1PO4mod1", "Post1PO4mod2", "Post1PO4mod3", "Post1PO4mod4", "Post1PO4mod5", "Post1PO4mod6", "Post1PO4mod7"),
                    AIC_values, BIC_values))
# Model AIC_values BIC_values
#1 Post1PO4mod1  402.66828  483.40872 - df 290, more than samples
#2 Post1PO4mod2   30.02509  110.76553 - thousands of df
#3 Post1PO4mod3 -105.08816  -24.34772 - singularity issues, cannot run emmeans
#4 Post1PO4mod4   89.21990  169.96034 - NAN's produced
#5 Post1PO4mod5  323.23832  403.97875 - Infinite df
#6 Post1PO4mod6  -46.80830   33.93214 - Infinite df
#7 Post1PO4mod7  -63.77138   16.96906 - 79 df, singulairt issues but otherwise seems to work, best model

#Run emmeans
(ModPots1em_PO4 <- emmeans(Post1PO4mod7,~Treatment|Soil, type="response", infer=TRUE))
(ModPots1cld_PO4 <- cld(ModPots1em_PO4, Letters=trimws(letters), reversed=TRUE, by="Soil", type="response", alpha=0.001))
write_xlsx(ModPots1cld_PO4, path="Pots1_PO4.xlsx")


### Soil Resin P ----
(PotsResinPStats <- SkewKur(Pots1, "ResinP"))
      #skewness kurtosis
      #   2.2742 6.214694
shapiro.test(Pots1$ResinP) # p=9.6e-12
hist(Pots1$ResinP) #severe left skew
leveneTest(ResinP ~ Treatment*Soil, data=Pots1)  # 0.0033
# Transform data
leveneTest(log(ResinP) ~ Treatment*Soil, data=Pots1)  # 0.34
leveneTest(log10(ResinP) ~ Treatment*Soil, data=Pots1)  #  0.34
leveneTest(sqrt(ResinP) ~ Treatment*Soil, data=Pots1)  # 0.072
shapiro.test(log(Pots1$ResinP)) # p=0.0008
shapiro.test(log10(Pots1$ResinP)) # p=0.0008
shapiro.test(sqrt(Pots1$ResinP)) # p=1.6e-05
hist((log(Pots1$ResinP))) # slight right skew
hist((log10(Pots1$ResinP))) # almost centered
hist((sqrt(Pots1$ResinP))) # left skew
# glmm with log
Post1ResPmod1 <- glmmTMB(log10(ResinP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # singularity issues
glmmTMB:::Anova.glmmTMB(Post1ResPmod1, type="III", alpha=0.001)
summary(Post1ResPmod1)
performance::r2(Post1ResPmod1) # NA
# lmer log transform 
Post1ResPmod2 <-lmer(log(ResinP)~Treatment*Soil+(1|Soil),data=Pots1) # convergence issues
Anova(Post1ResPmod2, type="III", alpha=0.001)
rsq(Post1ResPmod2) # 0.81
# glmer - could not get model to work
# lme mod
Post1ResPmod3 <- lme(ResinP ~ Treatment*Soil, random=~1|Soil, data=Pots1, na.action=na.omit)
summary(Post1ResPmod3)
Anova(Post1ResPmod3, type="III", alpha=0.001)
rsq(Post1ResPmod3) # 0.84

#AIC and BIC values - this indicated that Mod10 was the best fit
POts1ResP_modlist <- list(Post1ResPmod1, Post1ResPmod2, Post1ResPmod3)
AIC_values <- sapply(POts1ResP_modlist, AIC)
BIC_values <- sapply(POts1ResP_modlist, BIC)
(Pots1ResPAB <- data.frame(Model=c("Post1ResPmod1", "Post1ResPmod2", "Post1ResPmod3"), AIC_values, BIC_values))
#  Model AIC_values BIC_values
#1 Post1ResPmod1  -74.05198   6.688455 - 79 df, only one that works with emmeans, chosen as best model even though there are singularity issues
#2 Post1ResPmod2  157.49864 238.239075 - NAN's produced, 0 df
#3 Post1ResPmod3 -170.06531 -98.2318391 - NAN's produced, 0 df

#run emmeans
(ModPots1em_ResP<- emmeans(Post1ResPmod1,~Treatment|Soil, type="response", infer=TRUE))
(ModPots1cld_ResP <- cld(ModPots1em_ResP, Letters=trimws(letters), reversed=TRUE, by="Soil", type="response", alpha=0.001))
write_xlsx(ModPots1cld_ResP, path="Pots1_ResinP.xlsx")



### Soil WaterSolP ----
(PotsWSPStats <- SkewKur(Pots1, "WaterSolP"))
      #skewness kurtosis
      # 2.134349  5.38913
shapiro.test(Pots1$WaterSolP) # p=2.6e-11
hist(Pots1$WaterSolP) # severe left skew
leveneTest(WaterSolP ~ Treatment*Soil, data=Pots1)  # p=1.1e-06
# Transform data
leveneTest(log(WaterSolP) ~ Treatment*Soil, data=Pots1)  # 0.041
leveneTest(log10(WaterSolP) ~ Treatment*Soil, data=Pots1)  # 0.041
leveneTest(sqrt(WaterSolP) ~ Treatment*Soil, data=Pots1)  # 0.0008
shapiro.test(log(Pots1$WaterSolP)) # p=0.00028
shapiro.test(log10(Pots1$WaterSolP)) # p=0.00028
shapiro.test(sqrt(Pots1$WaterSolP)) # p=6.1e-08
hist((log(Pots1$WaterSolP))) # almost centered
hist((log10(Pots1$WaterSolP))) # almost centered
hist((sqrt(Pots1$WaterSolP))) # left skew
# glmm with log transofrm
Post1WSPmod1 <- glmmTMB(log(WaterSolP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # singularity issues
glmmTMB:::Anova.glmmTMB(Post1WSPmod1, type="III")
summary(Post1WSPmod1)
performance::r2(Post1WSPmod1) # NA
# lmer log transform 
Post1WSPmod2 <-lmer(log(WaterSolP)~Treatment*Soil+(1|Soil),data=Pots1) 
Anova(Post1WSPmod2, type="III", alpha=0.001)
rsq(Post1WSPmod2) # 0.8
# lme log transform
Post1WSPmod3 <- lme(log(WaterSolP) ~ Treatment*Soil, random=~1|Soil, data=Pots1, na.action=na.omit)
Anova(Post1WSPmod3, type="III", alpha=0.001)
summary(Post1WSPmod3)
rsq(Post1WSPmod3) # 0.79

#AIC and BIC values - this indicated that Mod11 was the best fit
Pots1WSP_modlist <- list(Post1WSPmod1, Post1WSPmod2, Post1WSPmod3)
AIC_values <- sapply(Pots1WSP_modlist, AIC)
BIC_values <- sapply(Pots1WSP_modlist, BIC)
(Pots1WSPAB <- data.frame(Model=c("Post1WSPmod1", "Post1WSPmod2", "Post1WSPmod3"), AIC_values, BIC_values))
#  Model AIC_values BIC_values
#1 Post1WSPmod1  -41.79776   39.21665 - 80 df, singularity issues but preferred model
#2 Post1WSPmod2   46.44369  127.45810 - 0 df, NAN's produced
#3 Post1WSPmod3   46.44369  118.64526 - 0 df, NAN's produced

#run emmeans
(ModPots1em_WSP<- emmeans(Post1WSPmod1,~Treatment|Soil, type="response", infer=TRUE))
(ModPots1cld_WSP <- cld(ModPots1em_WSP, Letters=trimws(letters), reversed=TRUE, by="Soil", type="response", alpha=0.001))
write_xlsx(ModPots1cld_WSP, path = "Pots1_WaterSolP.xlsx")



### Soil pH ----
(PotspHStats <- SkewKur(Pots1, "pH"))
      # skewness   kurtosis
      # -0.6816352 -0.1221551
shapiro.test(Pots1$pH) # p=0.00067
hist(Pots1$pH) #  right skew
leveneTest(pH ~ Treatment*Soil, data=Pots1)  # 0.043
# Transform data - did not improve data, transformation not applied
shapiro.test(log(Pots1$pH)) # p=0.00043
shapiro.test(log10(Pots1$pH)) # p=0.00043
shapiro.test(sqrt(Pots1$pH)) # 0.00053
leveneTest(log(pH) ~ Treatment*Soil, data=Pots1)  # 0.039
leveneTest(log10(pH) ~ Treatment*Soil, data=Pots1)  #  0.039
leveneTest(sqrt(pH) ~ Treatment*Soil, data=Pots1)  # 0.041
hist((log(Pots1$pH))) # right skew
hist((log10(Pots1$pH))) # right skew
hist((sqrt(Pots1$pH))) # right skew
# lmer
Post1pHmod1 <- lmer(pH~Treatment*Soil+(1|Soil), data=Pots1) # convergence issues
print(vif(Post1pHmod1)) #check collinearity
Anova(Post1pHmod1, type="III")
summary(Post1pHmod1)
rsq(Post1pHmod1)  # 0.96
# glmer
Post1pHmod2 <- glmer(pH~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(link="log")) #singularity issues
print(vif(Post1pHmod2))
Anova(Post1pHmod2, type="III")
summary(Post1pHmod2)
rsq(Post1pHmod2) # 0.96
# glmm - convergence issues
Post1pHmod3 <- glmmTMB(pH~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # convergence issues
glmmTMB:::Anova.glmmTMB(Post1pHmod3, type="III")
print(vif(Post1pHmod3))
summary(Post1pHmod3)
performance::r2(Post1pHmod3) # 0.99, NAN's produced
# lme 
Post1pHmod4 <- lme(pH~Treatment*Soil,random=~1|Soil, data=Pots1, na.action=na.exclude)
Anova(Post1pHmod4, type="III")
summary(Post1pHmod4)
rsq.lmm(Post1pHmod4) # 0.96

#AIC and BIC values - this indicated that Mod13 was the best fit
Post1pH_modlist <- list(Post1pHmod1, Post1pHmod2, Post1pHmod3, Post1pHmod4)
AIC_values <- sapply(Post1pH_modlist, AIC)
BIC_values <- sapply(Post1pH_modlist, BIC)
(Pots1pHAB <- data.frame(Model=c("Post1pHmod1", "Post1pHmod2", "Post1pHmod3", "Post1pHmod4"), AIC_values, BIC_values))
#  Model AIC_values BIC_values
#1 Post1pHmod1  -284.6756  -203.1206 - df=0, NAN's produced
#2 Post1pHmod2  -481.5428  -399.9878 - df=Inf - only model that works in emmeans
#3 Post1pHmod3         NA         NA - df=82, negative variance estimates
#4 Post1pHmod4  -284.6756  -211.7511 - df=0, NAN's produced

#run emmeans
(ModPots1em_pH<- emmeans(Post1pHmod2,~Treatment|Soil, infer=TRUE, type="response"))
(ModPots1cld_pH <- cld(ModPots1em_pH, Letters=trimws(letters), reversed=TRUE, by="Soil", alpha=0.001))
write_xlsx(ModPots1cld_pH, path="Pots1_pH.xlsx")


### Soil EC ----
(PotsECStats <- SkewKur(Pots1, "EC"))
      #skewness kurtosis
      # 1.325329  1.46656
shapiro.test(Pots1$EC) # p=2.8e-08
hist(Pots1$EC) # left skew
leveneTest(EC ~ Treatment*Soil, data=Pots1)  # p=0.00015
# Transform data - log transofrmation applied
shapiro.test(log(Pots1$EC)) # p=3.9e-06
shapiro.test(log10(Pots1$EC)) # p=3.9e-06
shapiro.test(sqrt(Pots1$EC)) # p=3.4e-07
leveneTest(log(EC) ~ Treatment*Soil, data=Pots1)  # 0.008
leveneTest(log10(EC) ~ Treatment*Soil, data=Pots1)  # 0.008
leveneTest(sqrt(EC) ~ Treatment*Soil, data=Pots1)  # 0.0012
hist((log(Pots1$EC))) # left skew
hist((log10(Pots1$EC))) # left skew
hist((sqrt(Pots1$EC))) # left skew
# glmm
Post1ECmod1 <- glmmTMB(log(EC)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # singularity issues
glmmTMB:::Anova.glmmTMB(Post1ECmod1, type="III")
summary(Post1ECmod1)
performance::r2(Post1ECmod1) # NA
# lme 
Post1ECmod2 <- lme(log(EC)~Treatment*Soil,random=~1|Soil, data=Pots1, na.action=na.exclude)
Anova(Post1ECmod2, type="III")
summary(Post1ECmod2)
rsq.lmm(Post1ECmod2) # NA
# lmer
Post1ECmod3 <- lmer(log(EC)~Treatment*Soil+(1|Soil), data=Pots1) # convergence issues
Anova(Post1ECmod3, type="III")
summary(Post1ECmod3)
rsq(Post1ECmod3) # 0.85
# glmer
Post1ECmod4 <- glmer(log(EC)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(link="log")) #singularity issues
Anova(Post1ECmod4, type="III")
summary(Post1ECmod4)
rsq(Post1ECmod4) # NA

#AIC and BIC values 
Pots1EC_modlist <- list(Post1ECmod1, Post1ECmod2, Post1ECmod3, Post1ECmod4)
AIC_values <- sapply(Pots1EC_modlist, AIC)
BIC_values <- sapply(Pots1EC_modlist, BIC)
(Pots1ECAB <- data.frame(Model=c("Post1ECmod1", "Post1ECmod2", "Post1ECmod3", "Post1ECmod4"), AIC_values, BIC_values))
#  Model AIC_values BIC_values
#1 Post1ECmod1 -223.86268 -143.398745 - df=78, chosen as best model
#2 Post1ECmod2  -88.59477  -17.133973 - df=0, NAN's produced
#3 Post1ECmod3  -88.59477   -8.130836 - df=Inf, p=1??
#4 Post1ECmod4 -221.86268 -141.398745 - df=Inf

#run emmeans
(ModPots1em_EC<- emmeans(Post1ECmod1,~Treatment|Soil, infer=TRUE, type="response"))
(ModPots1cld_EC <- cld(ModPots1em_EC, Letters=trimws(letters), reversed=TRUE, by="Soil") )
write_xlsx(ModPots1cld_EC, path="Pots1_EC.xlsx")



### Soil Organic carbon ----
(PotsOCStats <- SkewKur(Pots1, "OC"))
      #skewness  kurtosis
      # 0.1402157 -0.890197
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
# glmer
Post1OCmod1 <- glmer(OC~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="identity"), na.action=na.exclude) #singularity
Anova(Post1OCmod1, type="III")
summary(Post1OCmod1)
rsq(Post1OCmod1) # NA
# glm
Post1OCmod2 <- glm(OC ~ Treatment*Soil, family=gaussian, data=Pots1)
Anova(Post1OCmod2, type="III")
summary(Post1OCmod2)
rsq(Post1OCmod2) # 0.93
# YJ transformation
Pots1OC_YJ <- yjPower(Pots1$OC, 0.5,jacobian.adjusted=TRUE) # convergende issues
Post1OCmod3 <- lmer(Pots1OC_YJ ~ Treatment*Soil + (1|Soil), data=Pots1, control=lmerControl(optCtrl=list(maxfun=100000)))
Anova(Post1OCmod3, type="III")
summary(Post1OCmod3)
rsq(Post1OCmod3) # 0.93
# glmm
Post1OCmod4 <- glmmTMB(OC~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude) # singularity issues
glmmTMB:::Anova.glmmTMB(Post1OCmod4, type="III")
summary(Post1OCmod4)
performance::r2(Post1OCmod4) # NA

#AIC and BIC values 
Pots1OC_modlist <- list(Post1OCmod1, Post1OCmod2, Post1OCmod3, Post1OCmod4)
AIC_values <- sapply(Pots1OC_modlist, AIC)
BIC_values <- sapply(Pots1OC_modlist, BIC)
(Pots1OCAB <- data.frame(Model=c("Post1OCmod1", "Post1OCmod2", "Post1OCmod3", "Post1OCmod4"), AIC_values, BIC_values))
#  Model AIC_values BIC_values
#1 Post1OCmod1  -65.71302   13.90580 - df=Inf
#2 Post1OCmod2  -57.04445   19.92040 - df=77, chosen as best model
#3 Post1OCmod3   31.54353  111.16234 - more df than samples
#4 Post1OCmod4  -55.04445   24.57436 - df=75

#run emmeans
(ModPots1em_OC<- emmeans(Post1OCmod2,~Treatment|Soil, infer=TRUE, type="response"))
(ModPots1cld_OC <- cld(ModPots1em_OC, Letters=trimws(letters), reversed=TRUE, by="Soil"))
write_xlsx(ModPots1cld_OC, path="Pots1_OC.xlsx")


## Correlate char P  ----
# set up new data frame with columns from original dataset
Pots1CharPdf <- data.frame(Soil=Pots1$Soil,
  Treatment=Pots1$Treatment,
  PO4=Pots1$PO4,
  CharPRate=Pots1$CharPRate,
  CharPerc=Pots1$CharPerc)
(Pots1CharPdf <- subset(Pots1CharPdf, Treatment != "Control"))
# set up a weighted correlation - weighing PO4 and % P in char by the total P added in each treatment
weighted_cor <- function(PO4, CharPerc, CharPRate) {
  Pots1PO4Mean <- sum(PO4 * CharPRate) / sum(CharPRate) # Calculate weighted means
  Pots1CharPercMean <- sum(CharPerc * CharPRate) / sum(CharPRate)
  dev_PO4 <- PO4 - Pots1PO4Mean # Calculate weighted deviations
  dev_CharPerc <- CharPerc - Pots1CharPercMean
  Pots1Char_WeightedCov <- sum(CharPRate * dev_PO4 * dev_CharPerc) / sum(CharPRate) # Calculate weighted covariance
  sd_PO4 <- sqrt(sum(CharPRate * dev_PO4^2) / sum(CharPRate)) # Calculate weighted standard deviations
  sd_CharPerc <- sqrt(sum(CharPRate * dev_CharPerc^2) / sum(CharPRate))
  correlation <- Pots1Char_WeightedCov / (sd_PO4 * sd_CharPerc) # Calculate weighted correlation
  return(correlation)
}
# get a single correlation value for each PO4/CharPerc combination per treatment using the weighting calculated above
Pots1CharCor <- Pots1CharPdf %>%
  group_by(Soil, Treatment) %>%
  filter(complete.cases(CharPerc, PO4, CharPRate)) %>%
  summarize(Correlation=weighted_cor(CharPerc, PO4, CharPRate), .groups="drop")
Pots1CharCor$Treatment <- factor(Pots1CharCor$Treatment, levels=Pots1Trt_sub, labels=PotsLabDash_sub)
View(Pots1CharCor)
# visualize correlation using heatmap
(Pots1CharHeatPlot <- ggplot(Pots1CharCor, aes(x=Treatment, y=Soil, fill=Correlation)) +
    geom_point(data=Pots1CharCor, aes(size=abs(Correlation)*20), shape=21) + #set size of correlation circles
    scale_size(range=c(15, 30)) +
    scale_fill_viridis(option = "magma", limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f", Correlation), color = ifelse(Correlation < 0, "white", "black")), size=5.5)+
    scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
    scale_y_discrete(limits=rev(as.character(c("Haverhill", "Oxbow"))))+ # use rev to change order of soils
    guides(size="none")+
    labs(x="", y="", title=expression(bold("Percentage P in treatment - Soil residual PO"[4])))+
    theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
          legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold"),
          legend.key.siz=unit(15,"mm"),
          axis.title=element_blank(),
          axis.text.y=element_text(size=16, colour="black", face="bold"),
          axis.text.x=element_blank(), axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.spacing.x=unit(1, "cm"), plot.margin=margin(15,5,5,10)))

##  correlate P recovery to to %P in char
Pots1CharRecdf <- data.frame(Soil=Pots1$Soil, Treatment=Pots1$Treatment, Precovery=Pots1$Precovery,
                 CharPRate=Pots1$CharPRate, CharPerc=Pots1$CharPerc)
(Pots1CharRecdf <- subset(Pots1CharRecdf, Treatment != "Control"))
Pots1CharRecWght_cor <- function(Precovery, CharPerc, CharPRate) {
  Pots1PrecMean <- sum(Precovery * CharPRate) / sum(CharPRate)
  Pots1CharPercMean <- sum(CharPerc * CharPRate) / sum(CharPRate)
  dev_Prec <- Precovery - Pots1PrecMean
  dev_CharPerc <- CharPerc - Pots1CharPercMean
  Pots1Prec_WeightedCov <- sum(CharPRate * dev_Prec * dev_CharPerc) / sum(CharPRate)
  sd_Prec <- sqrt(sum(CharPRate * dev_Prec^2) / sum(CharPRate))
  sd_CharPerc <- sqrt(sum(CharPRate * dev_CharPerc^2) / sum(CharPRate))
  correlation <- Pots1Prec_WeightedCov / (sd_Prec * sd_CharPerc)
  return(correlation)
}
Pots1PrecCor <- Pots1CharRecdf %>%
  group_by(Soil, Treatment) %>%
  filter(complete.cases(CharPerc, Precovery, CharPRate)) %>%
  summarize(Correlation=Pots1CharRecWght_cor(CharPerc, Precovery, CharPRate), .groups="drop")
Pots1PrecCor$Treatment <- factor(Pots1PrecCor$Treatment, levels=Pots1Trt_sub, labels=PotsLabDash_sub)
print(Pots1PrecCor)
# visualize correlation using heatmap
(Pots1CharPrecPlot <- ggplot(Pots1PrecCor, aes(x=Treatment, y=Soil, fill=Correlation)) +
    geom_point(data=Pots1PrecCor, aes(size=abs(Correlation)*20), shape=21) +
    scale_size(range=c(15, 30)) +
    scale_fill_viridis(option = "magma", limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f", Correlation), color = ifelse(Correlation < 0, "white", "black")), size=5.5)+
    scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
    scale_y_discrete(limits=rev(as.character(c("Haverhill", "Oxbow"))))+
    guides(size="none")+
    labs(x="", y="", title="Percentage P in treatment - Percentage P Recovery")+
    theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
          legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold"),
          legend.key.siz=unit(15,"mm"),
          axis.title=element_blank(),
          axis.text.y=element_text(size=16, colour="black", face="bold"),
          axis.text.x=element_blank(), axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.spacing.x=unit(1, "cm"), plot.margin=margin(5,5,5,10)))

## correlate P recovery to residual PO4
Pots1PO4Recdf <- data.frame(Soil=Pots1$Soil, Treatment=Pots1$Treatment, Precovery=Pots1$Precovery,
                             PO4=Pots1$PO4, CharPRate=Pots1$CharPRate)
(Pots1PO4Recdf <- subset(Pots1PO4Recdf, Treatment != "Control"))
Pots1PO4RecWght_cor <- function(Precovery, PO4, CharPRate) {
  Pots1PrecMean <- sum(Precovery * CharPRate) / sum(CharPRate)
  Pots1PO4Mean <- sum(PO4 * CharPRate) / sum(CharPRate)
  dev_Prec <- Precovery - Pots1PrecMean
  dev_PO4 <- PO4 - Pots1PO4Mean
  Pots1PrecPO4_WghtCov <- sum(CharPRate * dev_Prec * dev_PO4) / sum(CharPRate)
  sd_Prec <- sqrt(sum(CharPRate * dev_Prec^2) / sum(CharPRate))
  sd_PO4 <- sqrt(sum(CharPRate * dev_PO4^2) / sum(CharPRate))
  correlation <- Pots1PrecPO4_WghtCov / (sd_Prec * sd_PO4)
  return(correlation)
}
Pots1PrecPO4Cor <- Pots1PO4Recdf %>%
  group_by(Soil, Treatment) %>%
  filter(complete.cases(Precovery, PO4, CharPRate)) %>%
  summarize(Correlation=Pots1PO4RecWght_cor(Precovery, PO4, CharPRate), .groups="drop")
Pots1PrecPO4Cor$Treatment <- factor(Pots1PrecPO4Cor$Treatment, levels=Pots1Trt_sub, labels=PotsLabDash_sub)
print(Pots1PrecPO4Cor)
# visualize correlation using heatmap
(Pots1PO4PrecPlot <- ggplot(Pots1PrecPO4Cor, aes(x=Treatment, y=Soil, fill=Correlation)) +
    geom_point(data=Pots1PrecPO4Cor, aes(size=abs(Correlation)*20), shape=21) +
    scale_size(range=c(15, 30)) +
    scale_fill_viridis(option = "magma", limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f", Correlation), color = ifelse(Correlation < 0, "white", "black")), size=5.5)+
    scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
    scale_y_discrete(limits=rev(as.character(c("Haverhill", "Oxbow"))))+
    scale_x_discrete(labels=(c("Canola Meal<br>50kg P ha<sup>-1</sup>", "Canola Hull<br>50kg P ha<sup>-1</sup>", 
                               "Manure<br>50kg P ha<sup>-1</sup>", "Willow<br>50kg P ha<sup>-1</sup>", "Canola Meal<br>10t ha<sup>-1</sup>",
                               "Canola Hull<br>10t ha<sup>-1</sup>", "Manure<br>10t h<sup>-1</sup>", "Willow<br>10t ha<sup>-1</sup>",
                               "Canola Meal<br>10t ha<sup>-1</sup> & TSP", "Canola Hull<br>10t ha<sup>-1</sup> & TSP",
                               "Manure<br>10t ha<sup>-1</sup> & TSP", "Willow<br>10t ha<sup>-1</sup> & TSP", "TSP<br>Fertilizer")))+
    guides(size="none")+
    labs(x="", y="", title=expression(bold("Percentage P Recovery - Soil residual PO"[4])))+
    theme(plot.title=element_text(hjust=0.5, face="bold", size=20),
          legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold"),
          legend.key.siz=unit(15,"mm"),
          axis.title=element_blank(),
          axis.text.y=element_text(size=16, colour="black", face="bold"),
          axis.text.x=element_markdown(angle=45, size=16, colour="black", hjust=0.95, face="bold"),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.spacing.x=unit(1, "cm"), plot.margin=margin(5,5,5,10)))

## combined plot - combined legend and ggarrange uses ggpubr package, set legend in individual plots
(CharRecPO4_plot <-ggarrange(Pots1CharHeatPlot, Pots1CharPrecPlot, Pots1PO4PrecPlot, nrow=3, heights = c(0.5,0.5,0.7),
                             common.legend=TRUE, legend="right"))
PotsCharAnnotate <- annotate_figure(CharRecPO4_plot, top=text_grob("Controlled environment study with various biochar", size=24, face="bold"))
ggsave(PotsCharAnnotate, file="Pots1_CharPO4Prec_combined.jpg", width=15, height=10, dpi=150)




# FIELD STUDY ----
## Ordering of data   ----
#Change columns in a dataframe to factors/categorical values, str displays 
Field$Block <- factor(Field$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
FieldTrt_order <- as.factor(c("Control", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"))
FieldTrtSub <- as.factor(c("Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"))

# for labels
FieldLab_Main <- as.factor(c("Control","Biochar 25kg P/ha", "Biochar 10t/ha", "Biochar 10t/ha & TSP", "TSP Fertilizer"))
FieldLab_Sub <- as.factor(c("Biochar 25kg P/ha", "Biochar 10t/ha", "Biochar 10t/ha & TSP", "TSP Fertilizer"))
FieldLabDash_Main <- as.factor(c("Control","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP", "TSP\nFertilizer"))
FieldLabDash_sub <- as.factor(c("Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP", "TSP\nFertilizer"))


## PLANT ANALYSIS ----  
### Straw   ----
(FieldStrawStats <- SkewKur(Field, "FStraw"))
    #skewness   kurtosis
    # 0.9853444 -0.1381437
ggplot(Field, aes(x=Treatment, y=FStraw, fill=Treatment)) + geom_boxplot() + labs(x="Treatment", y="Straw")
hist(Field$FStraw) #  left skew
shapiro.test(Field$FStraw) # p=0.01
leveneTest(FStraw~Treatment, data=Field)  # P=0.0072
# transform
hist(log(Field$FStraw)) # slight left skew
shapiro.test(log(Field$FStraw)) # p=0.065
leveneTest(log(FStraw)~Treatment, data=Field)  # P=0.022
# ModFStraw1
ModFStraw1<- lmer(log(FStraw)~Treatment+(1|Block),data=Field)
Anova(ModFStraw1, type="III") # no significant differences
summary(ModFStraw1)
rsq(ModFStraw1) # 0.37
# ModFStraw2
ModFStraw2 <- glmer(log(FStraw)~Treatment+(1|Block),data=Field,family=Gamma(link="log"), na.action=na.omit)
Anova(ModFStraw2, type="III") # no significant differences
summary(ModFStraw2)
rsq.glmm(ModFStraw2) # NA
# ModFStraw3
ModFStraw3 <- lme(log(FStraw)~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
Anova(ModFStraw3, type="III")  # no significant differences
summary(ModFStraw3)
rsq.lmm(ModFStraw3) # NA
# ModFStraw4
ModFStraw4 <- glmmTMB(log(FStraw)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModFStraw4, type="III") # no significant differences
summary(ModFStraw4)
performance::r2(ModFStraw4) # 0.47

# AIC & BIC
Fstraw_modlist <- list(ModFStraw1, ModFStraw2, ModFStraw3, ModFStraw4)
AIC_values <- sapply(Fstraw_modlist, AIC)
BIC_values <- sapply(Fstraw_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFStraw1", "ModFStraw2", "ModFStraw3", "ModFStraw4"), AIC_values, BIC_values))
#       Model AIC_values BIC_values
#1 ModFStraw1  17.173158  23.784231 - df=8.76
#2 ModFStraw2   1.519824   8.130897 - df=Inf
#3 ModFStraw3  17.173158  21.646560 - df=3
#4 ModFStraw4   3.441637  10.052710 - df=12, no issues, chosen as best model

# emmeans 
(ModFStrawem <- emmeans(ModFStraw4,~Treatment, infer=TRUE, type="response"))
(ModFStrawem_cld <- cld(ModFStrawem, Letters=trimws(letters), reversed=TRUE, alpha=0.1, type="response"))
ModFStrawem_cld <- ModFStrawem_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFStrawem_cld, path="Field_Straw.xlsx")



### Grain   ----
(FieldGrainStats <- SkewKur(Field, "FGrain"))
        #skewness kurtosis
        # 1.761797 2.774306
shapiro.test(Field$FGrain) # p=0.0006
hist(Field$FGrain) # severe left skew
leveneTest(FGrain~Treatment, data=Field) # 0.02
# transform
shapiro.test(log(Field$FGrain)) # p=0.18
shapiro.test(sqrt(Field$FGrain)) # p=0.01
hist(log(Field$FGrain)) # severe left skew
hist(sqrt(Field$FGrain)) # severe left skew
leveneTest(log(FGrain)~Treatment, data=Field) # 0.03
leveneTest(sqrt(FGrain)~Treatment, data=Field) # 0.02
#ModFGrain1 
ModFGrain1<- lmer(log(FGrain)~Treatment+(1|Block),data=Field)
Anova(ModFGrain1, type="III") # no significant differences
summary(ModFGrain1)
rsq(ModFGrain1) # 0.47
# ModFGrain2
ModFGrain2 <- glmer(log(FGrain)~Treatment+(1|Block),data=Field,family=Gamma(link="log"), na.action=na.omit)
Anova(ModFGrain2, type="III") # no significant differences
summary(ModFGrain2)
rsq(ModFGrain2) # NA
# ModFGrain3
ModFGrain3 <- lme(log(FGrain)~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
Anova(ModFGrain3, type="III")  # no significant differences
summary(ModFGrain3)
rsq.lmm(ModFGrain3) # NA
# ModFGrain4
ModFGrain4 <- glmmTMB(log(FGrain)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModFGrain4, type="III") # no significant differences
summary(ModFGrain4)
performance::r2(ModFGrain4) # 0.55

# AIC & BIC
FGrain_modlist <- list(ModFGrain1, ModFGrain2, ModFGrain3, ModFGrain4)
AIC_values <- sapply(FGrain_modlist, AIC)
BIC_values <- sapply(FGrain_modlist, BIC)
(FgrainAB <- data.frame(Model=c("ModFGrain1", "ModFGrain2", "ModFGrain3", "ModFGrain4"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFGrain1   34.04656   40.65763
#2 ModFGrain2   23.12671   29.73778
#3 ModFGrain3   34.04656   38.51996
#4 ModFGrain4   26.32623   32.93730 - df=12

#emmeans 
(ModFGrainem <- emmeans(ModFGrain4,~Treatment, infer=TRUE, type="response"))
(ModFGrainem_cld <- cld(ModFGrainem, Letters=trimws(letters), reversed=TRUE, type="response", alpha=0.1))
ModFGrainem_cld <- ModFGrainem_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFGrainem_cld, path="Field_Grain.xlsx")


### Biomass   ----
(FieldYieldStats <- SkewKur(Field, "Yield"))
      #skewness  kurtosis
      #  1.24986 0.6731453
shapiro.test(Field$Yield) # p=0.006
hist(Field$Yield) # severe left skew
leveneTest(Yield~Treatment, data=Field) # 0.004
# transform
shapiro.test(log(Field$Yield)) # p=0.08
shapiro.test(sqrt(Field$Yield)) # p=0.02
hist(log(Field$Yield)) # mild left skew
hist(sqrt(Field$Yield)) # left skew
leveneTest(log(Yield)~Treatment, data=Field) # 0.02
leveneTest(sqrt(Yield)~Treatment, data=Field) # 0.008
#ModFYield1 
ModFYield1<- lmer(log(Yield)~Treatment+(1|Block),data=Field)
Anova(ModFYield1, type="III") # no significant differences
summary(ModFYield1)
rsq(ModFYield1) # 0.42
# ModFYield2
ModFYield2 <- glmer(log(Yield)~Treatment+(1|Block), data=Field, family=gaussian(link="log"), na.action=na.omit)
Anova(ModFYield2, type="III") # no significant differences
summary(ModFYield2)
rsq(ModFYield2) # NA
# ModFYield3
ModFYield3 <- lme(log(Yield)~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
Anova(ModFYield3, type="III")  # no significant differences
summary(ModFYield3)
rsq.lmm(ModFYield3) # NA
# ModFYield4
ModFYield4 <- glmmTMB(log(Yield)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModFYield4, type="III") # no significant differences
summary(ModFYield4)
performance::r2(ModFYield4) # 0.51

# AIC & BIC
FYield_modlist <- list(ModFYield1, ModFYield2, ModFYield3, ModFYield4)
AIC_values <- sapply(FYield_modlist, AIC)
BIC_values <- sapply(FYield_modlist, BIC)
(FieldAB <- data.frame(Model=c("ModFYield1", "ModFYield2", "ModFYield3", "ModFYield4"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFYield1  20.001628   26.61270 - df=8.13
#2 ModFYield2   5.265376   11.87645 - df=Inf
#3 ModFYield3  20.001628   24.47503 - df=3
#4 ModFYield4   7.273410   13.88448 - df=12, chosen as best model

# emmeans 
(ModFYieldem <- emmeans(ModFYield4,~Treatment, infer=TRUE, type="response"))
(ModFYieldem_cld <- cld(ModFYieldem, Letters=trimws(letters), reversed=TRUE, type="response", alpha=0.1))
ModFYieldem_cld <- ModFYieldem_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFYieldem_cld, path="Field_Yield.xlsx")

# Plot option 1 - using grain and straw emmeans
## Create combined dataframe
ModFGrainem_cld$origin <- "FGrain"
ModFStrawem_cld$origin <- "FStraw"
BiomassFieldEm <- rbind(ModFGrainem_cld,ModFStrawem_cld)
BiomassFieldEm <- as.data.frame(BiomassFieldEm)
BiomassFieldEm <-  BiomassFieldEm[, c("Treatment", "emmean", "SE", ".group", "origin")]
BiomassFieldEm$.group <- str_trim(BiomassFieldEm$.group)
BiomassFieldEm$Treatment <- factor(BiomassFieldEm$Treatment, levels=FieldTrt_order)
BiomassFieldEm <- BiomassFieldEm[order(BiomassFieldEm$Treatment), ]
### Set confidence intervals
for (i in 1:nrow(BiomassFieldEm)) {
  if (BiomassFieldEm[i, 'origin'] == "FGrain") {
    treatment <- BiomassFieldEm[i, 'Treatment']
    fstraw_emmean <- BiomassFieldEm[BiomassFieldEm$Treatment == treatment & BiomassFieldEm$origin == "FStraw", 'emmean']
    BiomassFieldEm[i, 'u_conf'] <- BiomassFieldEm[i, 'emmean'] + sum(fstraw_emmean) + BiomassFieldEm[i, 'SE']
    BiomassFieldEm[i, 'l_conf'] <- BiomassFieldEm[i, 'emmean'] + sum(fstraw_emmean) - BiomassFieldEm[i, 'SE']
  } else if (BiomassFieldEm[i, 'origin'] == "FStraw") {
    BiomassFieldEm[i, 'u_conf'] <- BiomassFieldEm[i, 'emmean'] + BiomassFieldEm[i, 'SE']
    BiomassFieldEm[i, 'l_conf'] <- BiomassFieldEm[i, 'emmean'] - BiomassFieldEm[i, 'SE']
  }
}
print(BiomassFieldEm)
write_xlsx(BiomassFieldEm, path="Field_BiomassMeans.xlsx")
## Visualization
### Set text labels colours & fontface
YieldColour <- c("FGrain" = "white", "FStraw" = "black")
TotLetMax <- aggregate(u_conf ~ Treatment, data = BiomassFieldEm, FUN = max)
(FieldYield1 <- ggplot(BiomassFieldEm, aes(Treatment, y=emmean, fill=origin)) +
    geom_bar(stat="identity", position = "stack", width=0.65, col="black") +
    geom_errorbar(aes(ymin=l_conf, ymax=u_conf), width = .15, stat="identity")+
    geom_text(aes(label = trimws(.group), y = ifelse(origin == "FStraw", emmean, emmean), color = "white"), #color specified within aes layer
              position = position_stack(vjust = 0.5), hjust = -0.65, size = 6, fontface="bold") +
    geom_text(data=TotLetMax, (aes(x=Treatment, y = u_conf+150, label = "A")),
              fontface="bold", color="black", size = 6, inherit.aes = FALSE) +
    labs(x = "Treatment", y = "Grain and straw yields (kg/ha)") +
    scale_fill_manual(values = c("grey55", "grey25"), labels = c("Grain", "Straw"))+
    scale_color_manual(values = "white", guide="none") + # need to specify geom_text colur and guide removes associated legend
    scale_x_discrete(labels = FieldLabDash_Main)+
    theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
          legend.title = element_blank(), legend.text=element_text(size=18, face="bold"),
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=14, face="bold", colour="black"),
          axis.text.y=element_text(size=14, face="bold", colour="black"),
          axis.title.x=element_blank(), 
          axis.title=element_text(size=17, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))



### P uptake  ----
(FieldPupStats <- SkewKur(Field, "Puptake"))
      #skewness kurtosis
      # 0.1565875 -1.35454
shapiro.test(Field$Puptake) # p=0.39
hist(Field$Puptake) # Normalish
leveneTest(Puptake~Treatment, data=Field)  # 0.44
#ModFieldPup1
ModFieldPup1 <- glmmTMB(Puptake~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModFieldPup1, type="III") # no significant differences
summary(ModFieldPup1)
performance::r2(ModFieldPup1) # 0.44
# ModFieldPup2
ModFieldPup2 <- glmer(Puptake~Treatment+(1|Block),data=Field,family=gaussian(link="log"), na.action=na.omit)
Anova(ModFieldPup2, type="III") # no significant differences in block
summary(ModFieldPup2)
rsq(ModFieldPup2) #  0.51
# ModFieldPup3
ModFieldPup3 <- lmer(Puptake~Treatment+(1|Block),data=Field)
Anova(ModFieldPup3, type="III") # no significant differences
summary(ModFieldPup3)
rsq::rsq(ModFieldPup3) # 0.36
# ModFieldPup4
ModFieldPup4 <- lme(Puptake~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
Anova(ModFieldPup4, type="III")  # no significant differences
summary(ModFieldPup4)
rsq.lmm(ModFieldPup4) #0.36

# AIC & BIC
FPup_modlist <- list(ModFieldPup1, ModFieldPup2, ModFieldPup3, ModFieldPup4)
AIC_values <- sapply(FPup_modlist, AIC)
BIC_values <- sapply(FPup_modlist, BIC)
(FPupAB <- data.frame(Model=c("ModFieldPup1", "ModFieldPup2", "ModFieldPup3", "ModFieldPup4"), AIC_values, BIC_values))
# Model AIC_values BIC_values
#1 ModFieldPup1   76.46114   83.43126 - df=13
#2 ModFieldPup2   78.44409   85.41421 - df=Inf, best rsq, chosen as best model
#3 ModFieldPup3   72.09256   79.06268 - df=9.37
#4 ModFieldPup4   72.09256   77.04891 - df=3

#emmeans
(ModFieldPupem <- emmeans(ModFieldPup2,~Treatment, infer= TRUE, type="response"))
(ModFieldPupem_cld <- cld(ModFieldPupem, Letters=letters, reversed=TRUE, alpha=0.1))
ModFieldPupem_cld <- ModFieldPupem_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFieldPupem_cld, path="Field_Puptake.xlsx")
# Visualizations
(FieldPuptakePlot <- ggplot(ModFieldPupem_cld, aes(x=Treatment, y=emmean, fill=Treatment)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), fill="grey45", width=0.65)+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1) +
    labs(x="Treatments", y="Canola phosphorus uptake (mg/kg)") +
    scale_x_discrete(limits=c(FieldTrt_order), labels=FieldLabDash_Main)+
    scale_y_continuous(limits=c(0, 10))+
    theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
          legend.title = element_blank(), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))


### P recovery   ----
(FieldPrecStats <- SkewKur(Field, "Precovery"))
      #skewness  kurtosis
      # 0.9029246 0.8231659
shapiro.test(Field$Precovery) # p=0.26
hist(Field$Precovery) # normal
leveneTest(Precovery~Treatment, data=Field)  # 0.15
# glmm
ModFieldPrec1 <- glmmTMB(Precovery~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModFieldPrec1, type="III") # no significant differences
summary(ModFieldPrec1)
performance::r2(ModFieldPrec1) # 0.26
# glmer
ModFieldPrec2 <- glmer(Precovery~Treatment+(1|Block),data=Field,family=gaussian(link="identity"), na.action=na.omit)
Anova(ModFieldPrec2, type="III") # no significant differences in block
summary(ModFieldPrec2)
rsq(ModFieldPrec2) #  0.21
# lmer
ModFieldPrec3 <- lmer(Precovery~Treatment+(1|Block),data=Field)
Anova(ModFieldPrec3, type="III") # no significant differences
summary(ModFieldPrec3)
rsq::rsq(ModFieldPrec3) # 0.21
# lme
ModFieldPrec4 <- lme(Precovery~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
Anova(ModFieldPrec4, type="III")  # no significant differences
summary(ModFieldPrec4)
rsq.lmm(ModFieldPrec4) # NA

# AIC & BIC
FPrec_modlist <- list(ModFieldPrec1, ModFieldPrec2, ModFieldPrec3, ModFieldPrec4)
AIC_values <- sapply(FPrec_modlist, AIC)
BIC_values <- sapply(FPrec_modlist, BIC)
(FPrec_AB <- data.frame(Model=c("ModFieldPrec1", "ModFieldPrec2", "ModFieldPrec3", "ModFieldPrec4"), AIC_values, BIC_values))
# Model AIC_values BIC_values
#1 ModFieldPrec1  102.80613  107.05443 - df=9, highest rsq, chosen as best model
#2 ModFieldPrec2   87.27252   91.52082 - df=9.79
#3 ModFieldPrec3   87.27252   91.52082 - df=9.79
#4 ModFieldPrec4   87.27252   89.65989 - df=3

#emmeans 
(ModFieldPrecem <- emmeans(ModFieldPrec1,~Treatment, infer = TRUE))
(ModFieldPrecem_cld <- cld(ModFieldPrecem, Letters=letters, type="response", alpha=0.1))
write_xlsx(ModFieldPrecem_cld, path="Field_Precovery.xlsx")
# Visualizations
(FieldPrecPlot <- ggplot(ModFieldPrecem_cld, aes(x=Treatment, y=emmean, fill=Treatment)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), fill="grey45", width = 0.65)+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE), size=6, vjust=-1, fontface="bold") +
    labs(x="Treatments", y="Phosphorus recovery (%)") +
    scale_x_discrete(limits=FieldTrtSub, labels=FieldLabDash_sub)+
    scale_y_continuous(limits = c(-4,5))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=14, face="bold", colour="black"),
          axis.text.y=element_text(size=14, face="bold", colour="black"),
          axis.title.x=element_blank(), 
          axis.title=element_text(size=17, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Phosphorus Use Efficiency ----
Field$PUE <- as.numeric(Field$PUE)
shapiro.test(Field$PUE)  #p=0.03
hist(Field$PUE) # slight left skew
leveneTest(PUE ~ Treatment, data=Field)  # p=0.716
FPUEmod1 <- glmmTMB(PUE~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
glmmTMB:::Anova.glmmTMB(FPUEmod1, type="III") # significant differences
summary(FPUEmod1)
performance::r2(FPUEmod1) # 0.59
shapiro.test(resid(FPUEmod1)) # p= 0.032
plot(fitted(FPUEmod1),resid(FPUEmod1),pch=16) # normal
qqnorm(resid(FPUEmod1)) # S shaped accross line
qqline(resid(FPUEmod1))
(FPUEmodEm<- emmeans(FPUEmod1,~Treatment, subset=(Field$PUE), infer = TRUE))
(FPUEmod_cld <- cld(FPUEmodEm, Letters=trimws(letters), reversed=TRUE, alpha=0.1))
write_xlsx(FPUEmod_cld, path="Field_PUE.xlsx")
# Visualizations
(FieldPUE_plot <- ggplot(FPUEmod_cld, aes(x=Treatment, y=emmean, fill=Treatment)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), fill="grey45", width = 0.65)+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE+4), size=6, fontface="bold") +
    labs(x="Treatments", y="Phosphorus Use Efficiency (kg/kg)") +
    scale_x_discrete(limits=FieldTrtSub, labels=FieldLabDash_sub)+
    scale_y_continuous(limits = c(0,75))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=14, face="bold", colour="black"),
          axis.text.y=element_text(size=14, face="bold", colour="black"),
          axis.title.x=element_blank(), 
          axis.title=element_text(size=17, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Combine plant plots ----
(FieldPlant_plots <- ggarrange(FieldYield1, FieldPrecPlot,FieldPUE_plot, ncol=3, labels="AUTO", 
                               font.label = list(size=26, face="bold"), label.x = c(0.18, 0.15, 0.15)))
annotate_figure(FieldPlant_plots, bottom=text_grob("Treatment", size=20, face = "bold"))
ggsave(file="FieldPlant_plots.jpg", height=6, width=9, dpi=150)


## SOIL ANALYSIS ----
### Soil PO4   ----
# Calculating skewness and kurtosis
PO4_VarNames <- c("PO4_10", "PO4_20", "PO4_30")
(PO4_stats_all <- SkewKur(Field, PO4_VarNames))
      #PO4_10_skewness PO4_20_skewness PO4_30_skewness PO4_10_kurtosis PO4_20_kurtosis PO4_30_kurtosis
      #1       -0.4909716       0.1530266       0.1592975       -1.135886       -1.443209       -1.415583
# Normality and equality of variance
shapiro.test(Field$PO4_10) # p=0.11
hist(Field$PO4_10) #  normal
leveneTest(PO4_10~Treatment, data=Field)  # P=0.026
shapiro.test(Field$PO4_20) # p=0.13
hist(Field$PO4_20) #  slight left skew
leveneTest(PO4_20~Treatment, data=Field)  # P=0.019
shapiro.test(Field$PO4_30) # p=0.28
hist(Field$PO4_30) #  slight left skew
leveneTest(PO4_30~Treatment, data=Field)  # P=0.067
# Change data to long format
PO4_long <- Field |>
  gather(key="Depth", value="PO4", matches("^PO4_"))
print(PO4_long)
#ModFieldSPO41
ModFieldSPO41 <-  lmer(PO4~Treatment*Depth + (1|Block), data=PO4_long)
Anova(ModFieldSPO41, tyoe="III", alpha=0.1) # significant differences
summary(ModFieldSPO41, alpha=0.1)
print(coef(summary(ModFieldSPO41))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
rsq(ModFieldSPO41) # 0.95
# ModFieldSPO42 - model does not fit well 
ModFieldSPO42 <- glmer(PO4~Treatment*Depth+(1|Block),data=PO4_long,family=gaussian(link="log"), na.action=na.omit)
Anova(ModFieldSPO42, type = "III") # significant differences
summary(ModFieldSPO42)
rsq(ModFieldSPO42)  # NA
# ModFieldSPO43
ModFieldSPO43 <- glmmTMB(PO4~Treatment*Depth+(1|Block), data=PO4_long, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(ModFieldSPO43, type="III") # significant differences
summary(ModFieldSPO43)
performance::r2(ModFieldSPO43) # 0.95
# ModFieldSPO44
ModFieldSPO44 <- lme(PO4~Treatment*Depth,random=~1|Block, data=PO4_long, na.action=na.exclude)
Anova(ModFieldSPO44, type="III")  # significant differences
summary(ModFieldSPO44)
rsq(ModFieldSPO44) # NA

# AIC & BIC
FPO4_modlist <- list(ModFieldSPO41, ModFieldSPO42, ModFieldSPO43, ModFieldSPO44)
AIC_values <- sapply(FPO4_modlist, AIC)
BIC_values <- sapply(FPO4_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFieldSPO41", "ModFieldSPO42", "ModFieldSPO43", "ModFieldSPO44"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldSPO41   219.8146   254.2456 - fractional df
#2 ModFieldSPO42   228.4649   262.8959 - Inf df - chosen as best model
#3 ModFieldSPO43   243.4177   277.8487 - more df than samples
#4 ModFieldSPO44   219.8146   248.9453 - 3 df

#emmeans 
(ModFieldemSPO4 <- emmeans(ModFieldSPO44,~Treatment|Depth, infer=TRUE))
(ModFieldemSPO4_cld <- cld(ModFieldemSPO4,Letters=trimws(letters), reversed=TRUE, by = "Depth", alpha=0.1))
ModFieldemSPO4_cld$Depth <- factor(ModFieldemSPO4_cld$Depth,
                                   levels = c("PO4_10", "PO4_20", "PO4_30"),
                                   labels = c("0-10cm", "10-20cm", "20-30cm"))
write_xlsx(ModFieldemSPO4_cld, path="Field_SoilPO4.xlsx")
# Visualizations
(FieldPO4Plot <- ggplot(ModFieldemSPO4_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
    geom_point(size = 8, color="black", aes(fill=Depth)) +
    scale_fill_manual(values = c("grey10", "grey45", "grey80"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label=trimws(.group)), size = 7, nudge_x = 0.45, check_overlap = TRUE)+
    scale_x_discrete(labels=FieldLabDash_Main)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'))+
    labs(x = "", y = bquote(bold("MK-P (mg/kg)"))) +
    theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=16),
          axis.text.x=element_blank(),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=24, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Soil Water soluble P   ----
# Skewness and kurtosis
WSP_VarNames <- c("WatSolP_10", "WatSolP_20", "WatSolP_30")
(WSP_stats_all <- SkewKur(Field, WSP_VarNames))
      #WatSolP_10_skewness WatSolP_20_skewness WatSolP_30_skewness WatSolP_10_kurtosis WatSolP_20_kurtosis WatSolP_30_kurtosis
      #1          0.2822584           0.6910876            1.664705          -0.6004822          -0.3304243            2.795134
# Normality and equality of variance
shapiro.test(Field$WatSolP_10) # p=0.85
hist(Field$WatSolP_10) #  normal
leveneTest(WatSolP_10~Treatment, data=Field)  # P=0.79
shapiro.test(Field$WatSolP_20) # p=0.15
hist(Field$WatSolP_20) #  left skew
leveneTest(WatSolP_20~as.factor(Treatment), data=Field)  # P=7.5e-5
shapiro.test(Field$WatSolP_30) # p=0.00059
hist(Field$WatSolP_30) #  severe left skew
leveneTest(WatSolP_30~as.factor(Treatment), data=Field)  # P=0.04
# transform - log did not work on depth 20. Very difficult to do different transformations and scaling. All transformed to sqrt
shapiro.test(sqrt(Field$WatSolP_10)) # p=0.96
hist(sqrt(Field$WatSolP_10)) #  slight right  skew
leveneTest(sqrt(WatSolP_10)~Treatment, data=Field)  # P=0.74
shapiro.test(sqrt(Field$WatSolP_20)) # p=0.85
hist(sqrt(Field$WatSolP_20)) #  normal
leveneTest(sqrt(WatSolP_20)~Treatment, data=Field)  # P=0.00045
shapiro.test(log(Field$WatSolP_30)) # NA
hist(log(Field$WatSolP_30)) #  normalish, two spikes
leveneTest(log(WatSolP_30)~Treatment, data=Field)  # NAN/Inf
shapiro.test(sqrt(Field$WatSolP_30)) # 0.35
hist(sqrt(Field$WatSolP_30)) #  left skew
leveneTest(sqrt(WatSolP_30)~Treatment, data=Field)  # 0.11
# Change data to long format
WSP_long <- Field |>
  gather(key="Depth", value="WSP", matches("WatSolP_"))
WSP_long_sub <- subset(WSP_long, select=c(Block, Treatment, Depth, WSP))
WSP_long_sub <- WSP_long_sub[complete.cases(WSP_long_sub[, c("Block", "Treatment", "Depth", "WSP")]), ]
WSP_long_sub$Depth <- factor(WSP_long_sub$Depth,
                             levels = c("WatSolP_10", "WatSolP_20", "WatSolP_30"),
                             labels = c("0-10cm", "10-20cm", "20-30cm"))
WSP_long_sub$WSP<- as.numeric(WSP_long_sub$WSP)
print(WSP_long_sub)
# Models - anovas indicate coefficients have arithmetic operators in their names, glmer does not work
# ModFieldWSP1
ModFieldWSP1 <-   lmer(sqrt(WSP)~Treatment*Depth + (1|Block), data=WSP_long_sub)
Anova(ModFieldWSP1,type="III", alpha=0.1) # no significant differences
summary(ModFieldWSP1, alpha=0.1)
print(coef(summary(ModFieldWSP1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
rsq(ModFieldWSP1) # 0.83
# ModFieldWSP2
ModFieldWSP2 <- lme(sqrt(WSP)~Treatment*Depth,random=~1|Block, data=WSP_long_sub)
Anova(ModFieldWSP2, type="III")  # no significant differences
summary(ModFieldWSP2)
rsq(ModFieldWSP2) # 0.83
# ModFieldWSP3 
ModFieldWSP3 <- glmmTMB(sqrt(WSP)~Treatment*Depth+(1|Block), data=WSP_long_sub, family=gaussian())
glmmTMB:::Anova.glmmTMB(ModFieldWSP3, type="III") # no significant differences
summary(ModFieldWSP3)
performance::r2(ModFieldWSP3) # 0.83

# AIC & BIC
FWSP_modlist <- list(ModFieldWSP1, ModFieldWSP2, ModFieldWSP3)
AIC_values <- sapply(FWSP_modlist, AIC)
BIC_values <- sapply(FWSP_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFieldWSP1", "ModFieldWSP2", "ModFieldWSP3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldWSP1   97.01094   131.7428 fractional df more than samples
#2 ModFieldWSP2   97.01094   126.5513 - 3 df
#3 ModFieldWSP3   75.04049   109.7724 - 38 df, best fittting model

#emmeans 
(ModFieldemWSP <- emmeans(ModFieldWSP3,~Treatment|Depth, infer=TRUE, type="response"))
(ModFieldemWSP_cld <- cld(ModFieldemWSP,Letters=trimws(letters), reversed=TRUE, by = "Depth", alpha=0.1))
ModFieldemWSP_cld <- as.data.frame(ModFieldemWSP_cld)
(ModFieldemWSP_cld <- ModFieldemWSP_cld %>% dplyr::rename(emmean="response"))
write_xlsx(ModFieldemWSP_cld, path="Field_SoilWSP.xlsx")
# Visualizations
(FieldWSPPlot <- ggplot(ModFieldemWSP_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
    geom_point(size = 8, color="black", aes(fill=Depth)) +
    scale_fill_manual(values = c("grey10", "grey45", "grey80"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.45, check_overlap = TRUE) +
    scale_x_discrete(labels=FieldLabDash_Main)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'))+
    labs(x = "", y = bquote(bold("Water soluble P (mg/kg)"))) +
    theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=16),
          axis.text.x=element_blank(),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=24, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Soil resin P   ----
# Skewness and kurtosis
ResP_VarNames <- c("ResinP_10", "ResinP_20", "ResinP_30") 
(ResP_stats_all <- SkewKur(Field, ResP_VarNames))
      #ResinP_10_skewness ResinP_20_skewness ResinP_30_skewness ResinP_10_kurtosis ResinP_20_kurtosis ResinP_30_kurtosis
      #1          0.04961507          0.9152297          0.6344956         -0.9494498           -0.11395          -1.081098
# Normality and equality of variance
shapiro.test(Field$ResinP_10) # p=0.94
hist(Field$ResinP_10) #  normalish, two spikes
leveneTest(ResinP_10~Treatment, data=Field)  # P=0.00099
shapiro.test(Field$ResinP_20) # p=0.04
hist(Field$ResinP_20) #  left skew
leveneTest(ResinP_20~as.factor(Treatment), data=Field)  # P=0.21
shapiro.test(Field$ResinP_30) # p=0.0011
hist(Field$ResinP_30) #  severe left skew
leveneTest(ResinP_30~as.factor(Treatment), data=Field)  # P=0.91
# Transformations - neither transformation works on the2-30cm depth, use original data
shapiro.test(log(Field$ResinP_10))  #p=0.67
leveneTest(log(ResinP_10)~Treatment, data=Field)  # p=0.0022
shapiro.test(log(Field$ResinP_20))  #p=0.72
leveneTest(log(ResinP_20)~Treatment, data=Field)  # p=0.47
shapiro.test(log(Field$ResinP_30))  #p=NA
leveneTest(log(ResinP_30)~Treatment, data=Field)  # p=NA
shapiro.test(sqrt(Field$ResinP_10))  #p=0.84
leveneTest(sqrt(ResinP_10)~Treatment, data=Field)  # p=0.000085
shapiro.test(sqrt(Field$ResinP_20))  #p=0.36
leveneTest(sqrt(ResinP_20)~Treatment, data=Field)  # p=0.24
shapiro.test(sqrt(Field$ResinP_30))  #p=0.00036
leveneTest(sqrt(ResinP_30)~Treatment, data=Field)  # p=0.92
# Change data to long format
ResP_long <- Field |>
  gather(key="Depth", value="ResP", matches("ResinP_"))
ResP_long_sub <- subset(ResP_long, select=c(Block, Treatment, Depth, ResP))
ResP_long_sub <- ResP_long_sub[complete.cases(ResP_long_sub[, c("Block", "Treatment", "Depth", "ResP")]), ]
ResP_long_sub$Depth <- factor(ResP_long_sub$Depth,
                              levels = c("ResinP_10", "ResinP_20", "ResinP_30"),
                              labels = c("0-10cm", "10-20cm", "20-30cm"))
ResP_long_sub$ResP<- as.numeric(ResP_long_sub$ResP)
print(ResP_long_sub)
# Models
# ModFieldResP1 
ModFieldResP1 <- lmer(ResP~Treatment*Depth + (1|Block), data=ResP_long_sub)
Anova(ModFieldResP1, type="III", alpha=0.1) # significant differences
summary(ModFieldResP1, alpha=0.1)
print(coef(summary(ModFieldResP1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
rsq(ModFieldResP1) # 0.85
# ModFieldResP2 
ModFieldResP2 <- lme(ResP~Treatment*Depth,random=~1|Block, data=ResP_long_sub)
Anova(ModFieldResP2, type="III")  # significant differences
summary(ModFieldResP2)
rsq(ModFieldResP2) # 0.85
# ModFieldResP3
ModFieldResP3 <- glmmTMB(ResP~Treatment*Depth+(1|Block), data=ResP_long_sub, family=gaussian())
glmmTMB:::Anova.glmmTMB(ModFieldResP3, type="III") # significant differences
summary(ModFieldResP3)
performance::r2(ModFieldResP3) # 0.76

# AIC & BIC - mod3
FResP_modlist <- list(ModFieldResP1, ModFieldResP2, ModFieldResP3)
AIC_values <- sapply(FResP_modlist, AIC)
BIC_values <- sapply(FResP_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFieldResP1", "ModFieldResP2", "ModFieldResP3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldResP1  -12.48192   21.94906 - 36.3 df, highest rsq & lowest AIC, chosen as best model
#2 ModFieldResP2  -12.48192   16.64881 - 3 df
#3 ModFieldResP3  -73.80936  -39.37838 - 39 df

# emmeans 
(ModFieldemResP <- emmeans(ModFieldResP1,~Treatment|Depth, infer=TRUE))
(ModFieldemResP_cld <- cld(ModFieldemResP,Letters=trimws(letters), reversed=TRUE, by = "Depth", alpha=0.1))
write_xlsx(ModFieldemResP_cld, path="Field_SoilResinP.xlsx")
# Visualizations
(FieldResPPlot <- ggplot(ModFieldemResP_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
    geom_point(size = 8, color="black", aes(fill=Depth)) +
    scale_fill_manual(values = c("grey10", "grey45", "grey80"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.45) +
    scale_x_discrete(labels=FieldLabDash_Main)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'))+
    labs(x = "", y = bquote(bold("Resin P (µg/cm"^2*~")"))) +
    theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=16),
          axis.text.x=element_blank(),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=24, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Soil Organic carbon ----
# Skewness and kurtosis
OC_VarNames <- c("OC_10", "OC_20", "OC_30")
(OC_stats_all <- SkewKur(Field, OC_VarNames))
#OC_10_skewness OC_20_skewness OC_30_skewness OC_10_kurtosis OC_20_kurtosis OC_30_kurtosis
#1      0.7032598     0.09182247     -0.2754793      0.2556663      -1.166542      -1.369227
# Normality and equality of variance
shapiro.test(Field$OC_10) # p=0.42
hist(Field$OC_10) # normal
leveneTest(OC_10~Treatment, data=Field)  # P=0.89
shapiro.test(Field$OC_20) # p=0.62
hist(Field$OC_20) #  normalish
leveneTest(OC_20~as.factor(Treatment), data=Field)  # P=0.75
shapiro.test(Field$OC_30) # p=0.17
hist(Field$OC_30) # normalish
leveneTest(OC_30~as.factor(Treatment), data=Field)  # P=0.76
# Change data to long format
OC_long <- Field |>
  gather(key="Depth", value="OC", matches("OC_"))
OC_long_sub <- subset(OC_long, select=c(Block, Treatment, Depth, OC))
OC_long_sub <- OC_long_sub[complete.cases(OC_long_sub[, c("Block", "Treatment", "Depth", "OC")]), ]
OC_long_sub$Depth <- factor(OC_long_sub$Depth,
                            levels = c("OC_10", "OC_20", "OC_30"),
                            labels = c("0-10cm", "10-20cm", "20-30cm"))
OC_long_sub$OC<- as.numeric(OC_long_sub$OC)
print(OC_long_sub)
# Models
# ModFieldOC1  - issues with arithmentic operators in name
ModFieldOC1 <- lmer(OC~Treatment*Depth + (1|Block), data=OC_long_sub)
Anova(ModFieldOC1, alpha=0.1, type="III") # significant differences
summary(ModFieldOC1, alpha=0.1)
print(coef(summary(ModFieldOC1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
rsq(ModFieldOC1) # 0.93
# ModFieldOC2 
ModFieldOC2 <- lme(OC~Treatment*Depth,random=~1|Block, data=OC_long_sub)
Anova(ModFieldOC2, type="III")  # significant differences
summary(ModFieldOC2)
rsq(ModFieldOC2) # 0.93
# ModFieldOC3 
ModFieldOC3 <- glmmTMB(OC~Treatment*Depth+(1|Block), data=OC_long_sub, family=gaussian())
glmmTMB:::Anova.glmmTMB(ModFieldOC3, type="III") # significant differences
summary(ModFieldOC3)
performance::r2(ModFieldOC3) # 0.94

# AIC & BIC
FOC_modlist <- list(ModFieldOC1, ModFieldOC2, ModFieldOC3)
AIC_values <- sapply(FOC_modlist, AIC)
BIC_values <- sapply(FOC_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFieldOC1", "ModFieldOC2", "ModFieldOC3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldOC1   11.07437  45.505349 - df=8.35 
#2 ModFieldOC2   11.07437  40.205096 - df=3
#3 ModFieldOC3  -41.79174  -7.360762 - df=39, highest rsq, lowest BIC, chosen as best model

# emmeans 
(ModFieldemOC <- emmeans(ModFieldOC3,~Treatment|Depth, infer = TRUE))
(ModFieldemOC_cld <- cld(ModFieldemOC, Letters=trimws(letters), reversed=TRUE, by = "Depth", alpha=0.1))
write_xlsx(ModFieldemOC_cld, path="Field_soilOC.xlsx")
# Visualizations
(FieldOCPlot <- ggplot(ModFieldemOC_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
    geom_point(size = 8, color="black", aes(fill=Depth)) +
    scale_fill_manual(values = c("grey10", "grey45", "grey80"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.45) +
    scale_x_discrete(labels=FieldLabDash_Main)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'))+
    labs(x = "", y = bquote(bold("Organic carbon (%)"))) +
    theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=16),
          axis.text.x=element_blank(),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=24, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
### Soil pH ----
# Skewness and kurtosis
pH_VarNames <- c("pH_10", "pH_20", "pH_30") 
(pH_stats_all <- SkewKur(Field, pH_VarNames))
      #pH_10_skewness pH_20_skewness pH_30_skewness pH_10_kurtosis pH_20_kurtosis pH_30_kurtosis
      #  1     -0.2754083      0.2048821     0.05165455      -1.206359     -0.5453826      -1.297643
# Normality and equality of variance
shapiro.test(Field$pH_10) # p=0.36
hist(Field$pH_10) #  normalish, two spikes
leveneTest(pH_10~Treatment, data=Field)  # P=0.04
shapiro.test(Field$pH_20) # p=0.78
hist(Field$pH_20) #  normal
leveneTest(pH_20~as.factor(Treatment), data=Field)  # P=0.28
shapiro.test(Field$pH_30) # p=0.2
hist(Field$pH_30) #  slight right skew
leveneTest(pH_30~as.factor(Treatment), data=Field)  # P=0.52
# Change data to long format
pH_long <- Field |>
  gather(key="Depth", value="pH", matches("pH_"))
pH_long_sub <- subset(pH_long, select=c(Block, Treatment, Depth, pH))
pH_long_sub <- pH_long_sub[complete.cases(pH_long_sub[, c("Block", "Treatment", "Depth", "pH")]), ]
pH_long_sub$Depth <- factor(pH_long_sub$Depth,
                            levels = c("pH_10", "pH_20", "pH_30"),
                            labels = c("0-10cm", "10-20cm", "20-30cm"))
pH_long_sub$pH<- as.numeric(pH_long_sub$pH)
print(pH_long_sub)
# Models
# ModFieldpH1 
ModFieldpH1 <- lmer(pH~Treatment*Depth + (1|Block), data=pH_long_sub)
Anova(ModFieldpH1, alpha=0.1, type="III") # no significant differences
summary(ModFieldpH1, alpha=0.1)
print(coef(summary(ModFieldpH1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
rsq(ModFieldpH1) # 0.49
# ModFieldpH2 
ModFieldpH2 <- lme(pH~Treatment*Depth,random=~1|Block, data=pH_long_sub)
Anova(ModFieldpH2, type="III")   # no significant differences
summary(ModFieldpH2)
rsq(ModFieldpH2) # 0.49
# ModFieldpH3
ModFieldpH3 <- glmmTMB(pH~Treatment*Depth+(1|Block), data=pH_long_sub, family=gaussian())
glmmTMB:::Anova.glmmTMB(ModFieldpH3, type="III") # no significant differences
summary(ModFieldpH3)
performance::r2(ModFieldpH3) # 0.52

# AIC & BIC
FpH_modlist <- list(ModFieldpH1, ModFieldpH2, ModFieldpH3)
AIC_values <- sapply(FpH_modlist, AIC)
BIC_values <- sapply(FpH_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFieldpH1", "ModFieldpH2", "ModFieldpH3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldpH1  57.66083   92.97896 - df=28.5
#2 ModFieldpH2   57.66083   87.99205 - df=3
#3 ModFieldpH3   20.90485   56.22299 - df=42, chosen as best model

# emmeans 
(ModFieldempH <- emmeans(ModFieldpH3,~Treatment|Depth, infer = TRUE))
(ModFieldempH_cld <- cld(ModFieldempH, Letters=trimws(letters), reversed=TRUE, by = "Depth", alpha=0.1))
write_xlsx(ModFieldempH_cld, path="Field_soilpH.xlsx")
# Visualizations
(FieldpHPlot <- ggplot(ModFieldempH_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
    geom_point(size = 8, color="black", aes(fill=Depth)) +
    scale_fill_manual(values = c("grey10", "grey45", "grey80"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.45)+
    scale_x_discrete(labels=FieldLabDash_Main)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'))+
    labs(x = "", y = bquote(bold("pH"))) +
    theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=16),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_text(size = 24, face = "bold", colour = "black"),
          axis.title.y=element_text(size=24, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Soil EC ----
# Skewness and kurtosis
EC_VarNames <- c("EC_10", "EC_20", "EC_30") 
(EC_stats_all <- SkewKur(Field, EC_VarNames))
        #EC_10_skewness EC_20_skewness EC_30_skewness EC_10_kurtosis EC_20_kurtosis EC_30_kurtosis
        #1      0.6200731       1.361598       1.327928     -0.8811796       1.018784      0.4512029
# Normality and equality of variance
shapiro.test(Field$EC_10) # p=0.089
hist(Field$EC_10) #  left skew
leveneTest(EC_10~Treatment, data=Field)  # P=0.00064
shapiro.test(Field$EC_20) # p=0.0014
hist(Field$EC_20) # left skew
leveneTest(EC_20~as.factor(Treatment), data=Field)  # P=0.00019
shapiro.test(Field$EC_30) # p=0.00102
hist(Field$EC_30) #  severe left skew
leveneTest(EC_30~as.factor(Treatment), data=Field)  # P=0.62
# transform - log transform worked best
shapiro.test(log(Field$EC_10))  # p=0.34
leveneTest(log(EC_10)~Treatment, data=Field)  # p= 0.004
shapiro.test(log(Field$EC_20))  # p=0.19
leveneTest(log(EC_20)~Treatment, data=Field)  # p= 0.0089
shapiro.test(log(Field$EC_30))  # p=0.202
leveneTest(log(EC_30)~Treatment, data=Field)  # p=0.89
shapiro.test(sqrt(Field$EC_10))  # p=0.19
leveneTest(sqrt(EC_10)~Treatment, data=Field)  # p= 0.0017
shapiro.test(sqrt(Field$EC_20))  # p=8.2e-0.022
leveneTest(sqrt(EC_20)~Treatment, data=Field)  # p= 0.0014
shapiro.test(sqrt(Field$EC_30))  # p=0.017
leveneTest(sqrt(EC_30)~Treatment, data=Field)  # p=0.75
# Change data to long format
EC_long <- Field |>
  gather(key="Depth", value="EC", matches("EC_"))
EC_long_sub <- subset(EC_long, select=c(Block, Treatment, Depth, EC))
EC_long_sub <- EC_long_sub[complete.cases(EC_long_sub[, c("Block", "Treatment", "Depth", "EC")]), ]
EC_long_sub$Depth <- factor(EC_long_sub$Depth,
                            levels = c("EC_10", "EC_20", "EC_30"),
                            labels = c("0-10cm", "10-20cm", "20-30cm"))
EC_long_sub$EC<- as.numeric(EC_long_sub$EC)
print(EC_long_sub)
# Models
# ModFieldEC1 
ModFieldEC1 <- lmer(log(EC)~Treatment*Depth + (1|Block), data=EC_long_sub)
Anova(ModFieldEC1, alpha=0.1, type="III") # no significant differences
summary(ModFieldEC1, alpha=0.1)
print(coef(summary(ModFieldEC1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
rsq(ModFieldEC1) # 0.51
# ModFieldEC2 
ModFieldEC2 <- lme(log(EC)~Treatment*Depth,random=~1|Block, data=EC_long_sub)
Anova(ModFieldEC2, type="III")  # no significant differences
summary(ModFieldEC2)
rsq(ModFieldEC2) # 0.51
# ModFieldEC3 - singularity picked up in rsq
ModFieldEC3 <- glmmTMB(log(EC)~Treatment*Depth+(1|Block), data=EC_long_sub, family=gaussian())
glmmTMB:::Anova.glmmTMB(ModFieldEC3, type="III") # no significant differences
summary(ModFieldEC3)
performance::r2(ModFieldEC3) # NA0.53

# AIC & BIC
FEC_modlist <- list(ModFieldEC1, ModFieldEC2, ModFieldEC3)
AIC_values <- sapply(FEC_modlist, AIC)
BIC_values <- sapply(FEC_modlist, BIC)
(N_AB <- data.frame(Model=c("ModFieldEC1", "ModFieldEC2", "ModFieldEC3"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldEC1   109.59074   143.4035 - df=38.3
#2 ModFieldEC2  109.59074   137.8713 - df=3
#3 ModFieldEC3   94.67671   128.4894 - df=37, chosen as best model

# emmeans 
(ModFieldemEC <- emmeans(ModFieldEC3,~Treatment|Depth, type="response",infer = TRUE))
(ModFieldemEC_cld <- cld(ModFieldemEC, Letters=trimws(letters), reversed=TRUE, by = "Depth", alpha=0.1))
ModFieldemEC_cld <- ModFieldemEC_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFieldemEC_cld, path="Field_soilEC.xlsx")
# Visualizations
(FieldECPlot <- ggplot(ModFieldemEC_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
    geom_point(size = 8, color="black", aes(fill=Depth)) +
    scale_fill_manual(values = c("grey10", "grey45", "grey80"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.45, check_overlap = TRUE) +
    scale_x_discrete(labels=FieldLabDash_Main)+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'))+
    labs(x = "", y = bquote(bold("Electric conductivity (mS/cm)"))) +
    theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
          legend.title = element_blank(), legend.text=element_text(size=16),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_text(size = 24, face = "bold", colour = "black"),
          axis.title.y=element_text(size=24, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))


### combined residual plots ----
(FresidPlot <- ggarrange(FieldPO4Plot, FieldWSPPlot, FieldResPPlot, FieldOCPlot, FieldpHPlot, FieldECPlot, nrow=3, ncol=2, heights = c(0.8,0.8,1),
                         labels = "AUTO", font.label = list(size=25, face="bold"), label.x = c(0.18,0.17,0.17,0.17,0.17,0.17),
                         common.legend = TRUE, legend = "right"))
annotate_figure(FresidPlot, bottom=text_grob("Treatment", face="bold", size=25))
ggsave("Combined residual soil.jpg", height=18, width=16, dpi=150)



## Correlate char P ----
FieldCharPdf <- data.frame(Treatment=Field$Treatment, PO4=Field$PO4_10, CharPerc=Field$CharPerc)
FieldCharPdf <- subset(FieldCharPdf, Treatment != "Control")
print(FieldCharPdf)
FieldCharCor <- FieldCharPdf %>%
  group_by(Treatment) %>%
  dplyr::summarize(Correlation = cor(CharPerc, PO4, use = "complete.obs"))%>%
  ungroup()
FieldCharCor$Treatment <- factor(FieldCharCor$Treatment, levels=FieldTrtSub, labels=FieldLabDash_sub)
print(FieldCharCor)
# visualize correlation
(FieldCharHeatPlot <- ggplot(FieldCharCor, aes(x=Treatment, y=0.5, fill=Correlation)) +
    geom_point(data=FieldCharCor, aes(size=abs(Correlation)*20), shape=21) +
    scale_size(range = c(15, 30)) +
    scale_fill_viridis(option = "magma", limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f", Correlation), color = ifelse(Correlation < 0, "white", "black")), size=5.5)+
    scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
    scale_x_discrete(labels = c("Biochar<br>25kg P ha<sup>-1</sup>",
                                "Biochar<br>10t ha<sup>-1</sup>",
                                "Biochar<br>10t ha<sup>-1</sup>",
                                "TSP<br>Fertilizer"))+
    labs(x="", y="", title=expression(bold("% P in Treatment - Soil PO"[4])))+
    guides(size = "none")+
    theme(plot.title = element_text(size=18, face="bold", hjust=0.5),
          legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold"), legend.key.siz=unit(15,"mm"),
          axis.title=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_markdown(angle=45, size=16, colour="black", face="bold", vjust=1, hjust=0.5),
          axis.ticks=element_blank(), panel.background=element_blank(),
          panel.spacing.x=unit(1, "cm"), plot.margin=margin(15, 15, 5, 5)))

## correlate P uptake to to %P in char
FieldRecPdf <- data.frame(Treatment=Field$Treatment, Precovery=Field$Precovery, CharPerc=Field$CharPerc)
FieldRecPdf <- subset(FieldRecPdf, Treatment != "Control")
print(FieldRecPdf)
FieldRecCor <- FieldRecPdf %>%
  group_by(Treatment) %>%
  dplyr::summarize(Correlation = cor(Precovery, CharPerc, use = "complete.obs"))%>%
  ungroup()
FieldRecCor$Treatment <- factor(FieldRecCor$Treatment, levels=FieldTrtSub, labels=FieldLabDash_sub)
print(FieldRecCor)
# visualize correlation
(FieldRecHeatPlot <- ggplot(FieldRecCor, aes(x=Treatment, y=0.5, fill=Correlation)) +
    geom_point(data=FieldRecCor, aes(size=abs(Correlation)*20), shape=21) +
    scale_size(range = c(15, 30)) +
    scale_fill_viridis(option = "magma", limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f", Correlation), color = ifelse(Correlation < 0, "white", "black")), size=5.5)+
    scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
    scale_x_discrete(labels = c("Biochar<br>25kg P ha<sup>-1</sup>",
                                "Biochar<br>10t ha<sup>-1</sup>",
                                "Biochar<br>10t ha<sup>-1</sup>",
                                "TSP<br>Fertilizer"))+
    labs(x="", y="", title="% P in Treatment - % P Recovery")+
    guides(size = "none")+
    theme(plot.title = element_text(size=18, face="bold", hjust=0.5),
          legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold"), legend.key.siz=unit(15,"mm"),
          #legend.position = "none",
          axis.title=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_markdown(angle=45, size=16, colour="black", face="bold", vjust=1, hjust=0.5),
          axis.ticks=element_blank(), panel.background=element_blank(),
          panel.spacing.x=unit(1, "cm"), plot.margin=margin(15,5,5,5)))

## correlate P uptake to residual PO4
FieldPO4Pdf <- data.frame(Treatment=Field$Treatment, PO4=Field$PO4_10, Precovery=Field$Precovery)
FieldPO4Pdf <- subset(FieldPO4Pdf, Treatment != "Control")
print(FieldPO4Pdf)
FieldPO4Cor <- FieldPO4Pdf %>%
  group_by(Treatment) %>%
  dplyr::summarize(Correlation = cor(Precovery, PO4, use = "complete.obs"))%>%
  ungroup()
FieldPO4Cor$Treatment <- factor(FieldPO4Cor$Treatment, levels=FieldTrtSub, labels=FieldLabDash_sub)
print(FieldPO4Cor)
# visualize correlation
(FieldPO4HeatPlot <- ggplot(FieldPO4Cor, aes(x=Treatment, y=0, fill=Correlation)) +
    geom_point(data=FieldPO4Cor, aes(size=abs(Correlation)*20), shape=21) +
    scale_size(range = c(15, 30)) +
    scale_fill_viridis(option = "magma", limits=c(-1, 1), breaks=seq(-1, 1, by=0.5)) + 
    geom_text(aes(label=sprintf("%.2f", Correlation), color = ifelse(Correlation < 0, "white", "black")), size=5.5)+
    scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
    labs(x="", y="", title=expression(bold("% P Recovery - Soil PO"[4])))+
    scale_x_discrete(labels = c("Biochar<br>25kg P ha<sup>-1</sup>",
                                "Biochar<br>10t ha<sup>-1</sup>",
                                "Biochar<br>10t ha<sup>-1</sup>",
                                "TSP<br>Fertilizer")) +
    guides(size = "none")+
    theme(plot.title = element_text(size=18, face="bold", hjust=0.5),
          legend.text=element_text(size=14), legend.title=element_text(size=16, face="bold"), legend.key.size=unit(15,"mm"),
          legend.position = "none",
          axis.title=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x = element_markdown(angle=45, size=16, colour="black", face="bold", vjust = 1, hjust=0.5),  #debug = TRUE,
          axis.ticks=element_blank(), panel.background=element_blank(),
          plot.margin=margin(15,5,5,15)))
ggsave(FieldPO4HeatPlot, file="test_plot.jpg", width=6, height=3.22, dpi=150)

## combined plot - combined legend and ggarrange uses ggpubr package, set legend in individual plots
(FieldCharRecPO4_plot <-ggarrange(FieldCharHeatPlot, FieldRecHeatPlot, FieldPO4HeatPlot, ncol=3, common.legend = TRUE, legend="none"))
(FieldCharAnnotate <- annotate_figure(FieldCharRecPO4_plot, top=text_grob("Field study with willow biochar", size=24, face="bold")))
ggsave(file="Field_CharPO4Prec_combined.jpg", width=15, height=5, dpi=150)


(TotalCharPlot <- ggarrange(PotsCharAnnotate, FieldCharAnnotate, nrow=2, heights = c(0.9,0.3),
                            labels = "AUTO", font.label = list(size=30, face="bold")))
ggsave(TotalCharPlot, file="Fig2.Total_CharPO4Prec.jpg", width=13, height=14, dpi=150)
ggsave(TotalCharPlot, file="Figure2.eps", width=13, height=14, dpi=150)
postscript("Figure2.eps", horizontal=FALSE, onefile=FALSE, paper = "special")
plot(1:10)
dev.off()



## SNOWMELT ----
### PO4 load ----
print(LPO4_stats <- SkewKur(Field, "LPO4"))
      #skewness kurtosis
      #1 2.370513 5.432775
shapiro.test(Field$LPO4) # p=2.7e-05
hist(Field$LPO4) # heavy left skew
leveneTest(LPO4~factor(Treatment), data=Field)  # P=0.12
# transform - log provides most improvement
shapiro.test(log(Field$LPO4)) # p=0.32
hist(log(Field$LPO4)) #  slight right skew
leveneTest(log(LPO4)~factor(Treatment), data=Field)  # P= 0.39
shapiro.test(sqrt(Field$LPO4)) # p=0.039
hist(sqrt(Field$LPO4)) #  severe left skew
leveneTest(sqrt(LPO4)~factor(Treatment), data=Field)  # P= 0.11
#ModFieldLPO4a
ModFieldLPO4a <- lmer(log(LPO4)~Treatment + (1|Block), data=Field, na.action=na.omit)
Anova(ModFieldLPO4a, type="III") # no significant differences
summary(ModFieldLPO4a)
rsq(ModFieldLPO4a) # 0.48
# ModFieldLPO4b
ModFieldLPO4b <- lme(log(LPO4)~Treatment,random=~1|Block, data=Field, na.action=na.omit)
Anova(ModFieldLPO4b, type="III")  # no significant differences
summary(ModFieldLPO4b)
rsq(ModFieldLPO4b) # 0.33
# ModFieldLPO4c 
ModFieldLPO4c <- glmmTMB(log(LPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
glmmTMB:::Anova.glmmTMB(ModFieldLPO4c, type="III") # no significant differences
summary(ModFieldLPO4c)
performance::r2(ModFieldLPO4c) # 0.56
# one-way Kruskal-Wallis
LPO4_subset <- subset(Field, !is.na(LPO4))
LPO4_subset$LPO4 <- as.numeric(LPO4_subset$LPO4)
LPO4_subset$Treatment <- as.factor(LPO4_subset$Treatment)
(ModFieldLPO4d=kruskal.test(LPO4 ~Treatment, data=LPO4_subset)) #p=0.78
(ModFPO4_Dunn <- FSA::dunnTest(LPO4_subset$LPO4, LPO4_subset$Treatment, method="bonferroni")) # no sig dif

# AIC& BIC
FLPO4_modlist <- list(ModFieldLPO4a, ModFieldLPO4b, ModFieldLPO4c)
AIC_values <- sapply(FLPO4_modlist, AIC)
BIC_values <- sapply(FLPO4_modlist, BIC)
(FLPO4AB <- data.frame(Model=c("ModFieldLPO4a", "ModFieldLPO4b", "ModFieldLPO4c"), AIC_values, BIC_values))
##Model AIC_values BIC_values
#1 ModFieldLPO4a  64.58095   70.41344 - fradtional df
#2 ModFieldLPO4b  64.58095   67.97530 - 3 df
#3 ModFieldLPO4c  70.99796   76.83045 - 10 df - chosen as best option

# emmeans 
(ModFieldLPO4em <- emmeans(ModFieldLPO4c,~Treatment,infer = TRUE, type="response"))
(ModFieldLPO4em_cld <- cld(ModFieldLPO4em, Letters=trimws(letters), reversed=TRUE, alpha=0.1))
ModFieldLPO4em_cld <- ModFieldLPO4em_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFieldLPO4em_cld, path="Field_snowPO4.xlsx")
# Vizualization
(LPO4plot <- ggplot(ModFieldLPO4em_cld, aes(x=Treatment, y=emmean)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), col="black", fill="grey45", width=0.65)+
    scale_y_continuous(limits=c(-0.001, 0.07)) +
    geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.25)+
    geom_text(aes(label=trimws(.group), y=emmean+SE, fontface="bold"), size=6, vjust=-1)+
    labs(x="", y="Soluble reactive P load (kg/ha)")+
    scale_x_discrete(limits=as.character(FieldTrt_order), labels=FieldLabDash_Main)+
    theme(legend.title=element_blank() , legend.key=element_blank(), legend.text=element_blank(),
          strip.text.x.top=element_text(size=20, face="bold"), strip.background = element_blank(),
          plot.title=element_blank(),
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=14, face="bold", colour="black"),
          axis.text.y=element_text(size=14, face="bold", colour="black"),
          axis.title=element_text(size=17, face="bold"),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Resin PO4 load ----
print(ResPO4_stats <- SkewKur(Field, "ResinPO4"))
      #skewness kurtosis
      #1 1.642287 1.790308
shapiro.test(Field$ResinPO4) # p=0.00059
hist(Field$ResinPO4) #  left skew
leveneTest(ResinPO4~Treatment, data=Field)  # P=0.52
#transform
shapiro.test(log(Field$ResinPO4))  #p=0.69
hist(log(Field$ResinPO4)) # normal
leveneTest(log(ResinPO4)~Treatment, data=Field)  # p=0.7
#ModFieldResP1
ModFieldResPO4a <- lmer(log(ResinPO4)~Treatment + (1|Block), data=Field, na.action=na.omit)
Anova(ModFieldResPO4a, type="III") # no significant differences
summary(ModFieldResPO4a)
rsq(ModFieldResPO4a) # 0.43
# ModFieldResPO4b
ModFieldResPO4b <- lme(log(ResinPO4)~Treatment,random=~1|Block, data=Field, na.action=na.omit)
Anova(ModFieldResPO4b, type="III")  # no significant differences
summary(ModFieldResPO4b)
rsq(ModFieldResPO4b) # 0.37
# ModFieldResPO4c
ModFieldResPO4c <- glmmTMB(log(ResinPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
glmmTMB:::Anova.glmmTMB(ModFieldResPO4c, type="III") # no significant differences
summary(ModFieldResPO4c)
performance::r2(ModFieldResPO4c) # 0.55

# AIC& BIC
FResPO4_modlist <- list(ModFieldResPO4a, ModFieldResPO4b, ModFieldResPO4c)
AIC_values <- sapply(FResPO4_modlist, AIC)
BIC_values <- sapply(FResPO4_modlist, BIC)
(FResPO4AB <- data.frame(Model=c("ModFieldResPO4a", "ModFieldResPO4b", "ModFieldResPO4c"), AIC_values, BIC_values))
#Model AIC_values BIC_values
#1 ModFieldResPO4a   43.36803   49.20053 - fractional df
#2 ModFieldResPO4b   43.36803   46.76238 - df=3
#3 ModFieldResPO4c   40.95933   46.79182 - df=10, best rsq. lowet AIC, chosen as best model

# emmeans 
(ModFieldemResPO4 <- emmeans(ModFieldResPO4c,~Treatment,infer = TRUE, type="response"))
(ModFieldemResPO4_cld <- cld(ModFieldemResPO4, Letters=trimws(letters), reversed=TRUE, alpha=0.1))
ModFieldemResPO4_cld <- ModFieldemResPO4_cld %>% dplyr::rename(emmean="response")
write_xlsx(ModFieldemResPO4_cld, path="Field_snowResinPO4.xlsx")
# Vizualization
(ResinPO4PPlot <- ggplot(ModFieldemResPO4_cld, aes(x=Treatment, y=emmean)) +
    geom_bar(stat="identity", position=position_dodge2(padding=0.2), col="black", fill="grey45", width=0.65)+
    scale_y_continuous(limit = c(-0.01, 0.8)) +
    geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.25)+
    geom_text(aes(label=trimws(.group), y=emmean+SE, fontface="bold"), size=6, vjust=-1)+
    labs(x="", y=bquote(bold("Resin PO"[4]~" load (µg/cm"^2~")")))+
    scale_x_discrete(limits=as.character(FieldTrt_order), labels=FieldLabDash_Main)+
    theme(legend.title=element_blank() , legend.key=element_blank(), legend.text=element_blank(),
          strip.text.x.top=element_text(size=20, face="bold"), strip.background = element_blank(),
          plot.title=element_blank(),
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=14, face="bold", colour="black"),
          axis.text.y=element_text(size=14, face="bold", colour="black"),
          axis.title=element_text(size=17, face="bold"),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))

### Plotting Snowmelt load ----
(Snowmeltplot <- ggarrange(LPO4plot, ResinPO4PPlot, ncol=2, nrow=1, labels = "AUTO", font.label = list(size=20, face="bold"),
                           label.x = c(0.17,0.19)))
annotate_figure(Snowmeltplot, bottom=text_grob("Treatment", size=20, face="bold"))
ggsave("Field_Snowmelt.jpg", height=6, width=9, dpi=150)



## INFILTRATION ----
### Summary and ordering of data   ----
Infil$Block <- factor(Infil$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Infil$Treatment <- factor(Infil$Treatment,levels = FieldTrt_order)
Infil$Time <- as.numeric(as.character(Infil$Time))
Infil$CI <- as.numeric(as.character(Infil$CI))
Infilsub$Block <- factor(Infilsub$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Infilsub$Treatment <- factor(Infilsub$Treatment,levels = FieldTrt_order)

### Model infiltration data ----
    ### DO NOT RUN THE DIVISION CODE MORE THAN ONCE!!!!!!
      # Change scale to cm and hours
Infil$CI <- Infil$CI/10
Infil$Infiltration <- Infil$Infiltration/10
Infil$Time <- Infil$Time/3600
# select columns from Infilsub to be combined with Infil
InfilSelect <- dplyr::select(Infilsub, Block, Treatment, Sorptivity, K) 
InfilDF <- merge(Infil, InfilSelect, by = c("Block", "Treatment"), all.x = TRUE, all.y = TRUE) # exludes all missing data from both DF
View(InfilDF <- InfilDF[complete.cases(InfilDF), ]) # has 2 missing block/treatment observations for control
### Interpolate data to 2 hours
InfilInterpol <- function(data) {
  model <- nls(CI ~ Sorptivity * Time^0.5 + K, data = data, start = list(Sorptivity = 0.01, K = 0.01))
  CappedInfilTime <- data %>%
    group_by(Treatment, Block) %>%
    summarise(Time = c(Time, 2)) %>%  # mutate works better to get past the warning but I gave up trying to get everything to work
    arrange(Treatment, Block, Time)
  PredictedCI <- predict(model, newdata = CappedInfilTime)
  CappedInfilTime$PredictedCI <- PredictedCI
  return(CappedInfilTime)
}
InfilPredicted <- InfilDF %>%
  group_by(Treatment, Block) %>%
  do(InfilInterpol(.)) %>%
  ungroup() 
View(InfilPredicted)
### set up new data frame with Predicted CI, CI and Infiltration columns along with new row for Time=5400
empty_row <- InfilDF %>%
  distinct(Treatment, Block) %>%
  mutate(Time = 2, Infiltration = NA, CI = NA)
InfilDF_updated <- bind_rows(InfilDF, empty_row) %>%
  arrange(Treatment, Block, Time)
InfilCombined <- bind_cols(InfilPredicted, dplyr::select(InfilDF_updated, Infiltration, CI))
print(InfilCombined, n=30)
View(InfilCombined)
write_xlsx(InfilCombined, path="Predicted infiltration data.xlsx")


## Visualizations
# reshape the data into long format
InfilReady <- filter(InfilCombined, Time <= 2)
InfilReady_long <- InfilReady%>%
  pivot_longer(cols = c(CI, PredictedCI, Infiltration),
               names_to = "Variable", values_to = "Value") # run every time an update is made to the treatment labels below
InfilReady_long$Treatment <- factor(InfilReady_long$Treatment,
                                    levels=c("Control", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"), 
                                     labels=c("bold(Control)", expression("bold('Biochar 25kg P'^-1)"), expression("bold('Biochar 10t ha'^-1)"),
                                              expression("bold('Biochar 10t ha'^-1*' & TSP')"), expression("bold(TSP~Fertilizer)")))
View(InfilReady_long)
  ## To get mathematical notations in the strip text of facet wrap use a combination of expression within the label functions and the `label_parsed`
  # argument in facet_wrap. Do NOT use the `labeller(type-label_parsed)` function in facet_wrap. Any space specified in a label not listed with
  # expression MUST have a ~ instead of a space. If `expression(bold(...` is specified, the parsing does not work with the notations. Specifying bold
  # within the strip text also doesn't work. I didn't bother looking further

color_palette <- c("black", "cyan", "darkorange4")
(InfilPlot <- ggplot(InfilReady_long, aes(x=Time, y=Value, color=Variable, lty=Variable))+
    facet_rep_wrap(~ Treatment, scales="fixed", ncol=5, labeller=label_parsed)+ # facet_rep keeps tick lines on plots with free scales
    coord_capped_cart(bottom = "both")+
    geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1, aes(color=Variable, fill=Variable))+
    #scale_color_manual(values = color_palette)+ # sets the colours of the lines
    #scale_fill_manual(values = color_palette, aesthetics = "fill")+ # set the SE colours
    scale_color_manual(values=c('black', 'grey35', 'grey75'))+
    scale_linetype_manual(values=c(2,1,1))+ # need to specify both this and lte in ggplot aes
    scale_fill_manual(values=c('black', 'grey35', 'grey75'), aesthetics = "fill")+
    scale_y_continuous(limits=c(0,12))+
    labs(x = "Time (hours)", y = "Infiltration (cm)")+
    theme(strip.text.x = element_text(size = 15, face="bold"), 
          strip.background = element_blank(), 
          legend.position = "bottom", legend.key.size=unit(7,"mm"), 
          legend.text=element_text(size=10, face="bold"), legend.title = element_blank(),
          axis.title.x = element_text(size=20, face="bold", margin = margin(0.5,0,0,0, unit="cm")),
          axis.title.y = element_text(size=20, face="bold", margin = margin(0,0.5,0,0, unit="cm")),
          axis.text = element_text(size=14, face="bold", colour = "black"),
          panel.border = element_blank(), panel.grid=element_blank(), panel.background = element_blank(),
          axis.line=element_line(colour="black")))
ggsave(InfilPlot, file="Fig2.Infiltration_color.tiff", width=15, height=4, dpi=200)
# warning related to blank outlier data for CI & I
ggsave(InfilPlot, file="Fig2.Infiltration_b&w.tiff", width=15, height=4, dpi=200)


### Model significant differences ----
#### Initial infiltration rate ----
InfilModInit1 <- lme(InitialRate~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit) # 3 df
Anova(InfilModInit, type="III")  # NO significant differences
rsq(InfilModInit) # 0.29
InfilModInit2 <- glmmTMB(InitialRate~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude) # preferred model
glmmTMB:::Anova.glmmTMB(InfilModInit2, type="III")
performance::r2(InfilModInit2) # 0.36
(InfilModInitEm <- emmeans(InfilModInit2,~"Treatment", infer = TRUE))
(InfilModInitEm_cld <- cld(InfilModInitEm, Letters=trimws(letters), reversed=TRUE, alpha=0.1))
write_xlsx(InfilModInitEm_cld, path="InfilMod_initial rate.xlsx")

#### Final infiltration rate ----
#Modeled at 2 hours
InfilModFinal1 <- lme(FinalRate~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit) # 3 df
Anova(InfilModFinal1, type="III")  # significant differences
rsq(InfilModFinal1) # 0.49
InfilModFinal2 <- glmmTMB(FinalRate~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude) # preferred model
glmmTMB:::Anova.glmmTMB(InfilModFinal2, type="III")
performance::r2(InfilModFinal2) # NA
(InfilModFinalEm <- emmeans(InfilModFinal2,~"Treatment", infer = TRUE))
(InfilModFinalEm_cld <- cld(InfilModFinalEm, Letters=trimws(letters), reversed=TRUE, alpha=0.5))
write_xlsx(InfilModFinalEm_cld, path="InfilMod_final rate.xlsx")

#### Sorptivity ----
InfilModSorp <- glmmTMB(Sorptivity~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(InfilModSorp, type="III")  # NO significant differences
performance::r2(InfilModSorp) # 0.2
(InfilModSorpEm <- emmeans(InfilModSorp,~"Treatment", infer = TRUE))
(InfilModSorpEm_cld <- cld(InfilModSorpEm, Letters=trimws(letters), reversed=TRUE, alpha=0.1))
write_xlsx(InfilModSorpEm_cld, path="InfilMod_Sorptivity.xlsx")

#### Hydraulic conductivity ----
InfilModK <- glmmTMB(K~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude)
glmmTMB:::Anova.glmmTMB(InfilModK) # No significant differences
performance::r2(InfilModK) #0.31
(InfilModKEm <- emmeans(InfilModK,~"Treatment", infer = TRUE))
(InfilModKEm_cld <- cld(InfilModKEm, Letters=trimws(letters), reversed=TRUE, alpha=0.1)) 
write_xlsx(InfilModKEm_cld, path="InfilMod_K.xlsx")




# EXTRACT ANOVA TABLES ----
# Pots
Pots1YieldMod2<-lmer(Yield~Treatment*Soil+(1|Soil),data=Pots1)
YieldAN <- Anova(Pots1YieldMod2, type="III", alpha=0.001)
YieldAN$RowNames <- row.names(YieldAN)
rownames(YieldAN) <- NULL

Pots1PupMod1 <- glmmTMB(Puptake~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
PupAN <- glmmTMB:::Anova.glmmTMB(Pots1PupMod1, type="III", alpha=0.001)
PupAN$RowNames <- row.names(PupAN)
rownames(PupAN) <- NULL

Pots1PrecMod3 <- glmmTMB(Precovery~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.omit)
PrecAN <- glmmTMB:::Anova.glmmTMB(Pots1PrecMod3, type="III")
PrecAN$RowNames <- row.names(PrecAN)
rownames(PrecAN) <- NULL

Post1PUEmod2 <- lmer(PUE ~ Treatment*Soil + (1|Soil), data=Pots1, na.action=na.omit)
PUEAN <- Anova(Post1PUEmod2, type="III")
PUEAN$RowNames <- row.names(PUEAN)
rownames(PUEAN) <- NULL

Post1PO4mod7 <- glmmTMB(log(PO4)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
PO4AN <- glmmTMB:::Anova.glmmTMB(Post1PO4mod7, type="III")
PO4AN$RowNames <- row.names(PO4AN)
rownames(PO4AN) <- NULL

Post1ResPmod1 <- glmmTMB(log10(ResinP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
ResPAN <- glmmTMB:::Anova.glmmTMB(Post1ResPmod1, type="III", alpha=0.001)
ResPAN$RowNames <- row.names(ResPAN)
rownames(ResPAN) <- NULL

Post1WSPmod1 <- glmmTMB(log(WaterSolP)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
WSPAN <- glmmTMB:::Anova.glmmTMB(Post1WSPmod1, type="III")
WSPAN$RowNames <- row.names(WSPAN)
rownames(WSPAN) <- NULL

Post1pHmod2 <- glmer(pH~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(link="log"))
(pHAN <- Anova(Post1pHmod2, type="III"))
pHAN$RowNames <- row.names(pHAN)
rownames(pHAN) <- NULL

Post1ECmod1 <- glmmTMB(log(EC)~Treatment*Soil+(1|Soil), data=Pots1, family=gaussian(), na.action=na.exclude)
(ecAN <- glmmTMB:::Anova.glmmTMB(Post1ECmod1, type="III"))
ecAN$RowNames <- row.names(ecAN)
rownames(ecAN) <- NULL

Post1OCmod1 <- glmer(OC~Treatment*Soil+(1|Soil),data=Pots1,family=Gamma(link="identity"), na.action=na.exclude)
(ocAN <- Anova(Post1OCmod1, type="III"))
ocAN$RowNames <- row.names(ocAN)
rownames(ocAN) <- NULL


# Field
ModFStraw4 <- glmmTMB(log(FStraw)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
FStrawAN <- glmmTMB:::Anova.glmmTMB(ModFStraw4, type="III")
FStrawAN$RowNames <- row.names(FStrawAN)
rownames(FStrawAN) <- NULL

ModFGrain4 <- glmmTMB(log(FGrain)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
FGrainAN <- glmmTMB:::Anova.glmmTMB(ModFGrain4, type="III")
FGrainAN$RowNames <- row.names(FGrainAN)
rownames(FGrainAN) <- NULL

ModFYield4 <- glmmTMB(log(Yield)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
FYieldAN <- glmmTMB:::Anova.glmmTMB(ModFYield4, type="III")
FYieldAN$RowNames <- row.names(FYieldAN)
rownames(FYieldAN) <- NULL

ModFieldPup2 <- glmer(Puptake~Treatment+(1|Block),data=Field,family=gaussian(link="log"), na.action=na.omit)
FPupAN <- Anova(ModFieldPup2, type="III")
FPupAN$RowNames <- row.names(FPupAN)
rownames(FPupAN) <- NULL

ModFieldPrec1 <- glmmTMB(Precovery~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
FPrecAN <- glmmTMB:::Anova.glmmTMB(ModFieldPrec1, type="III")
FPrecAN$RowNames <- row.names(FPrecAN)
rownames(FPrecAN) <- NULL

FPUEmod1 <- glmmTMB(PUE~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
FPUEAN <- glmmTMB:::Anova.glmmTMB(FPUEmod1, type="III")
FPUEAN$RowNames <- row.names(FPUEAN)
rownames(FPUEAN) <- NULLrownames(FPrecAN) <- NULL

ModFieldSPO41 <-  lmer(PO4~Treatment*Depth + (1|Block), data=PO4_long)
FSPO4AN <- Anova(ModFieldSPO41, tyoe="III", alpha=0.1)
FSPO4AN$RowNames <- row.names(FSPO4AN)
rownames(FSPO4AN) <- NULL

ModFieldWSP3 <- glmmTMB(sqrt(WSP)~Treatment*Depth+(1|Block), data=WSP_long_sub, family=gaussian())
FWSPAN <- glmmTMB:::Anova.glmmTMB(ModFieldWSP3, type="III")
FWSPAN$RowNames <- row.names(FWSPAN)
rownames(FWSPAN) <- NULL

ModFieldResP1 <- lmer(ResP~Treatment*Depth + (1|Block), data=ResP_long_sub)
FResPAN <- Anova(ModFieldResP1, type="III", alpha=0.1)
FResPAN$RowNames <- row.names(FResPAN)
rownames(FResPAN) <- NULL

ModFieldOC3 <- glmmTMB(OC~Treatment*Depth+(1|Block), data=OC_long_sub, family=gaussian())
FocAN <- glmmTMB:::Anova.glmmTMB(ModFieldOC3, type="III")
FocAN$RowNames <- row.names(FocAN)
rownames(FocAN) <- NULL

ModFieldpH3 <- glmmTMB(pH~Treatment*Depth+(1|Block), data=pH_long_sub, family=gaussian())
FpHAN <- glmmTMB:::Anova.glmmTMB(ModFieldpH3, type="III")
FpHAN$RowNames <- row.names(FpHAN)
rownames(FpHAN) <- NULL

ModFieldEC3 <- glmmTMB(log(EC)~Treatment*Depth+(1|Block), data=EC_long_sub, family=gaussian())
FecAN <- glmmTMB:::Anova.glmmTMB(ModFieldEC3, type="III")
FecAN$RowNames <- row.names(FecAN)
rownames(FecAN) <- NULL

ModFieldLPO4c <- glmmTMB(log(LPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
FLPO4AN <- glmmTMB:::Anova.glmmTMB(ModFieldLPO4c, type="III")
FLPO4AN$RowNames <- row.names(FLPO4AN)
rownames(FLPO4AN) <- NULL

ModFieldResPO4c <- glmmTMB(log(ResinPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
FResPO4AN <- glmmTMB:::Anova.glmmTMB(ModFieldResPO4c, type="III")
FResPO4AN$RowNames <- row.names(FResPO4AN)
rownames(FResPO4AN) <- NULL

InfilModInit2 <- glmmTMB(InitialRate~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude)
InfilInitAN <- glmmTMB:::Anova.glmmTMB(InfilModInit2, type="III")
InfilInitAN$RowNames <- row.names(InfilInitAN)
rownames(InfilInitAN) <- NULL

InfilModFinal2 <- glmmTMB(FinalRate~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude)
InfilFinalAN <- glmmTMB:::Anova.glmmTMB(InfilModFinal2, type="III")
InfilFinalAN$RowNames <- row.names(InfilFinalAN)
rownames(InfilFinalAN) <- NULL

InfilModSorp <- glmmTMB(Sorptivity~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude)
InfilSorpAN <- glmmTMB:::Anova.glmmTMB(InfilModSorp, type="III")
InfilSorpAN$RowNames <- row.names(InfilSorpAN)
rownames(InfilSorpAN) <- NULL

InfilModK <- glmmTMB(K~Treatment+(1|Block), data=Infilsub, family=gaussian(), na.action=na.exclude)
InfilKAN <- glmmTMB:::Anova.glmmTMB(InfilModK)
InfilKAN$RowNames <- row.names(InfilKAN)
rownames(InfilKAN) <- NULL


JournalANOVAtables <- list(YieldAN, PupAN, PrecAN, PUEAN, PO4AN, ResPAN, WSPAN, pHAN, ecAN, ocAN, 
                         FStrawAN, FGrainAN, FYieldAN, FPupAN, FPrecAN, FPUEAN, FSPO4AN, FWSPAN, FResPAN, FpHAN, FecAN, FocAN, FLPO4AN, FResPO4AN,
                         InfilInitAN, InfilFinalAN, InfilSorpAN, InfilKAN)
names(JournalANOVAtables) <- c("Yield", "Puptake","Precovery", "PUE", "PO4", "ResinP", "WaterSolP", "pH", "EC", "OC",
                             "Straw", "Grain", "FieldYield", "FieldPuptake", "FieldPrecovery", "FieldPUE", "FieldPO4", "FieldWaterSolP", "FieldResinP",
                             "FieldpH", "FieldEC", "FieldOC", "SnowPO4", "SnowResinPO4",
                             "Initial Rate", "Final Rate", "Sorptivity", "Hydraulic Conductivity")
write_xlsx(JournalANOVAtables, path="Journal_ANOVAtables.xlsx")

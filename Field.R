# Loading data in to R  & Summaries ----
  Field<-read.csv("Field.csv", fileEncoding="UTF-8-BOM") 
  View(Field)
  #Fieldsplitraw<-read.csv("FieldSplitRaw.csv", fileEncoding="UTF-8-BOM") # to combine residuals if necessary
  Fieldraw <- read.csv("Fieldraw.csv", fileEncoding="UTF-8-BOM") # includes split data
  
## Loading libraries ----
  library(summarytools) # get the mean, media, skewness, SD, Min/Max, CV, etc. for a dataset
  library(lme4)
  library(nlme)
  library(lmerTest)
  library(glmmTMB)
  library(doBy)
  library(ggplot2)
  library(ggpattern)
  library(statsExpressions) # needed to run ggstatsplot
  library(ggstatsplot) #combo violin plots
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
  library(e1071) #skewness and kurtosis
  library(moments) #skewness and kurtosis
  library(rsq)
  library(pgirmess)
  library(dunn.test)
  library(directlabels)
  library(writexl)
  library(glmmTMB)
  library(stringr)
  library(cowplot) # for combining multiple ggplots instead of par(mfrow=c(x,x))
  library(openxlsx) # to create workbooks for covriance heatmaps
  library(RColorBrewer)
  library(FactoMineR) # for PCA
  library(factoextra) # for PCA
  
## Combining 0-30cm values   ----
  ## Calculate mean of soil data from incremental depths to combined depth (0-30cm)
  FieldTrt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus")
    Fieldsplitraw$Block <- factor(Fieldsplitraw$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
    Fieldsplitraw$Treatment <- factor(Fieldsplitraw$Treatment,levels=Trt_order)
    Fieldsplitraw$Plot <- factor(Fieldsplitraw$Plot)
    Fieldsplitraw$NO3 <- as.numeric(as.character(Fieldsplitraw$NO3))
    Fieldsplitraw$PO4 <- as.numeric(as.character(Fieldsplitraw$PO4))
    Fieldsplitraw$WatSolP <- as.numeric(as.character(Fieldsplitraw$WatSolP))
    Fieldsplitraw$ResinP <- as.numeric(as.character(Fieldsplitraw$ResinP))
    Fieldsplitraw$pH <- as.numeric(as.character(Fieldsplitraw$pH))
    Fieldsplitraw$EC <- as.numeric(as.character(Fieldsplitraw$EC))
    Fieldsplitraw$OC <- as.numeric(as.character(Fieldsplitraw$OC))
    summary(Field)
    str(Field) #displays the structure of the object
  ## Settiing up a new data frame containing combined values
    FieldResGroup <- Fieldsplitraw %>%
      group_by(Plot, Treatment, Block)
    FieldResid <- FieldResGroup %>%
      summarise(c(NO3c	= mean(NO3)), (PO4c=mean(PO4)), (WatSolPc=mean(WatSolP)), (ResinPc =mean(ResinP)),
                  (pHc=mean(pH)), (ECc=mean(EC)), (OCc=mean(OC)))
    View(FieldResid)
    write_xlsx(FieldResid, path="FieldResidMean.xlsx")


## Summary and ordering of data   ----
  #Check for missing values in a specific field
    missing <- colSums(is.na(Field[,]))
    missing <- colSums(is.na(Fieldraw[,]))
    print(missing)
  #Change columns in a dataframe to factors/categorical values, str displays 
    FieldTrt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus")
    Field$Block <- factor(Field$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
    Field$Treatment <- factor(Field$Treatment,levels=FieldTrt_order)
    Field$LNO3 <- as.numeric(Field$LNO3)
    Field$LNH4 <- as.numeric(Field$LNH4)
    Field$LPO4 <- as.numeric(Field$LPO4)
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
    Field_stats <- function(data, variable_names) { #uses 'moments' instead of 'e1071' which results in much higher kurtosis
      stats <- data %>%
        summarise_at(vars(all_of(variable_names)), list(skewness = ~ skewness(., na.rm = TRUE), kurtosis = ~ kurtosis(., na.rm = TRUE)))
      return(stats)
    } # use later during skewness and kurtosis determination

    
##  Check for outliers   ----
  #Straw
    ggplot(Fieldraw, aes(x=Treatment, y=FStraw, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Straw")
    ggsave("OutliersField_CanStraw24.jpg", width=10, height=10, dpi=200)
  #Grain
    ggplot(Fieldraw, aes(x=Treatment, y=FGrain, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Grain")
    ggsave("OutliersField_CanGrain24.jpg", width=10, height=10, dpi=200)
  # Volumetric Moisture Content - Wet sample
    ggplot(Fieldraw, aes(x=Treatment, y=MoistWet, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="MoistWet")
    ggsave("OutliersField_VMCwet.jpg", width=10, height=10, dpi=200)
  # Volumetric Moisture Content - drysample
    ggplot(Fieldraw, aes(x=Treatment, y=MoistDry, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="MoistDry")
    ggsave("OutliersField_VMCdry.jpg", width=10, height=10, dpi=200)
  # Bulk Density - wet
    ggplot(Fieldraw, aes(x=Treatment, y=BDwet, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="BDwet")
    ggsave("OutliersField_BDwet.jpg", width=10, height=10, dpi=200)
  # Bulk Density - dry
    ggplot(Fieldraw, aes(x=Treatment, y=BDdry, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="BDdry")
    ggsave("OutliersField_BDdry.jpg", width=10, height=10, dpi=200)
  # Nuptake
    ggplot(Fieldraw, aes(x=Treatment, y=Nuptake, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="N uptake")
    ggsave("OutliersField_Nuptake.jpg", width=10, height=10, dpi=200)
  # N recovery
    ggplot(Fieldraw, aes(x=Treatment, y=Nrecovery, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="N recovery")
    ggsave("OutliersField_Nrecovery.jpg", width=10, height=10, dpi=200)
  # NUE
    ggplot(Fieldraw, aes(x=Treatment, y=NUE, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="N use efficiency")
    ggsave("OutliersField_NUE.jpg", width=10, height=10, dpi=200)
  # P uptake
    ggplot(Fieldraw, aes(x=Treatment, y=Puptake, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="P uptake")
    ggsave("OutliersField_Puptake.jpg", width=10, height=10, dpi=200)
  # P recovery
    ggplot(Fieldraw, aes(x=Treatment, y=Precovery, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="P recovery")
    ggsave("OutliersField_Precovery.jpg", width=10, height=10, dpi=200)
  # PUE
    ggplot(Fieldraw, aes(x=Treatment, y=PUE, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="P use efficiency")
    ggsave("OutliersField_PUE.jpg", width=10, height=10, dpi=200)
  # NO3_10
    ggplot(Fieldraw, aes(x=Treatment, y=NO3_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="NO3")
    ggsave("OutliersField_NO3_10.jpg", width=10, height=10, dpi=200)
  # PO4_10
    ggplot(Fieldraw, aes(x=Treatment, y=PO4_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="PO4")
    ggsave("OutliersField_PO4_10.jpg", width=10, height=10, dpi=200)
  # WatSolP_10
    ggplot(Fieldraw, aes(x=Treatment, y=WatSolP_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Water Soluble P")
    ggsave("OutliersField_WatSolP_10.jpg", width=10, height=10, dpi=200)
  # ResinP_10
    ggplot(Fieldraw, aes(x=Treatment, y=ResinP_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="ResinP")
    ggsave("OutliersField_ResinP_10.jpg", width=10, height=10, dpi=200)
  # pH_10
    ggplot(Fieldraw, aes(x=Treatment, y=pH_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="pH")
    ggsave("OutliersField_pH_10.jpg", width=10, height=10, dpi=200)
  # EC_10
    ggplot(Fieldraw, aes(x=Treatment, y=EC_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="EC")
    ggsave("OutliersField_EC_10.jpg", width=10, height=10, dpi=200)
  # OC_10
    ggplot(Fieldraw, aes(x=Treatment, y=OC_10, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="OC")
    ggsave("OutliersField_OC_10.jpg", width=10, height=10, dpi=200)
  # NO3_20
    ggplot(Fieldraw, aes(x=Treatment, y=NO3_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="NO3")
    ggsave("OutliersField_NO3_20.jpg", width=10, height=10, dpi=200)
  # PO4_20
    ggplot(Fieldraw, aes(x=Treatment, y=PO4_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="PO4")
    ggsave("OutliersField_PO4_20.jpg", width=10, height=10, dpi=200)
  # WatSolP_20
    ggplot(Fieldraw, aes(x=Treatment, y=WatSolP_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Water Soluble P")
    ggsave("OutliersField_WatSolP_20.jpg", width=10, height=10, dpi=200)
  # ResinP_20
    ggplot(Fieldraw, aes(x=Treatment, y=ResinP_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="ResinP")
    ggsave("OutliersField_ResinP_20.jpg", width=10, height=10, dpi=200)
  # pH_20
    ggplot(Fieldraw, aes(x=Treatment, y=pH_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="pH")
    ggsave("OutliersField_pH_20.jpg", width=10, height=10, dpi=200)
  # EC_20
    ggplot(Fieldraw, aes(x=Treatment, y=EC_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="EC")
    ggsave("OutliersField_EC_20.jpg", width=10, height=10, dpi=200)
  # OC_20
    ggplot(Fieldraw, aes(x=Treatment, y=OC_20, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="OC")
    ggsave("OutliersField_OC_20.jpg", width=10, height=10, dpi=200)
  # NO3_30
    ggplot(Fieldraw, aes(x=Treatment, y=NO3_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="NO3")
    ggsave("OutliersField_NO3_30.jpg", width=10, height=10, dpi=200)
  # PO4_30
    ggplot(Fieldraw, aes(x=Treatment, y=PO4_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="PO4")
    ggsave("OutliersField_PO4_30.jpg", width=10, height=10, dpi=200)
  # WatSolP_30
    ggplot(Fieldraw, aes(x=Treatment, y=WatSolP_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Water Soluble P")
    ggsave("OutliersField_WatSolP_30.jpg", width=10, height=10, dpi=200)
  # ResinP_30
    ggplot(Fieldraw, aes(x=Treatment, y=ResinP_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="ResinP")
    ggsave("OutliersField_ResinP_30.jpg", width=10, height=10, dpi=200)
  # pH_30
    ggplot(Fieldraw, aes(x=Treatment, y=pH_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="pH")
    ggsave("OutliersField_pH_30.jpg", width=10, height=10, dpi=200)
  # EC_30
    ggplot(Fieldraw, aes(x=Treatment, y=EC_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="EC")
    ggsave("OutliersField_EC_30.jpg", width=10, height=10, dpi=200)
  # OC_30
    ggplot(Fieldraw, aes(x=Treatment, y=OC_30, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="OC")
    ggsave("OutliersField_OC_30.jpg", width=10, height=10, dpi=200)
  # NO3Load	
    ggplot(Fieldraw, aes(x=Treatment, y=NO3Load, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="NO3Load")
    ggsave("OutliersField_NO3Load.jpg", width=10, height=10, dpi=200)
  # NH4Load	
    ggplot(Fieldraw, aes(x=Treatment, y=NH4Load	, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="NH4Load	")
    ggsave("OutliersField_NH4Load.jpg", width=10, height=10, dpi=200)
  # PO4Load	
    ggplot(Fieldraw, aes(x=Treatment, y=PO4Load, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="PO4Load")
    ggsave("OutliersField_PO4Load.jpg", width=10, height=10, dpi=200) 
  # ResinPO4	
    ggplot(Fieldraw, aes(x=Treatment, y=ResinPO4, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="ResinPO4")
    ggsave("OutliersField_ResinPO4.jpg", width=10, height=10, dpi=200)
  # ResinNO3
    ggplot(Fieldraw, aes(x=Treatment, y=ResinNO3, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="ResinNO3")
    ggsave("OutliersField_ResinNO3.jpg", width=10, height=10, dpi=200)


# PLANT ANALYSIS ----  
##   Straw   ----
  FieldStraw_Mean <- summary_by(FStraw~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE))
  FieldStraw_Mean <- as.numeric(FieldStraw_Mean$FStraw, na.rm=TRUE)
  FieldStraw_skew <- skewness(FieldStraw_Mean, na.rm=TRUE)
  FieldStraw_kur <- kurtosis(FieldStraw_Mean, na.rm=TRUE)
  cat("Skewness:", FieldStraw_skew, "\n") # 0.7782955 
  cat("Kurtosis:", FieldStraw_kur, "\n") # -0.4127568 
  ggplot(Field, aes(x=Treatment, y=FStraw, fill=Treatment)) + geom_boxplot() + labs(x="Treatment", y="Straw")
  hist(Field$FStraw) #  left skew
  shapiro.test(Field$FStraw) # p=0.05474
  leveneTest(FStraw~Treatment, data=Field)  # P=0.03911
# transform
  hist(log(Field$FStraw)) #  left skew
  shapiro.test(log(Field$FStraw)) # p=0.3666
  leveneTest(log(FStraw)~Treatment, data=Field)  # P=0.1384
# ModFStraw1
  ModFStraw1<- lmer(FStraw~Treatment+(1|Block),data=Field)
  anova(ModFStraw1) # no significant differences
  summary(ModFStraw1)
  hist(resid(ModFStraw1)) 
  shapiro.test(resid(ModFStraw1))  # 0.7146
  plot(fitted(ModFStraw1),resid(ModFStraw1),pch=16)   
  qqnorm(resid(ModFStraw1)) # medium left tail
  qqline(resid(ModFStraw1))
  rsq(ModFStraw1) # 0.4246
# ModFStraw2
  ModFStraw2 <- glmer(FStraw~Treatment+(1|Block),data=Field,family=Gamma(link="log"), na.action=na.omit)
  anova(ModFStraw2) # no significant differences
  summary(ModFStraw2)
  shapiro.test(resid(ModFStraw2)) # p=0.7505
  bf.test(FStraw~Treatment, data=Field) # 0.80 
  rsq.glmm(ModFStraw2)
# ModFStraw3
  ModFStraw3 <- lme(FStraw~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
  anova(ModFStraw3)  # no significant differences
  summary(ModFStraw3)
  rsq.lmm(ModFStraw3)
# ModFStraw4
  ModFStraw4 <- glmmTMB(FStraw~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFStraw4, type="III") # no significant differences
  summary(ModFStraw4)
  performance::r2(ModFStraw4) # 0.502
#Comparing models - highest rsq and lowest AIC?BIC=ModFStraw2
  # cannot get most rsq values so this wasn't assessed
  Fstraw_modlist <- list(ModFStraw1, ModFStraw2, ModFStraw3, ModFStraw4)
  AIC_values <- sapply(Fstraw_modlist, AIC)
  BIC_values <- sapply(Fstraw_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFStraw1", "ModFStraw2", "ModFStraw3", "ModFStraw4"), AIC_values, BIC_values)
  print(N_AB)
    #       Model AIC_values BIC_values
    #1 ModFStraw1  286.4133   295.4973 - has fractional df
    #2 ModFStraw2  358.49187  367.57582 - has infinite df
    #3 ModFStraw3  286.41332  293.07903 - has df of only 3
    #4 ModFStraw4  363.98722  373.07118
# emmeans 
  ModFStrawem <- emmeans(ModFStraw4,~Treatment, alpha=0.1)
  ModFStrawem_cld <- cld(ModFStrawem, Letters=trimws(letters), reversed=TRUE) 
  View(ModFStrawem_cld)
  write_xlsx(ModFStrawem_cld, path="Field_Straw.xlsx")



##   Grain   ----
  FieldGrain_Mean <- summary_by(FGrain~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE))
  FieldGrain_Mean <- as.numeric(FieldGrain_Mean$FGrain)
  FieldGrain_skew <- skewness(FieldGrain_Mean, na.rm=TRUE)
  FieldGrain_kur <- kurtosis(FieldGrain_Mean, na.rm=TRUE)
  cat("Skewness:", FieldGrain_skew, "\n") # 1.589016 
  cat("Kurtosis:", FieldGrain_kur, "\n") # 2.121085 
  hist(FieldGrain_Mean) # severe left skew
  shapiro.test(FieldGrain_Mean) # p=0.0004853
  leveneTest(FGrain~Treatment, data=Field) # 0.0413
# transform
  hist(log(FieldGrain_Mean)) # severe left skew
  shapiro.test(log(FieldGrain_Mean)) # p=0.1371
  leveneTest(log(FGrain)~Treatment, data=Field) # 0.04796 
  hist(sqrt(FieldGrain_Mean)) # severe left skew
  shapiro.test(sqrt(FieldGrain_Mean)) # p=0.009663
  leveneTest(sqrt(FGrain)~Treatment, data=Field) # 0.03847
#ModFGrain1 
  ModFGrain1<- lmer(log(FGrain)~Treatment+(1|Block),data=Field)
  anova(ModFGrain1) # no significant differences
  summary(ModFGrain1)
  hist(resid(ModFGrain1)) # normal
  shapiro.test(resid(ModFGrain1))  # 0.8632
  plot(fitted(ModFGrain1),resid(ModFGrain1),pch=16)   # random
  qqnorm(resid(ModFGrain1)) # small left tail
  qqline(resid(ModFGrain1))
  rsq(ModFGrain1) # 0.5477
  # ModFGrain2
  ModFGrain2 <- glmer(log(FGrain)~Treatment+(1|Block),data=Field,family=Gamma(link="log"), na.action=na.omit)
  anova(ModFGrain2) # no significant differences
  summary(ModFGrain2)
  shapiro.test(resid(ModFGrain2)) # p=0.557
  bf.test(FGrain~Treatment, data=Field) # 0.656
  rsq(ModFGrain2)
  # ModFGrain3
  ModFGrain3 <- lme(log(FGrain)~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
  anova(ModFGrain3)  # no significant differences
  summary(ModFGrain3)
  rsq.lmm(ModFGrain3)
  # ModFGrain4
  ModFGrain4 <- glmmTMB(log(FGrain)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFGrain4, type="III") # no significant differences
  summary(ModFGrain4)
  performance::r2(ModFGrain4) # 0.502
#Comparing models - decent rsq and AIC?BIC=ModFGrain4
  # cannot get most rsq values so this wasn't assessed
  FGrain_modlist <- list(ModFGrain1, ModFGrain2, ModFGrain3, ModFGrain4)
  AIC_values <- sapply(FGrain_modlist, AIC)
  BIC_values <- sapply(FGrain_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFGrain1", "ModFGrain2", "ModFGrain3", "ModFGrain4"),
                     AIC_values, BIC_values)
  print(N_AB)
    #Model AIC_values BIC_values
    #1 ModFGrain1   38.12774   47.21170
    #2 ModFGrain2   23.74094   32.82489 - results in Infinite df in emmeans
    #3 ModFGrain3   38.12774   44.79345
    #4 ModFGrain4   28.04527   37.12923
#emmeans 
  ModFGrainem <- emmeans(ModFGrain4,~Treatment, alpha=0.1, type="response")
  ModFGrainem_cld <- cld(ModFGrainem, Letters=trimws(letters), reversed=TRUE, type="response") 
  ModFGrainem_cld <- ModFGrainem_cld %>% dplyr::rename(emmean="response")
  View(ModFGrainem_cld)
  write_xlsx(ModFGrainem_cld, path="Field_Grain.xlsx")


##   Biomass   ----
  FieldYield_Mean <- summary_by(Yield~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE))
  FieldYield_Mean <- as.numeric(FieldYield_Mean$Yield)
  FieldYield_skew <- skewness(FieldYield_Mean, na.rm=TRUE)
  FieldYield_kur <- kurtosis(FieldYield_Mean, na.rm=TRUE)
  cat("Skewness:", FieldYield_skew, "\n") # 0.8933767 
  cat("Kurtosis:", FieldYield_kur, "\n") # -0.3604379 
  hist(FieldYield_Mean) # moderate left skew
  shapiro.test(FieldYield_Mean) # p=0.01478
  leveneTest(Yield~Treatment, data=Field) # 0.09011
  # transform
  hist(log(FieldYield_Mean)) # mild left skew
  shapiro.test(log(FieldYield_Mean)) # p=0.2559
  leveneTest(log(Yield)~Treatment, data=Field) # 0.09588
  hist(sqrt(FieldYield_Mean)) # mild left skew
  shapiro.test(sqrt(FieldYield_Mean)) # p=0.06997
  leveneTest(sqrt(Yield)~Treatment, data=Field) # 0.09459
  #ModFYield1 
  ModFYield1<- lmer(log(Yield)~Treatment+(1|Block),data=Field)
  anova(ModFYield1) # no significant differences
  summary(ModFYield1)
  hist(resid(ModFYield1)) # normal
  shapiro.test(resid(ModFYield1))  # 0.1307
  plot(fitted(ModFYield1),resid(ModFYield1),pch=16)   # random
  qqnorm(resid(ModFYield1)) # medium left tail
  qqline(resid(ModFYield1))
  rsq(ModFYield1) # 0.5177
  # ModFYield2
  ModFYield2 <- glmer(log(Yield)~Treatment+(1|Block),data=Field,family=Gamma(link="log"), na.action=na.omit)
  anova(ModFYield2) # no significant differences
  summary(ModFYield2)
  shapiro.test(resid(ModFYield2)) # p=0.249
  bf.test(Yield~Treatment, data=Field) # 0.7566
  rsq(ModFYield2) #0.629
  # ModFYield3
  ModFYield3 <- lme(log(Yield)~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
  anova(ModFYield3)  # no significant differences
  summary(ModFYield3)
  rsq.lmm(ModFYield3) #0.5177
  # ModFYield4
  ModFYield4 <- glmmTMB(log(Yield)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFYield4, type="III") # no significant differences
  summary(ModFYield4)
  performance::r2(ModFYield4) # 0.594
  #Comparing models
  # Rsq = ModFYield2 is highest at 0.629
    # AIC & BIC
    FYield_modlist <- list(ModFYield1, ModFYield2, ModFYield3, ModFYield4)
    AIC_values <- sapply(FYield_modlist, AIC)
    BIC_values <- sapply(FYield_modlist, BIC)
    N_AB <- data.frame(Model=c("ModFYield1", "ModFYield2", "ModFYield3", "ModFYield4"),
                       AIC_values, BIC_values)
    print(N_AB)
      #Model AIC_values BIC_values
      #1 ModFYield1  24.585345   34.00978
      #2 ModFYield2   6.006701   15.43113 - Infinite DF
      #3 ModFYield3  24.585345   31.70832
      #4 ModFYield4   9.452402   18.87683
  # emmeans 
  ModFYieldem <- emmeans(ModFYield4,~Treatment, alpha=0.1, type="response")
  ModFYieldem_cld <- cld(ModFYieldem, Letters=trimws(letters), reversed=TRUE, type="response") 
  ModFYieldem_cld <- ModFYieldem_cld %>% dplyr::rename(emmean="response")
  View(ModFYieldem_cld)
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
    View(BiomassFieldEm)
    print(BiomassFieldEm)
    write_xlsx(BiomassFieldEm, path="Field_BiomassMeans.xlsx")
    ## Visualization
    ### Set text labels colours & fontface
    YieldColour <- c("FGrain" = "white", "FStraw" = "black")
    TotLetMax <- aggregate(u_conf ~ Treatment, data = BiomassFieldEm, FUN = max)
    (FieldYield1 <- ggplot(BiomassFieldEm, aes(Treatment, y=emmean, fill=origin)) +
        geom_bar(stat="identity", position = "stack", width=0.65) +
        geom_errorbar(aes(ymin=l_conf, ymax=u_conf), width = .15, stat="identity")+
        geom_text(aes(label = trimws(.group), y = ifelse(origin == "FStraw", emmean, emmean), color = origin), #color specified within aes layer
                  position = position_stack(vjust = 0.5), hjust = -1, size = 7, fontface="bold") +
        geom_text(data=TotLetMax, (aes(x=Treatment, y = u_conf+150, label = "A")),
                  fontface="bold", color="black", size = 7, inherit.aes = FALSE) +
        labs(x = "Treatment", y = "Grain and straw yields (kg/ha)") +
        scale_fill_manual(values = c("grey45", "grey89"), labels = c("Grain", "Straw"))+
        scale_color_manual(values = YieldColour, guide="none") + # need to specify geom_text colur and guide removes associated legend
        scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kg P/ha", "Biochar 10t/ha", "Biochar 10t/ha\n& TS",
                                    "TSP\nFertilizer"))+
        theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
              legend.title = element_blank(), legend.text=element_text(size=18, face="bold"),
              axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
              axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
              axis.title.x=element_blank(), 
              axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
        ggsave(FieldYield1, file="Field_Yield1.jpg", width = 10, height = 8, dpi = 150)
    
  # Plot option 2 - using ordinary means
    ## call dataframe
    BiomassField_Manual <- subset(Field, select=c("Treatment", "FGrain", "FStraw"), subset = complete.cases(Field[, c("FGrain", "FStraw")]))
    BiomassField_Manual$Treatment <- recode(BiomassField_Manual$Treatment,
                                            "Control1" = "Control 1", "Control2" = "Control 2", "Biochar25kgPha" = "Biochar 25kg P/ha",
                                            "Biochar10tha" = "Biochar 10t/ha", "Biochar10thaTSP" = "Biochar 10t/ha\n& TSP",
                                            "Phosphorus" = "TSP\nFertilizer")
    print(BiomassField_Manual)
    ## Visualization
    (FieldYield2 <- BiomassField_Manual|>
        pivot_longer(-Treatment) |>
        mutate(name = forcats::fct_relevel(name, "FGrain", "FStraw")) |>
        ggbarplot(x = "Treatment", y = "value", fill = "name", add = "mean_se") +
        labs(x = "Treatment", y = "Total grain and straw yield (kg/ha)", fill = "") +
        scale_fill_manual(values = c("grey30", "grey89"), labels = c("Grain", "Straw")) +
        scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar 25kg P/ha", "Biochar 10t/ha", "Biochar 10t/ha\n& TS",
                                    "TSP\nFertilizer"))+
        theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
              legend.title = element_blank(), legend.text=element_text(size=18),
              axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
              axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
              axis.title.x=element_blank(), 
              axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(FieldYield2, file="Field_Yield2.jpg", width = 10, height = 8, dpi = 150)


##   N uptake   ----
  FieldNup_Mean <- summary_by(Nuptake~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
  FieldNup_Mean <- as.numeric(FieldNup_Mean$Nuptake)
  FieldNup_skew <- skewness(FieldNup_Mean,na.rm=TRUE)
  FieldNup_kur <- kurtosis(FieldNup_Mean,na.rm=TRUE)
  cat("Skewness:", FieldNup_skew, "\n") # 0.7944
  cat("Kurtosis:", FieldNup_kur, "\n") # 1.405 
  shapiro.test(Field$Nuptake) # p=0.02145
  hist(Field$Nuptake) #  missing pieces to right
  leveneTest(Nuptake~Treatment, data=Field)  # 0.4566
# transform
  shapiro.test(log(Field$Nuptake)) # p=0.06483
  hist(log(Field$Nuptake)) # more towards right, less 'missing'  
  leveneTest(log(Nuptake)~Treatment, data=Field)  # 0.559
# ModFieldNup1
  ModFieldNup1 <- lmer(log(Nuptake)~Treatment+(1|Block),data=Field)
  anova(ModFieldNup1) # significant differences p=0.0075
  summary(ModFieldNup1)
  hist(resid(ModFieldNup1)) # normal but flat
  shapiro.test(resid(ModFieldNup1))  # 0.393
  plot(fitted(ModFieldNup1),resid(ModFieldNup1),pch=16)   # random, right skew
  qqnorm(resid(ModFieldNup1)) # medium right tail
  qqline(resid(ModFieldNup1))
  rsq(ModFieldNup1) # 0.583
# ModFieldNup2
  ModFieldNup2 <- glmer(log(Nuptake)~Treatment+(1|Block),data=Field,family=gaussian(link="log"), na.action=na.omit)
  anova(ModFieldNup2) # significant differences
  summary(ModFieldNup2)
  shapiro.test(resid(ModFieldNup2)) # p=0.3776
  bf.test(Nuptake~Treatment, data=Field) # 0.0739
  rsq(ModFieldNup2) #0.634
# ModFieldNup3
  ModFieldNup3 <- glmmTMB(log(Nuptake)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFieldNup3, type="III") # significant differences p=4.6e-6
  summary(ModFieldNup3)
  performance::r2(ModFieldNup3) # 0.594
# ModFieldNup4
  ModFieldNup4 <- lme(log(Nuptake)~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
  anova(ModFieldNup4)  # no significant differences
  summary(ModFieldNup4)
  rsq.lmm(ModFieldNup4) #0.609
#Comparing models
  # Rsq = ModFieldNup2 is highest followed by ModFieldNup4
  # AIC & BIC
  FNup_modlist <- list(ModFieldNup1, ModFieldNup2, ModFieldNup3, ModFieldNup4)
  AIC_values <- sapply(FNup_modlist, AIC)
  BIC_values <- sapply(FNup_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldNup1", "ModFieldNup2", "ModFieldNup3", "ModFieldNup4"),
                     AIC_values, BIC_values)
  print(N_AB)
      # Model AIC_values BIC_values
      #1 ModFieldNup1  24.585345   34.00978  - fractional df
      #2 ModFieldNup2   6.006701   15.43113  - infinite df
      #3 ModFieldNup3  24.585345   31.70832  - 3rd best AIC/BIC and only one with no df issues in emmeans
      #4 ModFieldNup4   9.452402   18.87683  - infinite df
#emmeans 
  ModFieldNupem <- emmeans(ModFieldNup2,~Treatment, alpha=0.1)
  ModFieldNupem_cld <- cld(ModFieldNupem, Letters=trimws(letters), reversed=TRUE, type="response") 
  ModFieldNupem_cld$Treatment <- factor(ModFieldNupem_cld$Treatment, levels = FieldTrt_order)
  print(ModFieldNupem_cld)
  write_xlsx(ModFieldNupem_cld, path="Field_Nuptake.xlsx")
# Visualizations
  (FieldNup_plot <- ggplot(ModFieldNupem_cld, aes(x=Treatment, y=response))+
      geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                     pattern_density=0.05, pattern_spacing=0.01, width=0.65)+
      geom_errorbar(aes(ymin=response - SE, ymax=response + SE), 
                    width=0.2, position=position_dodge(width=0.9)) +
      geom_text(aes(label=trimws(.group), y=response+SE), size=8, vjust=-1) +
      labs(x="Treatments", y="Canola nitrogen uptake (mg/kg)") +
      scale_x_discrete(labels=c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                  "TSP\nFertilizer"))+
      scale_y_continuous(limits=c(0, 85))+
      theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
            legend.title = element_blank(), legend.text=element_text(size=18),
            axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
            axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
            axis.title.x=element_blank(), 
            axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
            panel.background = element_blank(),
            panel.border=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
  ggsave(FieldNup_plot, file="Field_Nuptake.jpg", width=8, height=8, dpi=150)


##   N recovery   ----
  FieldNrec_Mean <- summary_by(Nrecovery~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
  FieldNrec_Mean <- as.numeric(FieldNrec_Mean$Nrecovery)
  FieldNrec_skew <- skewness(FieldNrec_Mean,na.rm=TRUE)
  FieldNrec_kur <- kurtosis(FieldNrec_Mean,na.rm=TRUE)
  cat("Skewness:", FieldNrec_skew, "\n") # 1.9169
  cat("Kurtosis:", FieldNrec_kur, "\n") # 3.536
  shapiro.test(Field$Nrecovery) # p=0.000294
  hist(Field$Nrecovery) #  severe left skew
  leveneTest(Nrecovery~Treatment, data=Field)  # 0.2904
# Nrecovery has missing values for Control1 
  FNrec_out <- Field[complete.cases(Field$Nrecovery),] #set up a subset removing the missing data from column Nrecovery
  FNrec_out$Block <- as.factor(FNrec_out$Block)
  FNrec_out$Treatment <- factor(FNrec_out$Treatment, levels=FieldTrt_order)
  print(FNrec_out)
  plot(Nrec_out$Nrecovery)
  leveneTest(Nrecovery~Treatment, data=FNrec_out) # 0.2904
  hist(FNrec_out$Nrecovery) #  severe left skew
  shapiro.test(FNrec_out$Nrecovery) #0.000294
# transform - produces NaNs when tested or used in model, transformation thus not used
  shapiro.test(log(FNrec_out$Nrecovery)) # p=0.7052
  hist(log(FNrec_out$Nrecovery)) #  normal
  leveneTest(log(Nrecovery)~Treatment, data=FNrec_out)  # 0.2388
  shapiro.test(sqrt(FNrec_out$Nrecovery)) # p=0.0458
  hist(sqrt(FNrec_out$Nrecovery)) #  slight left skew
  leveneTest(sqrt(Nrecovery)~Treatment, data=FNrec_out)  # 0.4403
# models - tried glmer (needs a valid starting point) and lmer (signularity issues).
# ModFieldNrec1 - it showed singularity issues when running the rsq to do with zero variance components
  ModFieldNrec1 <- glmmTMB(Nrecovery~Treatment+(1|Block), data=FNrec_out, family=gaussian(),
                           control = glmmTMBControl(optCtrl = list(iter.max=5000, eval.max=4000)))
  glmmTMB:::Anova.glmmTMB(ModFieldNrec1, type="III") # no significant differences 
  summary(ModFieldNrec1)
  performance::r2(ModFieldNrec1) # NA for full model - singularity with random effects
  # ModFieldNrec2 - has only 3 degrees of freedom
  ModFieldNrec2 <- lme(Nuptake~Treatment,random=~1|Block, data=FNrec_out)
  anova(ModFieldNrec2)  #  significant differences
  summary(ModFieldNrec2)
  rsq.lmm(ModFieldNrec2) #0.179
# Comparing models
  # Rsq = 
  # AIC & BIC
  FNrec_modlist <- list(ModFieldNrec1, ModFieldNrec2)
  AIC_values <- sapply(FNrec_modlist, AIC)
  BIC_values <- sapply(FNrec_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldNrec1", "ModFieldNrec2"), AIC_values, BIC_values)
  print(N_AB)
    # Model AIC_values BIC_values
    #1 ModFieldNrec1  165.2383   172.2084 - best AIC/BIC model but with singularity issues it is not the best choice
    #2 ModFieldNrec2  142.2151   147.1715
#emmeans 
  (ModFieldNrecem <- emmeans(ModFieldNrec2,~Treatment, alpha=0.1)) # can also do model,~Soil or other
  #ModFieldNrecem_contrast <- contrast(ModFieldNrecem, method = "tukey") # obtain t & p values, removes upper & lower conf int data
  (ModFieldNrecem_cld <- cld(ModFieldNrecem, Letters=letters, type="response", reversed=TRUE))
  View(ModFieldNrecem_cld)
  write_xlsx(ModFieldNrecem_cld, path="Field_Nrecovery.xlsx")
# Visualizations
    (FielNrecPlot <- ggplot(ModFieldNrecem_cld, aes(x=Treatment, y=emmean, fill=Treatment)) +
      geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                       pattern_density=0.05, pattern_spacing=0.01, width = 0.65)+
      geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                    width=0.2, position=position_dodge(width=0.9)) +
      geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1) +
      labs(x="Treatments", y="Canola nitrogen recovery (%)") +
      scale_x_discrete(labels=c("Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "Fertilizer\nPhosphorus"))+
      scale_y_continuous(limits=c(0, 90))+
      theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
          legend.title = element_blank(), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
  ggsave(FielNrecPlot, file="Field_Nreocvery.jpg", width=8, height=8, dpi=150)



##   P uptake  ----
  FieldPup_Mean <- summary_by(Puptake~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
  FieldPup_Mean <- as.numeric(FieldPup_Mean$Puptake)
  FieldPup_skew <- skewness(FieldPup_Mean,na.rm=TRUE)
  FieldPup_kur <- kurtosis(FieldPup_Mean,na.rm=TRUE)
  cat("Skewness:", FieldPup_skew, "\n") # 0.01110207 
  cat("Kurtosis:", FieldPup_kur, "\n") #  -1.097268 
  shapiro.test(Field$Puptake) # p=0.6779
  hist(Field$Puptake) # Normalish
  leveneTest(Puptake~Treatment, data=Field)  # 0.4865
#ModFieldPup1
  ModFieldPup1 <- glmmTMB(Puptake~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFieldPup1, type="III") # no significant differences
  summary(ModFieldPup1)
  performance::r2(ModFieldPup1) # 0.45
  # ModFieldPup2
  ModFieldPup2 <- glmer(Puptake~Treatment+(1|Block),data=Field,family=gaussian(link="log"), na.action=na.omit)
  anova(ModFieldPup2) # no significant differences in block
  coef(summary(ModFieldPup2))[, "Pr(>|z|)"]
  summary(ModFieldPup2)
  shapiro.test(resid(ModFieldPup2)) # p=0.522
  bf.test(Puptake~Treatment, data=Field) # 0.777
  rsq(ModFieldPup2) #  0.512
  # ModFieldPup3
  ModFieldPup3 <- lmer(Puptake~Treatment+(1|Block),data=Field)
  anova(ModFieldPup3) # no significant differences
  summary(ModFieldPup3)
  hist(resid(ModFieldPup3)) # normal
  shapiro.test(resid(ModFieldPup3))  # 0.771
  plot(fitted(ModFieldPup3),resid(ModFieldPup3),pch=16)   # random
  qqnorm(resid(ModFieldPup3)) # small  tails
  qqline(resid(ModFieldPup3))
  rsq::rsq(ModFieldPup3) # 0.37
  # ModFieldPup4
  ModFieldPup4 <- lme(Puptake~Treatment,random=~1|Block, data=Field, na.action=na.exclude)
  anova(ModFieldPup4)  # no significant differences
  summary(ModFieldPup4)
  rsq.lmm(ModFieldPup4) #0.37
#Comparing models
  # Rsq = ModFieldPup2 (glmer) is highest followed by ModFieldPup1 (glmm)
  # AIC & BIC
  FPup_modlist <- list(ModFieldPup1, ModFieldPup2, ModFieldPup3, ModFieldPup4)
  AIC_values <- sapply(FPup_modlist, AIC)
  BIC_values <- sapply(FPup_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldPup1", "ModFieldPup2", "ModFieldPup3", "ModFieldPup4"),
                     AIC_values, BIC_values)
  print(N_AB)
  # Model AIC_values BIC_values
  #1 ModFieldPup1   92.20457  101.62900 - looks reasonable in emmeans
  #2 ModFieldPup2   94.53165  103.95608 - emmeans resulted in iinfinite df
  #3 ModFieldPup3   86.64947   96.07390 - fractional df in emmeans
  #4 ModFieldPup4   86.64947   93.77244 - only 3 df in emmeans
#emmeans - chosen ModFieldPup1 as second highest rsq, reasonable AIC/BIC and seems to be fine in emmeans function
  ModFieldPupem <- emmeans(ModFieldPup1,~Treatment, alpha=0.1)
  ModFieldPupem_cld <- cld(ModFieldPupem, Letters=letters, reversed=TRUE, type="response") 
  View(ModFieldPupem_cld)
  write_xlsx(ModFieldPupem_cld, path="Field_Puptake.xlsx")
# Visualizations
    (FieldPuptakePlot <- ggplot(ModFieldPupem_cld, aes(x=Treatment, y=emmean, fill=Treatment)) +
      geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                       pattern_density=0.05, pattern_spacing=0.01, width=0.65)+
      geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                    width=0.2, position=position_dodge(width=0.9)) +
      geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1) +
      labs(x="Treatments", y="Canola phosphorus uptake (mg/kg)") +
      scale_x_discrete(limits=c(FieldTrt_order),
                       labels=c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
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
  ggsave(FieldPuptakePlot, file="Field_Puptake.jpg", width=8, height=8, dpi=150)



##  P recovery   ----
  FieldPrec_Mean <- summary_by(Precovery~Treatment+Block, data=Field, FUN=function(x) mean(x, na.rm=TRUE)) 
  FieldPrec_Mean <- as.numeric(FieldPrec_Mean$Precovery)
  FieldPrec_skew <- skewness(FieldPrec_Mean,na.rm=TRUE)
  FieldPrec_kur <- kurtosis(FieldPrec_Mean,na.rm=TRUE)
  cat("Skewness:", FieldPrec_skew, "\n") # 01.001374 
  cat("Kurtosis:", FieldPrec_kur, "\n") # 4.388838 
  shapiro.test(Field$Precovery) # p=0.2583
  hist(Field$Precovery) # normal
  leveneTest(Precovery~Treatment, data=Field)  # 0.1486
  #ModFieldPrec1
  ModFieldPrec1 <- glmmTMB(Precovery~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFieldPrec1, type="III") # no significant differences
  summary(ModFieldPrec1)
  performance::r2(ModFieldPrec1) # 0.263
  # ModFieldPrec2
  ModFieldPrec2 <- lmer(Precovery~Treatment+(1|Block),data=Field)
  anova(ModFieldPrec2) # no significant differences
  summary(ModFieldPrec2)
  hist(resid(ModFieldPrec2)) # normalish
  shapiro.test(resid(ModFieldPrec2))  # 0.176
  plot(fitted(ModFieldPrec2),resid(ModFieldPrec2),pch=16)   # random
  qqnorm(resid(ModFieldPrec2)) # small  tails
  qqline(resid(ModFieldPrec2))
  rsq::rsq(ModFieldPrec2) # 0.205
  #Comparing models
  # Rsq = ModFieldPrec1 (glmm) is highest 
  # AIC & BIC
  FPrec_modlist <- list(ModFieldPrec1, ModFieldPrec2)
  AIC_values <- sapply(FPrec_modlist, AIC)
  BIC_values <- sapply(FPrec_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldPrec1", "ModFieldPrec2"),
                     AIC_values, BIC_values)
  print(N_AB)
      #Model AIC_values BIC_values
      #1 ModFieldPrec1  102.80613  107.05443 - only 10 df
      #2 ModFieldPrec2   87.27252   91.52082 - fractional df
#emmeans 
  (ModFieldPrecem <- emmeans(ModFieldPrec1,~Treatment, alpha=0.1,infer = TRUE))
  (ModFieldPrecem_cld <- cld(ModFieldPrecem, Letters=letters, type="response") )
  write_xlsx(ModFieldPrecem_cld, path="Field_Precovery.xlsx")
# Visualizations
  (FieldPrecPlot <- ggplot(ModFieldPrecem_cld, aes(x=Treatment, y=emmean, fill=Treatment)) +
    geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                     pattern_density=0.05, pattern_spacing=0.01, width = 0.65)+
    geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                  width=0.2, position=position_dodge(width=0.9)) +
    geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1) +
    labs(x="Treatments", y="Canola phosphorus recovery (%)") +
    scale_x_discrete(limits=c("Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                     labels=c("Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      scale_y_continuous(limits = c(-4,5))+
    theme(legend.position = "top", legend.key.size=unit(10,"mm"), 
          legend.title = element_blank(), legend.text=element_text(size=18),
          axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
          axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
          axis.title.x=element_blank(), 
          axis.title.y=element_text(size=26, face="bold", margin=margin(r=15)),
          panel.background = element_blank(),
          panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
  ggsave(FieldPrecPlot, file="Field_Precovery.jpg", width=8, height=8, dpi=150)


# SOIL ANALYSIS ----
  ### Soils at 3 different depths will be analysed for differences as well as auto-correlated
##   Soil NO3   ----
  # Calculating skewness and kurtosis
    NO3_stats <- function(data, variable_names) { #uses 'moments' instead of 'e1071' which results in much higher kurtosis
    stats <- data %>%
      summarise_at(vars(all_of(variable_names)), list(skewness = ~ skewness(., na.rm = TRUE), kurtosis = ~ kurtosis(., na.rm = TRUE)))
    return(stats)
  }
  variable_names <- c("NO3_10", "NO3_20", "NO3_30") # Specify the variable names to calculate stats for
  NO3_stats_all <- NO3_stats(Field, variable_names)# Call the function
  print(NO3_stats_all)
          #NO3_10_skewness NO3_20_skewness NO3_30_skewness NO3_10_kurtosis NO3_20_kurtosis NO3_30_kurtosis
          #1       0.6327243        1.261657      -0.2870261        4.929834        3.858688        2.415493
  # Normality and equality of variance
  shapiro.test(Field$NO3_10) # p=0.137
  hist(Field$NO3_10) #  norma 
  leveneTest(NO3_10~Treatment, data=Field)  # P=0.559
  shapiro.test(Field$NO3_20) # p=0.0056
  hist(Field$NO3_20) #  left skew
  leveneTest(NO3_20~Treatment, data=Field)  # P=0.0114
  shapiro.test(Field$NO3_30) # p=0.955
  hist(Field$NO3_30) #  slight right skew
  leveneTest(NO3_30~Treatment, data=Field)  # P=0.301
# Change data to long format
  NO3_long <- Field |>
    gather(key="Depth", value="NO3", matches("^NO3_"))
  #ModFieldSNO31
  ModFieldSNO31 <- lmer(NO3~Treatment*Depth + (1|Block), data=NO3_long)
  Anova(ModFieldSNO31, alpha=0.1) # significant differences
  summary(ModFieldSNO31, alpha=0.1)
  print(coef(summary(ModFieldSNO31))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldSNO31)) #normal
  shapiro.test(resid(ModFieldSNO31))  # 0.0169
  plot(fitted(ModFieldSNO31),resid(ModFieldSNO31),pch=16)   # fish shaped
  qqnorm(resid(ModFieldSNO31)) # moderate tails
  qqline(resid(ModFieldSNO31))
  rsq(ModFieldSNO31) # 0.625
# ModFieldSNO32
  ModFieldSNO32 <- glmer(NO3~Treatment*Depth+(1|Block),data=NO3_long,family=gaussian(link="log"), na.action=na.omit)
  Anova(ModFieldSNO32, type = "III") # significant differences
  summary(ModFieldSNO32)
  shapiro.test(resid(ModFieldSNO32)) # p=0.159
  # ModFieldSNO33
  ModFieldSNO33 <- glmmTMB(NO3~Treatment*Depth+(1|Block), data=NO3_long, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFieldSNO33, type="III") # significant differences
  summary(ModFieldSNO33)
  performance::r2(ModFieldSNO33) # 0.650
  # ModFieldSNO34
  ModFieldSNO34 <- lme(NO3~Treatment*Depth,random=~1|Block, data=NO3_long, na.action=na.exclude)
  anova(ModFieldSNO34)  # significant differences
  summary(ModFieldSNO34)
  rsq(ModFieldSNO34) # NA
#Comparing models
  # Rsq = ModFieldNup3 is highest of the measured rsq
  # AIC & BIC
  FNO3_modlist <- list(ModFieldSNO31, ModFieldSNO32, ModFieldSNO33, ModFieldSNO34)
  AIC_values <- sapply(FNO3_modlist, AIC)
  BIC_values <- sapply(FNO3_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldSNO31", "ModFieldSNO32", "ModFieldSNO33", "ModFieldSNO34"), AIC_values, BIC_values)
  print(N_AB)
      # Model AIC_values BIC_values
      #1 ModFieldSNO31   300.9198   344.4076 - fractional df
      #2 ModFieldSNO32   349.4724   392.9602 - results in Inf df
      #3 ModFieldSNO33   347.9463   391.4340
      #4 ModFieldSNO34   300.9198   337.9228 - only 3 df
#emmeans - ModFieldSNO33 chosen as best mod based on rsq and it's response in the emmeans function
  (ModFieldemSNO3 <- emmeans(ModFieldSNO33,~Treatment|Depth, alpha=0.1))
  (ModFieldemSNO3a <- emmeans(ModFieldSNO33,~Depth|Treatment, alpha=0.1))
  (ModFieldemSNO3_cld <- cld(ModFieldemSNO3, Letters=trimws(letters), reversed=TRUE, by = "Depth"))
  (ModFieldemSNO3_clda <- cld(ModFieldemSNO3a, Letters=trimws(letters), reversed=TRUE, by = "Depth"))
  NO3_combEm <- list(Treatment=ModFieldemSNO3_cld, Depth=ModFieldemSNO3_clda)
  write_xlsx(NO3_combEm, path="Field_SoilNO3.xlsx")



##  Soil PO4   ----
  # Calculating skewness and kurtosis
  PO4_VarNames <- c("PO4_10", "PO4_20", "PO4_30") # Specify the variable names to calculate stats for
  PO4_stats_all <- Field_stats(Field, PO4_VarNames)# Call the function
  print(PO4_stats_all)
            #PO4_10_skewness PO4_20_skewness PO4_30_skewness PO4_10_kurtosis PO4_20_kurtosis PO4_30_kurtosis
            #1       -0.390592      0.03913544      0.05779742        2.004251        1.633379        1.711494
  # Normality and equality of variance
  shapiro.test(Field$PO4_10) # p=0.207
  hist(Field$PO4_10) #  normal
  leveneTest(PO4_10~Treatment, data=Field)  # P=0.1086
  shapiro.test(Field$PO4_20) # p=0.109
  hist(Field$PO4_20) #  slight left skew
  leveneTest(PO4_20~Treatment, data=Field)  # P=0.00219
  shapiro.test(Field$PO4_30) # p=0.1994
  hist(Field$PO4_30) #  normal
  leveneTest(PO4_30~Treatment, data=Field)  # P=0.893
# Change data to long format
  PO4_long <- Field |>
    gather(key="Depth", value="PO4", matches("^PO4_"))
  print(PO4_long)
#ModFieldSPO41 - model is singular, not a good fit
  ModFieldSPO41 <-  lmer(PO4~Treatment*Depth + (1|Block), data=PO4_long)
  Anova(ModFieldSPO41, alpha=0.1) # significant differences
  summary(ModFieldSPO41, alpha=0.1)
  print(coef(summary(ModFieldSPO41))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldSPO41)) #normal
  shapiro.test(resid(ModFieldSPO41))  # 0.0111
  plot(fitted(ModFieldSPO41),resid(ModFieldSPO41),pch=16)   # normal around 0
  qqnorm(resid(ModFieldSPO41)) # moderate to heavy tails
  qqline(resid(ModFieldSPO41))
  rsq(ModFieldSPO41) # 0.938
  # ModFieldSPO42 - model is singular, not a god fit
  ModFieldSPO42 <- glmer(PO4~Treatment*Depth+(1|Block),data=PO4_long,family=gaussian(link="log"), na.action=na.omit)
  Anova(ModFieldSPO42, type = "III") # significant differences
  summary(ModFieldSPO42)
  shapiro.test(resid(ModFieldSPO42)) # p=0.0111
    #rsq(ModFieldSPO42)  #doesn't work
  # ModFieldSPO43
  ModFieldSPO43 <- glmmTMB(PO4~Treatment*Depth+(1|Block), data=PO4_long, family=gaussian(), na.action=na.exclude)
  glmmTMB:::Anova.glmmTMB(ModFieldSPO43, type="III") # significant differences
  summary(ModFieldSPO43)
  performance::r2(ModFieldSPO43) # NA singularity issues
  # ModFieldSPO44
  ModFieldSPO44 <- lme(PO4~Treatment*Depth,random=~1|Block, data=PO4_long, na.action=na.exclude)
  anova(ModFieldSPO44)  # significant differences
  summary(ModFieldSPO44)
  rsq(ModFieldSPO44) # NA
#Comparing models
  # Rsq = can't tell
  # AIC & BIC
  FPO4_modlist <- list(ModFieldSPO41, ModFieldSPO42, ModFieldSPO43, ModFieldSPO44)
  AIC_values <- sapply(FPO4_modlist, AIC)
  BIC_values <- sapply(FPO4_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldSPO41", "ModFieldSPO42", "ModFieldSPO43", "ModFieldSPO44"), AIC_values, BIC_values)
  print(N_AB)
        #Model AIC_values BIC_values
        #1 ModFieldSPO41   268.4968   312.5906
        #2 ModFieldSPO42   301.3189   345.4127
        #3 ModFieldSPO43   299.3189   343.4127
        #4 ModFieldSPO44   268.4968   306.3332
#emmeans 
  (ModFieldemSPO4 <- emmeans(ModFieldSPO44,~Treatment|Depth, alpha=0.1))
  (ModFieldemSPO4a <- emmeans(ModFieldSPO44,~Depth|Treatment, alpha=0.1))
  (ModFieldemSPO4_cld <- cld(ModFieldemSPO4,Letters=trimws(letters), reversed=TRUE, by = "Depth"))
  (ModFieldemSPO4_cld$Depth <- factor(ModFieldemSPO4_cld$Depth,
                                     levels = c("PO4_10", "PO4_20", "PO4_30"),
                                     labels = c("0-10cm", "10-20cm", "20-30cm")))
  (ModFieldemSPO4_clda <- cld(ModFieldemSPO4a, Letters=trimws(letters), reversed=TRUE, by = "Depth"))
  PO4_combEm <- list(Treatment=ModFieldemSPO4_cld, Depth=ModFieldemSPO4_clda)
  write_xlsx(PO4_combEm, path="Field_SoilPO4.xlsx")
# Visualizations
  (FieldPO4Plot <- ggplot(ModFieldemSPO4_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
      geom_point(size = 8, color="black", aes(fill=Treatment)) +
      scale_fill_manual(values = brewer.pal(6, name = "Dark2"), guide = "none") +
      scale_shape_manual(values = c(21,24,22)) +
      geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.5) +  # Add this line for geom_text
      scale_x_discrete(limits=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                       labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      labs(x = "", y = bquote(bold("PO"[4] ~ "(mg/kg)"))) +
      theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
            legend.title = element_blank(), legend.text=element_text(size=16),
            axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
            axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=28, face="bold", margin=margin(r=15)),
            panel.background = element_blank(),
            panel.border=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
  ggsave(FieldPO4Plot, file="Field_soilPO4.jpg", height=8, width = 11, dpi=150)
### PO4 autocorrelation ----
  # This method is not really applicable as I have only 3 depths, need more data for autocorrelation
  ## option 1 - more related to time series with lags, see explanation in word document in Thesis folder
    PO4TS <- ts(PO4TS[which(!is.na(PO4TS))], start = start(PO4TS)[which(!is.na(PO4TS))], frequency = frequency(PO4TS)) # convert data to time series
    acf(PO4TS)
    acf(PO4TS, plot = FALSE)
    pacf(PO4TS)
  ## option 2
    # convert Depth to numeric values for use in correlation
    PO4_long_sub <- subset(PO4_long, select=c(Block, Treatment, Depth, PO4))
    PO4_long_sub <- PO4_long_sub[complete.cases(PO4_long_sub[, c("Block", "Treatment", "Depth", "PO4")]), ]
    PO4_long_sub <- PO4_long_sub %>%
      mutate(Depth_numeric = as.numeric(gsub("PO4_(\\d+)", "\\1", Depth))) %>%
      filter(!is.na(Depth_numeric))
    PO4_long_sub$Depth <- factor(PO4_long_sub$Depth,
                                       levels = c("PO4_10", "PO4_20", "PO4_30"),
                                       labels = c("0-10cm", "10-20cm", "20-30cm"))
    print(PO4_long_sub)
    # run the regression model - format is to allow by function for us in the cor function
    ModFieldSPO4sub <- PO4_long_sub %>% ### cannot run ANOVA, summary or rsq on this model. Need to investigate
      group_by(Depth_numeric) %>%
      do(model = lme(PO4 ~ Treatment, random = ~1|Block, data = ., na.action = na.exclude))
    Anova(ModFieldSPO4sub)
    # run correlation on regression by Depth - this doesn't work as the depths are the same number for each depth causing SD=0
    soilPO4Cor <- PO4_long_sub %>%
      group_by(Depth_numeric) %>%
      summarise(correlation = cor(PO4, Depth_numeric))
    # autocorrelation for longitudinal data
    PO4acf <- PO4_long_sub %>%
      group_by(Depth_numeric) %>%
      summarise(acf = acf(PO4))
    print(PO4acf)

###  Cross-correlation  ----
    # This method correlates the relationship between each depth and the next
    # need to subtract and add very small values to create variation in dataset
    PO4_sub_var <- PO4_long_sub %>%
      mutate(DepthVar = case_when(
        Depth_numeric == 10 & row_number() %in% c(1, 2) ~ Depth_numeric + 0.01,
        Depth_numeric == 10 & row_number() %in% c(3, 4) ~ Depth_numeric - 0.01,
        Depth_numeric == 20 & row_number() %in% c(25, 26) ~ Depth_numeric + 0.01,
        Depth_numeric == 20 & row_number() %in% c(27, 28) ~ Depth_numeric - 0.01,
        Depth_numeric == 30 & row_number() %in% c(46, 47) ~ Depth_numeric + 0.01,
        Depth_numeric == 30 & row_number() %in% c(48, 49) ~ Depth_numeric - 0.01,
        TRUE ~ Depth_numeric
      ))
    print(PO4_sub_var$DepthVar)
    # undertake cross-correlation & develop plot
    Depth_names <- unique(PO4_sub_var$Depth)
    print(Depth_names)
    PO4croscor <- with(PO4_sub_var, ccf(PO4, DepthVar, lag.max = length(Depth_names))) # lag.max can be changed to any number
    plot(PO4croscor, xlab = "Lag", ylab = "Cross-correlation", main = "Cross-correlation of PO4 across Depths")
    axis(1, at = seq(0, length(Depth_names)-1), labels = Depth_names) # does not work as the axis labels are not linked to the depths
    # Find the lag with maximum correlation
    Po4maxCorLag <- PO4croscor$lag[which.max(PO4croscor$acf)]
    cat("Maximum correlation at lag:", Po4maxCorLag) # Print the lag with maximum correlation
    # Cross corr by treatment
    PO4croscorTrt <- by(PO4_sub_var, PO4_sub_var$Treatment, function(df) { # cross corr by treatment
      with(df, ccf(PO4, DepthVar, lag.max = length(Depth_names) - 1))
    })
    PO4croscorTrtDf <- do.call(rbind, lapply(names(PO4croscorTrt), function(treatment) {
      ccf_result <- PO4croscorTrt[[treatment]]
      data.frame(Treatment = treatment, lag = ccf_result$lag, correlation = ccf_result$acf)
    }))
    print(PO4croscorTrtDf)
    ggplot(PO4croscorTrtDf, aes(x = lag, y = correlation)) + #plot corr plot
      geom_line() +
      facet_wrap(~ Treatment, ncol = 2) +
      xlab("Lag") +
      ylab("Cross-correlation") +
      ggtitle("Cross-correlation of PO4 across Depths by Treatment")
    
    
##  Water soluble P   ----
    # Skewness and kurtosis
    WSP_VarNames <- c("WatSolP_10", "WatSolP_20", "WatSolP_30") # Specify the variable names to calculate stats for
    WSP_stats_all <- Field_stats(Field, WSP_VarNames)# Call the function
    print(WSP_stats_all)
        #WatSolP_10_skewness WatSolP_20_skewness WatSolP_30_skewness WatSolP_10_kurtosis WatSolP_20_kurtosis WatSolP_30_kurtosis
        #1          0.08689468            1.208605            1.668301             2.82506             3.98232            6.578185
    # Normality and equality of variance
    shapiro.test(Field$WatSolP_10) # p=0.597
    hist(Field$WatSolP_10) #  slight right skew
    leveneTest(WatSolP_10~Treatment, data=Field)  # P=0.2
    shapiro.test(Field$WatSolP_20) # p=0.011
    hist(Field$WatSolP_20) #  left skew
    leveneTest(WatSolP_20~as.factor(Treatment), data=Field)  # P=9.6e-07
    shapiro.test(Field$WatSolP_30) # p=03000635
    hist(Field$WatSolP_30) #  severe left skew
    leveneTest(WatSolP_30~as.factor(Treatment), data=Field)  # P=0.061
  # transform - log did not work on depth 20. Very difficult to do different transformations and scaling. All transformed to sqrt
    shapiro.test(sqrt(Field$WatSolP_10)) # p=0.073
    hist(sqrt(Field$WatSolP_10)) #  slight right  skew
    leveneTest(sqrt(WatSolP_10)~Treatment, data=Field)  # P=0.21
    shapiro.test(sqrt(Field$WatSolP_20)) # p=0.073
    hist(sqrt(Field$WatSolP_20)) #  slight right  skew
    leveneTest(sqrt(WatSolP_20)~Treatment, data=Field)  # P=0.21
    shapiro.test(log(Field$WatSolP_30)) # NAN
    hist(log(Field$WatSolP_30)) #  normalish, two spikes
    leveneTest(log(WatSolP_30)~Treatment, data=Field)  # NAN/Inf
    shapiro.test(sqrt(Field$WatSolP_30)) # 0.4
    hist(sqrt(Field$WatSolP_30)) #  normalish
    leveneTest(sqrt(WatSolP_30)~Treatment, data=Field)  # NAN/0.024
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
# Models
  # ModFieldWSP1 - model is singular
  ModFieldWSP1 <-   lmer(sqrt(WSP)~Treatment*Depth + (1|Block), data=WSP_long_sub)
  Anova(ModFieldWSP1, alpha=0.1) # significant differences
  summary(ModFieldWSP1, alpha=0.1)
  print(coef(summary(ModFieldWSP1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldWSP1)) # normal
  shapiro.test(resid(ModFieldWSP1))  # 0.116
  plot(fitted(ModFieldWSP1),resid(ModFieldWSP1),pch=16)   # two clusters, equal around 0
  qqnorm(resid(ModFieldWSP1)) # moderate to heavy tails
  qqline(resid(ModFieldWSP1))
  rsq(ModFieldWSP1) # 0.873
  # ModFieldWSP2 - only model that seems to work fine
  ModFieldWSP2 <- lme(sqrt(WSP)~Treatment*Depth,random=~1|Block, data=WSP_long_sub)
  Anova(ModFieldWSP2, type="III")  # significant differences
  summary(ModFieldWSP2)
  rsq(ModFieldWSP2) # 0.873
  # ModFieldWSP3 - singularity picked up in r2
  ModFieldWSP3 <- glmmTMB(sqrt(WSP)~Treatment*Depth+(1|Block), data=WSP_long_sub, family=gaussian())
  glmmTMB:::Anova.glmmTMB(ModFieldWSP3, type="III") # significant differences
  summary(ModFieldWSP3)
  performance::r2(ModFieldWSP3) # NA singularity issues
  #Comparing models
  # Rsq = can't tell
  # AIC & BIC
    FWSP_modlist <- list(ModFieldWSP1, ModFieldWSP2, ModFieldWSP3)
    AIC_values <- sapply(FWSP_modlist, AIC)
    BIC_values <- sapply(FWSP_modlist, BIC)
    N_AB <- data.frame(Model=c("ModFieldWSP1", "ModFieldWSP2", "ModFieldWSP3"), AIC_values, BIC_values)
    print(N_AB)
          #Model AIC_values BIC_values
          #1 ModFieldWSP1   97.16340   141.2573 - similar output to WSP2, singularity issues
          #2 ModFieldWSP2   97.16340   134.9998 - only 3 df
          #3 ModFieldWSP3   65.04675   109.1406 - singularity issues
#emmeans 
    (ModFieldemWSP <- emmeans(ModFieldWSP3,~Treatment|Depth, alpha=0.1))
    (ModFieldemWSP_cld <- cld(ModFieldemWSP,Letters=trimws(letters), reversed=TRUE, by = "Depth"))
    (ModFieldemWSP_cld <- ModFieldemWSP_cld %>% dplyr::rename(emmean="response"))
    write_xlsx(ModFieldemWSP_cld, path="Field_SoilWSP.xlsx")
# Visualizations
    (FieldWSPPlot <- ggplot(ModFieldemWSP_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
        geom_point(size = 8, color="black", aes(fill=Treatment)) +
      scale_fill_manual(values = brewer.pal(6, name = "Dark2"), guide = "none") +
      scale_shape_manual(values = c(21,24,22)) +
      geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.5) +  # Add this line for geom_text
      scale_x_discrete(limits=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                       labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      labs(x = "", y = bquote(bold("Water soluble P (mg/kg)"))) +
        theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
              legend.title = element_blank(), legend.text=element_text(size=16),
              axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
              axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size=28, face="bold", margin=margin(r=15)),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(FieldWSPPlot, file="Field_soilWSP.jpg", height=8, width = 11, dpi=150)



##  Soil resin P   ----
  # Skewness and kurtosis
    ResP_VarNames <- c("ResinP_10", "ResinP_20", "ResinP_30") 
    ResP_stats_all <- Field_stats(Field, ResP_VarNames)
    print(ResP_stats_all)
          #ResinP_10_skewness ResinP_20_skewness ResinP_30_skewness ResinP_10_kurtosis ResinP_20_kurtosis ResinP_30_kurtosis
          #1          0.1096331           1.062363          0.7336615           1.921336           3.379832           2.079667
  # Normality and equality of variance
    shapiro.test(Field$ResinP_10) # p=0.312
    hist(Field$ResinP_10) #  normalish, two spikes
    leveneTest(ResinP_10~Treatment, data=Field)  # P=0.039
    shapiro.test(Field$ResinP_20) # p=0.014
    hist(Field$ResinP_20) #  left skew
    leveneTest(ResinP_20~as.factor(Treatment), data=Field)  # P=0.0185
    shapiro.test(Field$ResinP_30) # p=0.00072
    hist(Field$ResinP_30) #  severe left skew
    leveneTest(ResinP_30~as.factor(Treatment), data=Field)  # P=0.0149
  # Transformations - log doesn't work on ResinP_30, sqrt improves normality but not variance: use sqrt
    shapiro.test(log(Field$ResinP_10))  #p=0.174
    leveneTest(log(ResinP_10)~Treatment, data=Field)  # p=0.229
    shapiro.test(log(Field$ResinP_20))  #p=0.831
    leveneTest(log(ResinP_20)~Treatment, data=Field)  # p=0.0052
    shapiro.test(log(Field$ResinP_30))  #p=NA
    leveneTest(log(ResinP_30)~Treatment, data=Field)  # p=NA
    shapiro.test(sqrt(Field$ResinP_10))  #p=0.323
    leveneTest(sqrt(ResinP_10)~Treatment, data=Field)  # p=0.103
    shapiro.test(sqrt(Field$ResinP_20))  #p=0.374
    leveneTest(sqrt(ResinP_20)~Treatment, data=Field)  # p=0.00603
    shapiro.test(sqrt(Field$ResinP_30))  #p=0.0097
    leveneTest(sqrt(ResinP_30)~Treatment, data=Field)  # p=0.00046
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
  ModFieldResP1 <- lmer(sqrt(ResP)~Treatment*Depth + (1|Block), data=ResP_long_sub)
  Anova(ModFieldResP1, alpha=0.1) # significant differences
  summary(ModFieldResP1, alpha=0.1)
  print(coef(summary(ModFieldResP1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldResP1)) # normal
  shapiro.test(resid(ModFieldResP1))  # 0.97
  plot(fitted(ModFieldResP1),resid(ModFieldResP1),pch=16)   # normal
  qqnorm(resid(ModFieldResP1)) # almost no tails
  qqline(resid(ModFieldResP1))
  rsq(ModFieldResP1) # 0.92
  # ModFieldResP2 
  ModFieldResP2 <- lme(sqrt(ResP)~Treatment*Depth,random=~1|Block, data=ResP_long_sub)
  Anova(ModFieldResP2, type="III")  # significant differences
  summary(ModFieldResP2)
  rsq(ModFieldResP2) # 0.92
  # ModFieldResP3
  ModFieldResP3 <- glmmTMB(sqrt(ResP)~Treatment*Depth+(1|Block), data=ResP_long_sub, family=gaussian())
  glmmTMB:::Anova.glmmTMB(ModFieldResP3, type="III") # significant differences
  summary(ModFieldResP3)
  performance::r2(ModFieldResP3) # 0.92
  #Comparing models
  # Rsq = all equal
  # AIC & BIC
  FResP_modlist <- list(ModFieldResP1, ModFieldResP2, ModFieldResP3)
  AIC_values <- sapply(FResP_modlist, AIC)
  BIC_values <- sapply(FResP_modlist, BIC)
  N_AB <- data.frame(Model=c("ModFieldResP1", "ModFieldResP2", "ModFieldResP3"), AIC_values, BIC_values)
  print(N_AB)
        #Model AIC_values BIC_values
        #1 ModFieldResP1  -33.96354  10.130309 - fractional df, better AIC/BIC than mod3 - use this
        #2 ModFieldResP2  -33.96354   3.872862 - best model for AIC/BIC, only 3df 
        #3 ModFieldResP3 -114.25215 -70.158296
# emmeans 
  (ModFieldemResP <- emmeans(ModFieldResP1,~Treatment|Depth, alpha=0.1))
  (ModFieldemResP_cld <- cld(ModFieldemResP,Letters=trimws(letters), reversed=TRUE, by = "Depth"))
  ModFieldemResP_cld <- ModFieldemResP_cld %>% dplyr::rename(emmean="response")
  write_xlsx(ModFieldemResP_cld, path="Field_SoilResinP.xlsx")
# Visualizations
  (FieldResPPlot <- ggplot(ModFieldemResP_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
      geom_point(size = 8, color="black", aes(fill=Treatment)) +
    scale_fill_manual(values = brewer.pal(6, name = "Dark2"), guide = "none") +
    scale_shape_manual(values = c(21,24,22)) +
    geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.5) +  # Add this line for geom_text
    scale_x_discrete(limits=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                     labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "TSP\nFertilizer"))+
    labs(x = "", y = bquote(bold("Resin P (ug/cm)"))) +
      theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
            legend.title = element_blank(), legend.text=element_text(size=16),
            axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
            axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=28, face="bold", margin=margin(r=15)),
            panel.background = element_blank(),
            panel.border=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
  ggsave(FieldResPPlot, file="Field_soilResP.jpg", height=8, width = 11, dpi=150)    



##  pH   ----
  # Skewness and kurtosis
    pH_VarNames <- c("pH_10", "pH_20", "pH_30") 
    pH_stats_all <- Field_stats(Field, pH_VarNames)
    print(pH_stats_all)
        #pH_10_skewness pH_20_skewness pH_30_skewness pH_10_kurtosis pH_20_kurtosis pH_30_kurtosis
        #  1     -0.1769981      0.1608135       0.069542       1.964359       2.972009        1.93708
  # Normality and equality of variance
    shapiro.test(Field$pH_10) # p=0.357
    hist(Field$pH_10) #  normalish, two spikes
    leveneTest(pH_10~Treatment, data=Field)  # P=0.0035
    shapiro.test(Field$pH_20) # p=0.667
    hist(Field$pH_20) #  normal
    leveneTest(pH_20~as.factor(Treatment), data=Field)  # P=0.00013
    shapiro.test(Field$pH_30) # p=0.139
    hist(Field$pH_30) #  normalish
    leveneTest(pH_30~as.factor(Treatment), data=Field)  # P=0.39
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
    Anova(ModFieldpH1, alpha=0.1) # significant differences
    summary(ModFieldpH1, alpha=0.1)
    print(coef(summary(ModFieldpH1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
    hist(resid(ModFieldpH1)) # normal
    shapiro.test(resid(ModFieldpH1))  # 0.29
    plot(fitted(ModFieldpH1),resid(ModFieldpH1),pch=16)   # normal
    qqnorm(resid(ModFieldpH1)) # small righttail
    qqline(resid(ModFieldpH1))
    rsq(ModFieldpH1) # 0.63
  # ModFieldpH2 
    ModFieldpH2 <- lme(pH~Treatment*Depth,random=~1|Block, data=pH_long_sub)
    Anova(ModFieldpH2, type="III")  # significant differences
    summary(ModFieldpH2)
    rsq(ModFieldpH2) # 0.63
  # ModFieldpH3
    ModFieldpH3 <- glmmTMB(pH~Treatment*Depth+(1|Block), data=pH_long_sub, family=gaussian())
    glmmTMB:::Anova.glmmTMB(ModFieldpH3, type="III") # significant differences
    summary(ModFieldpH3)
    performance::r2(ModFieldpH3) # 0.64
  #Comparing models
  # Rsq = Mo3 slightly better
  # AIC & BIC
    FpH_modlist <- list(ModFieldpH1, ModFieldpH2, ModFieldpH3)
    AIC_values <- sapply(FpH_modlist, AIC)
    BIC_values <- sapply(FpH_modlist, BIC)
    N_AB <- data.frame(Model=c("ModFieldpH1", "ModFieldpH2", "ModFieldpH3"), AIC_values, BIC_values)
    print(N_AB)
        #Model AIC_values BIC_values
        #1 ModFieldpH1  48.841897   93.52403
        #2 ModFieldpH2  48.841897   87.47841
        #3 ModFieldpH3  -1.491188   43.19094 - best model
  # emmeans 
    (ModFieldempH <- emmeans(ModFieldpH3,~Treatment|Depth, type="response",infer = TRUE, alpha=0.1))
    (ModFieldempH_cld <- cld(ModFieldempH, Letters=trimws(letters), reversed=TRUE, by = "Depth"))
    write_xlsx(ModFieldempH_cld, path="Field_pH.xlsx")
  # Visualizations
    (FieldpHPlot <- ggplot(ModFieldempH_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
        geom_point(size = 8, color="black", aes(fill=Treatment)) +
      scale_fill_manual(values = brewer.pal(6, name = "Dark2"), guide = "none") +
      scale_shape_manual(values = c(21,24,22)) +
      geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.5) +  # Add this line for geom_text
      scale_x_discrete(limits=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                       labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      labs(x = "", y = bquote(bold("pH"))) +
      theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
            legend.title = element_blank(), legend.text=element_text(size=16),
            axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
            axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
            axis.title.x=element_text(size = 28, face = "bold", colour = "black"),
            axis.title.y=element_text(size=28, face="bold", margin=margin(r=15)),
            panel.background = element_blank(),
            panel.border=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(FieldpHPlot, file="Field_soilpH.jpg", height=8, width = 11, dpi=150)    



##   Electrical conductivity   ----
    # Skewness and kurtosis
      EC_VarNames <- c("EC_10", "EC_20", "EC_30") 
      EC_stats_all <- Field_stats(Field, EC_VarNames)
      print(EC_stats_all)
          #EC_10_skewness EC_20_skewness EC_30_skewness EC_10_kurtosis EC_20_kurtosis EC_30_kurtosis
          #1       1.798002       4.027183       2.409736       6.651053       18.06702       8.314175
    # Normality and equality of variance
      shapiro.test(Field$EC_10) # p=0.00104
      hist(Field$EC_10) #  left skew, very flat
      leveneTest(EC_10~Treatment, data=Field)  # P=0.049
      shapiro.test(Field$EC_20) # p=1.39e-08
      hist(Field$EC_20) #  severe left skew
      leveneTest(EC_20~as.factor(Treatment), data=Field)  # P=0.126
      shapiro.test(Field$EC_30) # p=3.27e-06
      hist(Field$EC_30) #  severe left skew
      leveneTest(EC_30~as.factor(Treatment), data=Field)  # P=0.28
    # transform - log transform worked best
      shapiro.test(log(Field$EC_10))  # p=0.098
      leveneTest(log(EC_10)~Treatment, data=Field)  # p= 0.036
      shapiro.test(log(Field$EC_20))  # p=5.64e-05
      leveneTest(log(EC_20)~Treatment, data=Field)  # p= 0.00087
      shapiro.test(log(Field$EC_30))  # p=0.059
      leveneTest(log(EC_30)~Treatment, data=Field)  # p=0.186
      shapiro.test(sqrt(Field$EC_10))  # p=0.011
      leveneTest(sqrt(EC_10)~Treatment, data=Field)  # p= 0.038
      shapiro.test(sqrt(Field$EC_20))  # p=3.07e-07
      leveneTest(sqrt(EC_20)~Treatment, data=Field)  # p= 0.023
      shapiro.test(sqrt(Field$EC_30))  # p=0.00036
      leveneTest(sqrt(EC_30)~Treatment, data=Field)  # p=0.029
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
    Anova(ModFieldEC1, alpha=0.1) # significant differences
    summary(ModFieldEC1, alpha=0.1)
    print(coef(summary(ModFieldEC1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
    hist(resid(ModFieldEC1)) # normal
    shapiro.test(resid(ModFieldEC1))  # 0.0018
    plot(fitted(ModFieldEC1),resid(ModFieldEC1),pch=16)   # equal around 0
    qqnorm(resid(ModFieldEC1)) # moderate tails
    qqline(resid(ModFieldEC1))
    rsq(ModFieldEC1) # 0.575
    # ModFieldEC2 
    ModFieldEC2 <- lme(log(EC)~Treatment*Depth,random=~1|Block, data=EC_long_sub)
    Anova(ModFieldEC2, type="III")  # significant differences
    summary(ModFieldEC2)
    rsq(ModFieldEC2) # 0.575
    # ModFieldEC3
    ModFieldEC3 <- glmmTMB(log(EC)~Treatment*Depth+(1|Block), data=EC_long_sub, family=gaussian())
    glmmTMB:::Anova.glmmTMB(ModFieldEC3, type="III") # significant differences
    summary(ModFieldEC3)
    performance::r2(ModFieldEC3) # 0.585
    #Comparing models
    # Rsq = Mo3 slightly better
    # AIC & BIC
    FEC_modlist <- list(ModFieldEC1, ModFieldEC2, ModFieldEC3)
    AIC_values <- sapply(FEC_modlist, AIC)
    BIC_values <- sapply(FEC_modlist, BIC)
    N_AB <- data.frame(Model=c("ModFieldEC1", "ModFieldEC2", "ModFieldEC3"), AIC_values, BIC_values)
    print(N_AB)
          #Model AIC_values BIC_values
          #1 ModFieldEC1   165.4088   209.2019
          #2 ModFieldEC2   165.4088   202.8328
          #3 ModFieldEC3   159.4709   203.2640 - best model
    # emmeans 
    (ModFieldemEC <- emmeans(ModFieldEC3,~Treatment|Depth, type="response",infer = TRUE, alpha=0.1))
    (ModFieldemEC_cld <- cld(ModFieldemEC, Letters=trimws(letters), reversed=TRUE, by = "Depth"))
    ModFieldemEC_cld <- ModFieldemEC_cld %>% dplyr::rename(emmean="response")
    write_xlsx(ModFieldemEC_cld, path="Field_EC.xlsx")
    # Visualizations
    (FieldECPlot <- ggplot(ModFieldemEC_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
        geom_point(size = 8, color="black", aes(fill=Treatment)) +
      scale_fill_manual(values = brewer.pal(6, name = "Dark2"), guide = "none") +
      scale_shape_manual(values = c(21,24,22)) +
      geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.5) +  # Add this line for geom_text
      scale_x_discrete(limits=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                       labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      labs(x = "", y = bquote(bold("Electric conductivity (mS/cm)"))) +
        theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
              legend.title = element_blank(), legend.text=element_text(size=16),
              axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
              axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
              axis.title.x=element_text(size = 28, face = "bold", colour = "black"),
              axis.title.y=element_text(size=28, face="bold", margin=margin(r=15)),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(FieldECPlot, file="Field_soilEC.jpg", height=8, width = 11, dpi=150)    



##  Organic carbon   ----
  # Skewness and kurtosis
    OC_VarNames <- c("OC_10", "OC_20", "OC_30") 
    OC_stats_all <- Field_stats(Field, OC_VarNames)
    print(OC_stats_all)
            #OC_10_skewness OC_20_skewness OC_30_skewness OC_10_kurtosis OC_20_kurtosis OC_30_kurtosis
            #1      -2.050084    -0.03850754      -0.226943       9.304963        1.96851       1.829608
  # Normality and equality of variance
      shapiro.test(Field$OC_10) # p=0.00041
      hist(Field$OC_10) #  severe right skew
      leveneTest(OC_10~Treatment, data=Field)  # P=0.467
      shapiro.test(Field$OC_20) # p=0.449
      hist(Field$OC_20) #  normalish
      leveneTest(OC_20~as.factor(Treatment), data=Field)  # P=1.86e-06
      shapiro.test(Field$OC_30) # p=0.154
      hist(Field$OC_30) #  normalish
      leveneTest(OC_30~as.factor(Treatment), data=Field)  # P=0.874
  # transform - transformaions made it worse, didn't use
      shapiro.test(log(Field$OC_10))  # p=1.66e-06
      leveneTest(log(OC_10)~Treatment, data=Field)  # p= 0.45
      shapiro.test(log(Field$OC_20))  # p=0.32
      leveneTest(log(OC_20)~Treatment, data=Field)  # p= 9.3e-07
      shapiro.test(log(Field$OC_30))  # p=0.089
      leveneTest(log(OC_30)~Treatment, data=Field)  # p=0.811
      shapiro.test(sqrt(Field$OC_10))  # p=2.29e-05
      leveneTest(sqrt(OC_10)~Treatment, data=Field)  # p= 0.45
      shapiro.test(sqrt(Field$OC_20))  # p=0.41
      leveneTest(sqrt(OC_20)~Treatment, data=Field)  # p= 1.22e-06
      shapiro.test(sqrt(Field$OC_30))  # p=0.125
      leveneTest(sqrt(OC_30)~Treatment, data=Field)  # p=0.855
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
  # ModFieldOC1  - model is singular
    ModFieldOC1 <- lmer(OC~Treatment*Depth + (1|Block), data=OC_long_sub)
    Anova(ModFieldOC1, alpha=0.1) # significant differences
    summary(ModFieldOC1, alpha=0.1)
    print(coef(summary(ModFieldOC1))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
    hist(resid(ModFieldOC1)) # right skew
    shapiro.test(resid(ModFieldOC1))  # 2.69e-06
    plot(fitted(ModFieldOC1),resid(ModFieldOC1),pch=16)   # above 0
    qqnorm(resid(ModFieldOC1)) # moderate tails, higher on plot
    qqline(resid(ModFieldOC1))
    rsq(ModFieldOC1) # 0.867
  # ModFieldOC2 
    ModFieldOC2 <- lme(OC~Treatment*Depth,random=~1|Block, data=OC_long_sub)
    Anova(ModFieldOC2, type="III")  # significant differences
    summary(ModFieldOC2)
    rsq(ModFieldOC2) # 0.867
  # ModFieldOC3 - singularity issue in R2
    ModFieldOC3 <- glmmTMB(OC~Treatment*Depth+(1|Block), data=OC_long_sub, family=gaussian())
    glmmTMB:::Anova.glmmTMB(ModFieldOC3, type="III") # significant differences
    summary(ModFieldOC3)
    performance::r2(ModFieldOC3) # NA
  #Comparing models
    # Rsq = either of first two models
    # AIC & BIC
    FOC_modlist <- list(ModFieldOC1, ModFieldOC2, ModFieldOC3)
    AIC_values <- sapply(FOC_modlist, AIC)
    BIC_values <- sapply(FOC_modlist, BIC)
    N_AB <- data.frame(Model=c("ModFieldOC1", "ModFieldOC2", "ModFieldOC3"), AIC_values, BIC_values)
    print(N_AB)
      #Model AIC_values BIC_values
      #1 ModFieldOC1   43.50440   87.59825 - looks fine, using this one as mod3 has singularity issue
      #2 ModFieldOC2   43.50440   81.34080 - only 3 df
      #3 ModFieldOC3   -8.32373   35.77012 Best model AIC/BIC
  # emmeans 
    (ModFieldemOC <- emmeans(ModFieldOC1,~Treatment|Depth, type="response",infer = TRUE, alpha=0.1))
    (ModFieldemOC_cld <- cld(ModFieldemOC, Letters=trimws(letters), reversed=TRUE, by = "Depth"))
    write_xlsx(ModFieldemOC_cld, path="Field_OC.xlsx")
  # Visualizations
    (FieldOCPlot <- ggplot(ModFieldemOC_cld, aes(x = Treatment, y = emmean, shape = Depth)) +
      geom_point(size = 8, color="black", aes(fill=Treatment)) +
      scale_fill_manual(values = brewer.pal(6, name = "Dark2"), guide = "none") +
      scale_shape_manual(values = c(21,24,22)) +
      geom_text(aes(label = trimws(.group)), size = 7, nudge_x = 0.5) +  # Add this line for geom_text
      scale_x_discrete(limits=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                       labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      labs(x = "", y = bquote(bold("Organic carbon (%)"))) +
        theme(legend.position = "right", legend.key.size=unit(11,"mm"), legend.key = element_blank(),
              legend.title = element_blank(), legend.text=element_text(size=16),
              axis.text.x=element_text(angle=45, hjust=1, size=20, face="bold", colour="black"),
              axis.text.y = element_text(size = 20, face = "bold", colour = "black"),
              axis.title.x=element_blank(),
              axis.title.y=element_text(size=28, face="bold", margin=margin(r=15)),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
     ggsave(FieldOCPlot, file="Field_soilOC.jpg", height=8, width = 11, dpi=150)    
  
## combined residual plots ----
     (FresidPlot <- plot_grid(FieldPO4Plot, FieldWSPPlot, FieldResPPlot, FieldOCPlot, FieldpHPlot, FieldECPlot, nrow=3, ncol=2,
               labels = c("A", "B", "C", "D", "E", "F"), label_size = 35, label_x = c(0.11,0.11,0.11,0.11,0.11,0.11)))
     (FresidPlot_label <- ggdraw()+draw_plot(FresidPlot)+ draw_label("Treatment", y=0.02, size=30, fontface="bold"))
     ggsave("Combined residual soil.jpg", height=24, width=20, dpi=150)
     
     

# SNOWMELT ----
##  NO3 load    ----
    print(LNO3_stats <- Field_stats(Field, "LNO3"))
          #skewness kurtosis
          #1 0.6142101 2.395002
    shapiro.test(Field$LNO3) # p=0.118
    hist(Field$LNO3) #  slight left skew
    leveneTest(LNO3~factor(Treatment), data=Field)  # P=0.542
  #ModFieldLNO3a - model has singularity issues due to block having no effect
    ModFieldLNO3a <- lmer(LNO3~Treatment + (1|Block), data=Field, na.action=na.omit)
    ranef(ModFieldLNO3a)
    Anova(ModFieldLNO3a, type="III") # significant differences
    summary(ModFieldLNO3a)
    print(coef(summary(ModFieldLNO3a))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
    hist(resid(ModFieldLNO3a)) # normalish
    shapiro.test(resid(ModFieldLNO3a))  #0.366
    plot(fitted(ModFieldLNO3a),resid(ModFieldLNO3a),pch=16)   # equal around 0
    qqnorm(resid(ModFieldLNO3a)) # small tails
    qqline(resid(ModFieldLNO3a))
    rsq(ModFieldLNO3a) # 0.41
  # ModFieldLNO3b
    ModFieldLNO3b <- lme(LNO3~Treatment,random=~1|Block, data=Field, na.action=na.omit)
    ranef(ModFieldLNO3b)
    Anova(ModFieldLNO3b, type="III")  # significant differences
    summary(ModFieldLNO3b)
    hist(resid(ModFieldLNO3b)) # normalish
    shapiro.test(resid(ModFieldLNO3b))  #0.366
    plot(fitted(ModFieldLNO3b),resid(ModFieldLNO3b),pch=16)   # equal around 0
    qqnorm(resid(ModFieldLNO3b)) # small tails
    qqline(resid(ModFieldLNO3b))
    rsq(ModFieldLNO3b) # 0.378
  # ModFieldLNO3c - singularity issues in the R2 function
    ModFieldLNO3c <- glmmTMB(LNO3~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
    ranef(ModFieldLNO3c)
    glmmTMB:::Anova.glmmTMB(ModFieldLNO3c, type="III") # significant differences
    summary(ModFieldLNO3c)
    hist(resid(ModFieldLNO3c)) # normalish
    shapiro.test(resid(ModFieldLNO3c))  #0.366
    plot(fitted(ModFieldLNO3c),resid(ModFieldLNO3c),pch=16)   # equal around 0
    qqnorm(resid(ModFieldLNO3c)) # small tails
    qqline(resid(ModFieldLNO3c))
    performance::r2(ModFieldLNO3c) # NA
  # compare models  
    # Rsq = mod a, nots sure about mod c
    # AIC & BIC
    LNO3_modlist <- list(ModFieldLNO3a, ModFieldLNO3b, ModFieldLNO3c)
    AIC_values <- sapply(LNO3_modlist, AIC)
    BIC_values <- sapply(LNO3_modlist, BIC)
    N_AB <- data.frame(Model=c("ModFieldLNO3a", "ModFieldLNO3b", "ModFieldLNO3c"), AIC_values, BIC_values)
    print(N_AB)
          #Model AIC_values BIC_values
          #1 ModFieldLNO3a   96.09301   105.1770 - model has singularity issues, df=17, chosen as best model
          #2 ModFieldLNO3b   96.09301   102.7587 - best model but lowest rsq, only 3 df
          #3 ModFieldLNO3c  106.54443   115.6284 - model has singularity issues, df=15
    # emmeans 
    (ModFieldemLNO3 <- emmeans(ModFieldLNO3b,~"Treatment", infer = TRUE, alpha=0.1)) #%>%pairs(method="lsd", side="="))
    (ModFieldemLNO3_cld <- cld(ModFieldemLNO3, Letters=trimws(letters), reversed=TRUE, alpha=0.1))
    #kruskal.test(LNO3 ~ Treatment, data = Field)
    pwpm(ModFieldemLNO3)
    pwpp(ModFieldemLNO3)
    (LNO3plot <- ggplot(ModFieldemLNO3_cld,aes(x=Treatment,y=emmean))+geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE)))
    pairs(ModFieldemLNO3)
    comparison.plot(ModFieldemLNO3)
    write_xlsx(ModFieldemLNO3_cld, path ="Field_NO3Load.xlsx")



##  NH4 load  ----
  print(LNH4_stats <- Field_stats(Field, "LNH4"))
        #skewness kurtosis
        #1 2.057661 6.962457
  shapiro.test(Field$LNH4) # p= 2.222e-05
  hist(Field$LNH4) #  heavy left skew
  leveneTest(LNH4~factor(Treatment), data=Field)  # P=0.004341
# transform
  shapiro.test(na.omit(log(Field$LNH4))) # p=0.6202
  hist(log(Field$LNH4)) # normal
  leveneTest(log(LNH4)~factor(Treatment), data=Field)  # P=0.4345
  shapiro.test(sqrt(Field$LNH4)) # p=0.02159
  hist(sqrt(Field$LNH4)) #  slight left skew
  leveneTest(sqrt(LNH4)~factor(Treatment), data=Field)  # P=0.02392
#ModFieldLNH41
  ModFieldLNH4a <- lmer(log(LNH4)~Treatment + (1|Block), data=Field, na.action=na.omit)
  ranef(ModFieldLNH4a)
  Anova(ModFieldLNH4a, type="III") # significant differences
  summary(ModFieldLNH4a)
  print(coef(summary(ModFieldLNH4a))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldLNH4a)) # slight right skew
  shapiro.test(resid(ModFieldLNH4a))  #0.036
  plot(fitted(ModFieldLNH4a),resid(ModFieldLNH4a),pch=16)   # slightly more above 0
  qqnorm(resid(ModFieldLNH4a)) # moderate right tail
  qqline(resid(ModFieldLNH4a))
  rsq(ModFieldLNH4a) # 0.46
# ModFieldLNH4b
  ModFieldLNH4b <- lme(log(LNH4)~Treatment,random=~1|Block, data=Field, na.action=na.omit)
  ranef(ModFieldLNH4b)
  Anova(ModFieldLNH4b, type="III")  # significant differences
  summary(ModFieldLNH4b)
  hist(resid(ModFieldLNH4b)) # slight right skew
  shapiro.test(resid(ModFieldLNH4b))  #0.036
  plot(fitted(ModFieldLNH4b),resid(ModFieldLNH4b),pch=16)   # slightly more above 0
  qqnorm(resid(ModFieldLNH4b)) # moderate right tail
  qqline(resid(ModFieldLNH4b))
  rsq(ModFieldLNH4b) # 0.433
  # ModFieldLNH4c 
  ModFieldLNH4c <- glmmTMB(log(LNH4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  ranef(ModFieldLNH4c)
  glmmTMB:::Anova.glmmTMB(ModFieldLNH4c, type="III") # significant differences
  summary(ModFieldLNH4c)
  hist(resid(ModFieldLNH4c)) # slight right skew
  shapiro.test(resid(ModFieldLNH4c))  #0.036
  plot(fitted(ModFieldLNH4c),resid(ModFieldLNH4c),pch=16)   # slightly more above 0
  qqnorm(resid(ModFieldLNH4c)) # moderate right tail
  qqline(resid(ModFieldLNH4c))
  performance::r2(ModFieldLNH4c) # 0.538
# compare models  
  # Rsq = mod c
  # AIC& BIC
  FLNH4_modlist <- list(ModFieldLNH4a, ModFieldLNH4b, ModFieldLNH4c)
  AIC_values <- sapply(FLNH4_modlist, AIC)
  BIC_values <- sapply(FLNH4_modlist, BIC)
  FLNH4AB <- data.frame(Model=c("ModFieldLNH4a", "ModFieldLNH4b", "ModFieldLNH4c"), AIC_values, BIC_values)
  print(FLNH4AB)
        ## Model AIC_values BIC_values
        #1 ModFieldLNH4a   82.71708   91.80103
        #2 ModFieldLNH4b   82.71708   89.38278
        #3 ModFieldLNH4c   88.40730   97.49126
# emmeans 
  (ModFieldemLNH4 <- emmeans(ModFieldLNH4c, ~Treatment, alpha=0.1,infer = TRUE))
  (ModFieldemLNH4_cld <- cld(ModFieldemLNH4, Letters=trimws(letters), reversed=TRUE, type="response"))
  ModFieldemLNH4_cld <- ModFieldemLNH4_cld %>% rename(emmean="response")
  pwpm(ModFieldemLNH4) # pairwise p-value mean
  pwpp(ModFieldemLNH4) # pairwise p-value plot
  (LNH4plot <- ggplot(ModFieldemLNH4_cld,aes(x=Treatment,y=emmean))+geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE)))
  pairs(ModFieldemLNH4)
  emmip(ModFieldLNH4c, ~Treatment)
  write_xlsx(ModFieldemLNH4_cld, path="Field_LNH4.xlsx")

##  Resin NO3 load ----
  print(ResNO3_stats <- Field_stats(Field, "ResinNO3"))
          #skewness kurtosis
          #1 1.943068  7.00508
  shapiro.test(Field$ResinNO3) # p=0.0002299
  hist(Field$ResinNO3) #  left skew
  leveneTest(ResinNO3~Treatment, data=Field)  # P=0.1291
  # Transform
  shapiro.test(log(Field$ResinNO3))  #p=0.7096
  hist(log(Field$ResinNO3)) # normalish
  leveneTest(log(ResinNO3)~Treatment, data=Field)  # p=0.4265
  #ModFieldResNO31 - singularity issues
  ModFieldResNO3a <- lmer((ResinNO3)~Treatment + (1|Block), data=Field, na.action=na.omit)
  ranef(ModFieldResNO3a) # intercept=0
  Anova(ModFieldResNO3a, type="III") # no significant differences
  summary(ModFieldResNO3a)
  print(coef(summary(ModFieldResNO3a))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldResNO3a)) # normal
  shapiro.test(resid(ModFieldResNO3a))  #0.23
  plot(fitted(ModFieldResNO3a),resid(ModFieldResNO3a),pch=16)   # slightly more above 0
  qqnorm(resid(ModFieldResNO3a)) # small tails
  qqline(resid(ModFieldResNO3a))
  rsq(ModFieldResNO3a) # 0.377
  # ModFieldResNO3b
  ModFieldResNO3b <- lme(log(ResinNO3)~Treatment,random=~1|Block, data=Field, na.action=na.omit)
  ranef(ModFieldResNO3b) 
  Anova(ModFieldResNO3b, type="III")  # no significant differences
  summary(ModFieldResNO3b)
  hist(resid(ModFieldResNO3b)) # normal
  shapiro.test(resid(ModFieldResNO3b))  #0.23
  plot(fitted(ModFieldResNO3b),resid(ModFieldResNO3b),pch=16)   # slightly more above 0
  qqnorm(resid(ModFieldResNO3b)) # small  tails
  qqline(resid(ModFieldResNO3b))
  rsq(ModFieldResNO3b) # 0.375
  # ModFieldResNO3c - singularity issues in R2
  ModFieldResNO3c <- glmmTMB(log(ResinNO3)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  ranef(ModFieldResNO3c)
  glmmTMB:::Anova.glmmTMB(ModFieldResNO3c, type="III") # significant differences
  summary(ModFieldResNO3c)
  hist(resid(ModFieldResNO3c)) # normal
  shapiro.test(resid(ModFieldResNO3c))  #0.23
  plot(fitted(ModFieldResNO3c),resid(ModFieldResNO3c),pch=16)   # slightly more above 0
  qqnorm(resid(ModFieldResNO3c)) # small  tails
  qqline(resid(ModFieldResNO3c))
  performance::r2(ModFieldResNO3c) # NA
  # compare models  
  # Rsq = mod b
  # AIC& BIC
  FResNO3_modlist <- list(ModFieldResNO3a, ModFieldResNO3b, ModFieldResNO3c)
  AIC_values <- sapply(FResNO3_modlist, AIC)
  BIC_values <- sapply(FResNO3_modlist, BIC)
  FResNO3AB <- data.frame(Model=c("ModFieldResNO3a", "ModFieldResNO3b", "ModFieldResNO3c"), AIC_values, BIC_values)
  print(FResNO3AB)
        ##Model AIC_values BIC_values
        ##1 ModFieldResNO3a   63.10933   72.19328
        ##2 ModFieldResNO3b   63.10933   69.77504
        ##3 ModFieldResNO3c   61.91946   71.00342
  #emmeans 
    (ModFieldResNO3em <- emmeans(ModFieldResNO3a,~Treatment,infer = TRUE, alpha=0.1))
    (ModFieldResNO3em_cld <- cld(ModFieldResNO3em, Letters=trimws(letters), type="response") )
    ModFieldResNO3em_cld <- ModFieldResNO3em_cld %>% rename(emmean="response")
    pwpm(ModFieldResNO3em) # pairwise p-value mean
    pwpp(ModFieldResNO3em) # pairwise p-value plot
    (ResNO3plot <- ggplot(ModFieldResNO3em_cld,aes(x=Treatment,y=emmean))+geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE)))
    pairs(ModFieldResNO3em)
    emmip(ModFieldResNO3em, ~Treatment)
    write_xlsx(ModFieldResNO3em_cld, path="Field_ResinNO3.xlsx")
  
  # To combine multiple ggplots in one use cowplot
    plot_grid(LNO3plot, LNH4plot, ResNO3plot, labels = c('NO3', 'NH4', 'ResNO3'), label_size = 12)
    

##  PO4 load ----
    print(LPO4_stats <- Field_stats(Field, "LPO4"))
          #skewness kurtosis
          #1 3.058225 12.68079
    shapiro.test(Field$LPO4) # p=6.935e-07
    hist(Field$LPO4) # heavy left skew
    leveneTest(LPO4~factor(Treatment), data=Field)  # P=0.1006
  # transform - can't seem to log transfrom data, possibly linked to missing values or extrenmely low values
    shapiro.test(log(Field$LPO4)) # p=0.8156
    hist(log(Field$LPO4)) #  slight right skew
    leveneTest(log(LPO4)~Treatment, data=Field)  # P= 0.4527
    shapiro.test(sqrt(Field$LPO4)) # p=0.008712
    hist(sqrt(Field$LPO4)) #  severe left skew
    leveneTest(sqrt(LPO4)~Treatment, data=Field)  # P= 0.03249
  #ModFieldLPO4a
    ModFieldLPO4a <- lmer(log(LPO4)~Treatment + (1|Block), data=Field, na.action=na.omit)
    ranef(ModFieldLPO4a)
    Anova(ModFieldLPO4a, type="III") # no significant differences
    summary(ModFieldLPO4a)
    print(coef(summary(ModFieldLPO4a))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
    hist(resid(ModFieldLPO4a)) # normal
    shapiro.test(resid(ModFieldLPO4a))  #0.886
    plot(fitted(ModFieldLPO4a),resid(ModFieldLPO4a),pch=16)   # normal
    qqnorm(resid(ModFieldLPO4a)) # small tails
    qqline(resid(ModFieldLPO4a))
    rsq(ModFieldLPO4a) # 0.297
  # ModFieldLPO4b
    ModFieldLPO4b <- lme(log(LPO4)~Treatment,random=~1|Block, data=Field, na.action=na.omit)
    ranef(ModFieldLPO4b)
    Anova(ModFieldLPO4b, type="III")  # no significant differences
    summary(ModFieldLPO4b)
    hist(resid(ModFieldLPO4b))  # normal
    shapiro.test(resid(ModFieldLPO4b))  #0.886
    plot(fitted(ModFieldLPO4b),resid(ModFieldLPO4b),pch=16)    # normal
    qqnorm(resid(ModFieldLPO4b)) # small tails
    qqline(resid(ModFieldLPO4b))
    rsq(ModFieldLPO4b) # 0.269
  # ModFieldLPO4c 
    ModFieldLPO4c <- glmmTMB(log(LPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
    ranef(ModFieldLPO4c)
    glmmTMB:::Anova.glmmTMB(ModFieldLPO4c, type="III") # no significant differences
    summary(ModFieldLPO4c)
    hist(resid(ModFieldLPO4c))  # normal
    shapiro.test(resid(ModFieldLPO4c))  #0.888
    plot(fitted(ModFieldLPO4c),resid(ModFieldLPO4c),pch=16)    # normal
    qqnorm(resid(ModFieldLPO4c)) # small tails
    qqline(resid(ModFieldLPO4c))
    performance::r2(ModFieldLPO4c) # 0.359
  # one-way Kruskal-Wallis
    LPO4_subset <- subset(Field, !is.na(LPO4))
    LPO4_subset$LPO4 <- as.numeric(LPO4_subset$LPO4)
    LPO4_subset$Treatment <- as.factor(LPO4_subset$Treatment)
    ModFieldLPO4d=kruskal.test(LPO4 ~Treatment, data=LPO4_subset)
    ModFPO4_Dunn <- dunn.test(LPO4_subset$LPO4, LPO4_subset$Treatment, method="bonferroni")
    print(ModFPO4_Dunn)
  # AIC& BIC
    FLPO4_modlist <- list(ModFieldLPO4a, ModFieldLPO4b, ModFieldLPO4c)
    AIC_values <- sapply(FLPO4_modlist, AIC)
    BIC_values <- sapply(FLPO4_modlist, BIC)
    FLPO4AB <- data.frame(Model=c("ModFieldLPO4a", "ModFieldLPO4b", "ModFieldLPO4c"), AIC_values, BIC_values)
    print(FLPO4AB)
            ##Model AIC_values BIC_values
            ##1 ModFieldLPO4a    91.6988  100.78276
            ##2 ModFieldLPO4b    91.6988   98.36451
            ##3 ModFieldLPO4c   100.5754  109.65934
  # emmeans 
    (ModFieldLPO4em <- emmeans(ModFieldLPO4a,~Treatment,infer = TRUE, type="response"))
    (ModFieldLPO4em_cld <- cld(ModFieldLPO4em, Letters=trimws(letters), reversed=TRUE))
    ModFieldLPO4em_cld <- ModFieldLPO4em_cld %>% rename(emmean="response")
    pwpm(ModFieldLPO4em) # pairwise p-value mean
    pwpp(ModFieldLPO4em) # pairwise p-value plot
    (LPO4plot <- ggplot(ModFieldLPO4em_cld,aes(x=Treatment,y=emmean))+geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE)))
    pairs(ModFieldLPO4em)
    emmip(ModFieldLPO4em, ~Treatment)
    View(ModFieldLPO4em_cld)
    write_xlsx(ModFieldLPO4em_cld, path="Field_PO4Load.xlsx")
    # Vizualization
    (LPO4plot <- ggplot(ModFieldLPO4em_cld, aes(x=Treatment, y=emmean)) +
        geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                         pattern_density=0.05, pattern_spacing=0.01, width=0.65)+
        scale_y_continuous(limits=c(-0.001, 0.03)) +
        geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.25)+
        geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1)+
        labs(x="", y=bquote(bold("PO"[4] ~ " load in snowmelt runoff (kg/ha)"))) +
        scale_x_discrete(labels=c("Control1", "Control2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                  "TSP\nFertilizer"))+
        theme(legend.title=element_blank() , legend.key=element_blank(), legend.text=element_blank(),
              strip.text.x.top=element_text(size=20, face="bold"), strip.background = element_blank(),
              plot.title=element_blank(),
              axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=16, face="bold", colour="black"),
              axis.text.y=element_text(size=16, face="bold", colour="black"),
              axis.title=element_text(size=24, face="bold"),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    
    ## ggbetweenstats
    (LPO4statsplot <- ggbetweenstats(LPO4_subset, x=Treatment, y=LPO4, #need to used dataset with no missing values
                                     type="robust", # required
                                     ylab=expression("Leachate PO"[4]), xlab="Treatments", # expression used for subscript at end of text
                                     ggtheme = ggplot2::theme_gray(), # change theme & look of plot
                                     title=bquote(bold("Leachate PO"[4] ~ "-" ~ " in snowmelt runoff")))) # bquote used for subscript within text
    ggsave(LPO4statsplot, file="Field_leachatePO4.jpg", width=12, height=8, dpi=150)
    
    
##  Resin PO4 load ----
  print(ResPO4_stats <- Field_stats(Field, "ResinPO4"))
          #skewness kurtosis
          #1 1.797786 6.074146
  shapiro.test(Field$ResinPO4) # p=0.0003421
  hist(Field$ResinPO4) #  left skew
  leveneTest(ResinPO4~Treatment, data=Field)  # P=0.227
  #transform
  shapiro.test(log(Field$ResinPO4))  #p=0.8348
  hist(log(Field$ResinPO4)) # normal
  leveneTest(log(ResinPO4)~Treatment, data=Field)  # p=0.6948
  #ModFieldResP1
  ModFieldResPO4a <- lmer(log(ResinPO4)~Treatment + (1|Block), data=Field, na.action=na.omit)
  ranef(ModFieldResPO4a) 
  Anova(ModFieldResPO4a, type="III") # significant differences
  summary(ModFieldResPO4a)
  print(coef(summary(ModFieldResPO4a))[, "Pr(>|t|)"], pvalues = TRUE, significance_level = 0.1)
  hist(resid(ModFieldResPO4a)) # normal
  shapiro.test(resid(ModFieldResPO4a))  #0.204
  plot(fitted(ModFieldResPO4a),resid(ModFieldResPO4a),pch=16)  # normal
  qqnorm(resid(ModFieldResPO4a)) # very large tails
  qqline(resid(ModFieldResPO4a))
  rsq(ModFieldResPO4a) # 0.455
  # ModFieldResPO4b
  ModFieldResPO4b <- lme(log(ResinPO4)~Treatment,random=~1|Block, data=Field, na.action=na.omit)
  ranef(ModFieldResPO4b) 
  Anova(ModFieldResPO4b, type="III")  # significant differences
  summary(ModFieldResPO4b)
  hist(resid(ModFieldResPO4b)) # normal
  shapiro.test(resid(ModFieldResPO4b))  #0.204
  plot(fitted(ModFieldResPO4b),resid(ModFieldResPO4b),pch=16)  # normal
  qqnorm(resid(ModFieldResPO4b)) # very large tails
  qqline(resid(ModFieldResPO4b))
  rsq(ModFieldResPO4b) # 0.455
  # ModFieldResPO4c
  ModFieldResPO4c <- glmmTMB(log(ResinPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  ranef(ModFieldResPO4c)
  glmmTMB:::Anova.glmmTMB(ModFieldResPO4c, type="III") # significant differences
  summary(ModFieldResPO4c)
  hist(resid(ModFieldResPO4c)) # normal
  shapiro.test(resid(ModFieldResPO4c))  #0.204
  plot(fitted(ModFieldResPO4c),resid(ModFieldResPO4c),pch=16) # normal
  qqnorm(resid(ModFieldResPO4c)) # very large tails
  qqline(resid(ModFieldResPO4c))
  performance::r2(ModFieldResPO4c) # 0.499
  # compare models  
  # Rsq = mod b
  # AIC& BIC
  FResPO4_modlist <- list(ModFieldResPO4a, ModFieldResPO4b, ModFieldResPO4c)
  AIC_values <- sapply(FResPO4_modlist, AIC)
  BIC_values <- sapply(FResPO4_modlist, BIC)
  FResPO4AB <- data.frame(Model=c("ModFieldResPO4a", "ModFieldResPO4b", "ModFieldResPO4c"), AIC_values, BIC_values)
  print(FResPO4AB)
          ##Model AIC_values BIC_values
          ##1 ModFieldResPO4a    56.75125   66.17568
          ##2 ModFieldResPO4b    56.75125   63.87423
          ##3 ModFieldResPO4c    52.34028   61.76471 - highest r2, best AIC/BIC
  # emmeans 
    (ModFieldemResPO4 <- emmeans(ModFieldResPO4c,~Treatment,infer = TRUE, type="response"))
    (ModFieldemResPO4_cld <- cld(ModFieldemResPO4, Letters=trimws(letters), reversed=TRUE))
    ModFieldemResPO4_cld <- ModFieldemResPO4_cld %>% rename(emmean="response")
    pwpm(ModFieldemResPO4) # pairwise p-value mean
    pwpp(ModFieldemResPO4) # pairwise p-value plot
    (LPO4plot <- ggplot(ModFieldemResPO4_cld,aes(x=Treatment,y=emmean))+geom_point()+geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE)))
    pairs(ModFieldemResPO4)
    emmip(ModFieldemResPO4, ~Treatment)
    View(ModFieldemResPO4_cld)
    write_xlsx(ModFieldemResPO4_cld, path="Field_ResinPO4.xlsx")
    # Vizualization
    (ResinPO4PPlot <- ggplot(ModFieldemResPO4_cld, aes(x=Treatment, y=emmean)) +
        geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                         pattern_density=0.05, pattern_spacing=0.01, width=0.65)+
        scale_y_continuous(limit = c(-0.01, 0.8)) +
        geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.25)+
        geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1)+
        labs(x="", y="Resin P load in snowmelt runoff (kg/ha)") +
        scale_x_discrete(labels=c("Control1", "Control2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                  "TSP\nFertilizer"))+
        theme(legend.title=element_blank() , legend.key=element_blank(), legend.text=element_blank(),
              strip.text.x.top=element_text(size=20, face="bold"), strip.background = element_blank(),
              plot.title=element_blank(),
              axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=16, face="bold", colour="black"),
              axis.text.y=element_text(size=16, face="bold", colour="black"),
              axis.title=element_text(size=24, face="bold"),
              panel.background = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    
  
##  Plotting Snowmelt load ----
#create and combine data frames for the emmeans functions
  emPO4 <- as.data.frame(ModFieldLPO4em_cld)
  emResP <- as.data.frame(ModFieldemResPO4_cld)
  em_labels <- list("EM1" = "PO\u2084", "EM2" = "Resin PO\u2084")
  em_all <- bind_rows(list(EM1=emPO4, EM2=emResP), .id="EM") 
  em_all$EM <- factor(em_all$EM, levels=names(em_labels), labels=unlist(em_labels))
  print(em_all)
# define function to calculate position adjustment for secondary axis
  (Snowmeltplot <- ggplot(em_all, aes(x=Treatment, y=emmean)) +
      geom_bar_pattern(stat="identity", position=position_dodge2(padding=0.2), colour="black", fill="white", 
                       pattern_density=0.05, pattern_spacing=0.01, width=0.65)+
      facet_wrap(~ EM, scales="free_y") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.25)+
      geom_text(aes(label=trimws(.group), y=emmean+SE), size=8, vjust=-1)+
      labs(x="Treatment", y="Nutrient load in snowmelt runoff (kg/ha)") +
      scale_x_discrete(labels=c("Control1", "Control2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                "TSP\nFertilizer"))+
      theme(legend.title=element_blank() , legend.key=element_blank(), legend.text=element_blank(),
            strip.text.x.top=element_text(size=20, face="bold"), strip.background = element_blank(),
            plot.title=element_blank(),
            axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=16, face="bold", colour="black"),
            axis.text.y=element_text(size=16, face="bold", colour="black"),
            axis.title=element_text(size=24, face="bold"),
            panel.background = element_blank(),
            panel.border=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(Snowmeltplot, file="Field_Snowmelt.jpg", width=12, height=8, dpi=150)

    # Combined plot using cowplot
    (Snowmeltplot <- plot_grid(LPO4plot, ResinPO4PPlot, ncol=2, labels = c("A", "B"), label_size = 20, label_x = c(0.2,0.15)))
    (SnowPlot_label <- ggdraw()+draw_plot(Snowmeltplot)+ draw_label("Treatment", y=0.02, size=25, fontface="bold"))
    ggsave("Field_Snowmelt.jpg", height=8, width=12, dpi=150)




# COVARIANCE HEAT MAPS ----
##   Yield  ----
  FieldCovVar <- c("Yield", "NO3_10", "PO4_10", "WatSolP_10", "ResinP_10", "pH_10", "EC_10", "OC_10")
  FieldCovYield <- subset(Field, select=c("Treatment", FieldCovVar), 
                         na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
  FieldCovScaleYield <- as.data.frame(scale(FieldCovYield[,-1])) #remove treatment
  FieldCovScaleYield$Treatment <- FieldCovYield$Treatment
  FieldCovYieldSplit <- split(FieldCovScaleYield[, -ncol(FieldCovScaleYield)], FieldCovScaleYield$Treatment)
  YieldCov_Field <- lapply(FieldCovYieldSplit, function(x) cov(x, use="pairwise.complete.obs"))
  YieldCovFieldWb <- createWorkbook() 
  for (i in seq_along(YieldCov_Field)) { # for loop to bring all matrices into separate worksheets
    treatment_name <- names(YieldCov_Field)[i] # make sure that treatment names are used and not repeat first treatment
    sheet_name <- paste0(treatment_name)
    addWorksheet(YieldCovFieldWb, sheet_name)
    writeData(YieldCovFieldWb, sheet=sheet_name, x=YieldCov_Field[[i]], startRow=1, startCol=1, rowNames=TRUE)
  }
  saveWorkbook(YieldCovFieldWb, "Field_Yield_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
  YieldCovField_df <- lapply(seq_along(YieldCov_Field), function(i) {
    cov_mat1h <- as.matrix(YieldCov_Field[[i]])
    cov_mat1h <- setNames(cov_mat1h, FieldCovVar)
    cov_df1h <- as.data.frame(cov_mat1h)
    cov_df1h$Var1 <- rownames(cov_df1h)
    cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
    cov_df1h_long$treatment <- names(YieldCov_Field)[i]
    return(cov_df1h_long)
  })
# Combine all dataframes into one and set the variable names as factors and in the correct order
  YieldCovField_dfAll <- do.call(rbind, YieldCovField_df)
  YieldCovField_dfAll$Var1 <- factor(YieldCovField_dfAll$Var1, levels=FieldCovVar, 
                                     labels=c("Yield"="Yield", "NO3"="NO3", "PO4"="PO4", "WatSolP"="Soluble P",
                                              "ResinP"="Resin P","pH"="pH", "EC"="EC", "OC"="% SOC"))
  YieldCovField_dfAll$variable <- factor(YieldCovField_dfAll$variable, levels=FieldCovVar, 
                                         labels=c("Biomass"="Yield", "NO3"="NO3", "PO4"="PO4", 
                                                  "WatSolP"="Soluble P", "ResinP"="Resin P", "pH"="pH", 
                                                  "EC"="EC", "OC"="% SOC"))
  YieldCovField_dfAll$treatment <- factor(YieldCovField_dfAll$treatment,
                                          levels=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha",
                                                   "Biochar10thaTSP", "Phosphorus"),
                                          labels=c("Control 1", "Control 2", "Biochar 25kg P/ha", 
                                                   "Biochar 10t/ha", "Biochar 10t/ha & TSP", "Phosphorus Fertilizer"))
  write_xlsx(YieldCovField_dfAll, path="Field_YieldCov.xlsx")
# ggplot best option - brackets on both sides of the variable and plot code assigns and calls all in one
  (YieldCovFieldHeat <- ggplot(YieldCovField_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
      geom_tile() +
      scale_fill_gradientn(colors=brewer.pal(9, "YlGnBu"), limits=c(-2.4, 3.3), breaks=seq(-2.4, 3.3, by=1)) +
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
  ggsave(YieldCovFieldHeat, file="Field_YieldCovHeat.jpg", width=20, height=20, dpi=150)



##   Uptake  ----
  FieldUptakeCovVar <- c("Puptake",  "NO3_10", "PO4_10", "WatSolP_10", "ResinP_10", "pH_10", "EC_10", "OC_10")
  FieldCovUptake <- subset(Field, select=c("Treatment", FieldUptakeCovVar), 
                          na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
  FieldCovScaleUptake <- as.data.frame(scale(FieldCovUptake[,-1]))
  FieldCovScaleUptake$Treatment <- FieldCovUptake$Treatment
  FieldCovScaleUptakeSplit <- split(FieldCovScaleUptake[, -ncol(FieldCovScaleUptake)], FieldCovScaleUptake$Treatment)
## calculate the covariance matrix for each treatment excluding missing data
  UptakeCov_Field <- lapply(FieldCovScaleUptakeSplit, function(x) cov(x, use="pairwise.complete.obs"))
  UptakeCovFieldWb <- createWorkbook() # create workbook to save in xlsx
  for (i in seq_along(UptakeCov_Field)) { # for loop to bring all matrices into separate worksheets
    treatment_name <- names(UptakeCov_Field)[i] # make sure that treatment names are used and not repeat first treatment
    sheet_name <- paste0(treatment_name)
    addWorksheet(UptakeCovFieldWb, sheet_name)
    writeData(UptakeCovFieldWb, sheet=sheet_name, x=UptakeCov_Field[[i]], startRow=1, startCol=1, rowNames=TRUE)
  }
  saveWorkbook(UptakeCovFieldWb, "Field_Uptake_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
  UptakeCovField_df <- lapply(seq_along(UptakeCov_Field), function(i) {
    cov_mat1h <- as.matrix(UptakeCov_Field[[i]])
    cov_mat1h <- setNames(cov_mat1h, FieldUptakeCovVar)
    cov_df1h <- as.data.frame(cov_mat1h)
    cov_df1h$Var1 <- rownames(cov_df1h)
    cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
    cov_df1h_long$treatment <- names(UptakeCov_Field)[i]
    return(cov_df1h_long)
  })
# Combine all dataframes into one and set the variable names as factors and in the correct order
  UptakeCovField_dfAll <- do.call(rbind, UptakeCovField_df)
  UptakeCovField_dfAll$Var1 <- factor(UptakeCovField_dfAll$Var1, levels=FieldUptakeCovVar, 
                                     labels=c("Yield"="Yield", "NO3"="NO3", "PO4"="PO4", "WatSolP"="Soluble P",
                                              "ResinP"="Resin P","pH"="pH", "EC"="EC", "OC"="% SOC"))
  UptakeCovField_dfAll$variable <- factor(UptakeCovField_dfAll$variable, levels=FieldUptakeCovVar, 
                                         labels=c("Biomass"="Yield", "NO3"="NO3", "PO4"="PO4", 
                                                  "WatSolP"="Soluble P", "ResinP"="Resin P", "pH"="pH", 
                                                  "EC"="EC", "OC"="% SOC"))
  UptakeCovField_dfAll$treatment <- factor(UptakeCovField_dfAll$treatment,
                                          levels=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha",
                                                   "Biochar10thaTSP", "Phosphorus"),
                                          labels=c("Control 1", "Control 2", "Biochar 25kg P/ha", 
                                                   "Biochar 10t/ha", "Biochar 10t/ha & TSP", "Phosphorus Fertilizer"))
  write_xlsx(UptakeCovField_dfAll, path="Field_UptakeCov.xlsx")
# Generate the heatmap for each treatment and facet wrap them
  (UptakeCovFieldHeat <- ggplot(UptakeCovField_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
      geom_tile() +
      scale_fill_gradientn(colors=brewer.pal(9, "PuBuGn"), limits=c(-1.7, 3.3), breaks=seq(-1.7, 3.3, by=1)) +
      facet_wrap(~ treatment, nrow=3, scales="fixed") +
      geom_text(aes(label=sprintf("%.2f", Covariance), color = ifelse(Covariance > 2, "white", "black")), size=6.5) +
      scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
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
  ggsave(UptakeCovFieldHeat, file="Field_UptakeCovHeat.jpg", width=20, height=20, dpi=150)

##   P Recovery  ----
  FieldRecoveryCovVar <- c("Precovery",  "NO3_10", "PO4_10", "WatSolP_10", "ResinP_10", "pH_10", "EC_10", "OC_10")
  FieldCovRecovery <- subset(Field, select=c("Treatment", FieldRecoveryCovVar), 
                            na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
  FieldCovScaleRecovery <- as.data.frame(scale(FieldCovRecovery[,-1]))
  FieldCovScaleRecovery$Treatment <- FieldCovRecovery$Treatment
  FieldCovScaleRecoverySplit <- split(FieldCovScaleRecovery[, -ncol(FieldCovScaleRecovery)], FieldCovScaleRecovery$Treatment)
  RemoveControls <- c("Control1", "Control2")
  FieldCovScaleRecoverySplit <- FieldCovScaleRecoverySplit[!(names(FieldCovScaleRecoverySplit) %in% RemoveControls)]
## calculate the covariance matrix for each treatment excluding missing data
  RecoveryCov_Field <- lapply(FieldCovScaleRecoverySplit, function(x) cov(x, use="pairwise.complete.obs"))
  RecoveryCovFieldWb <- createWorkbook() # create workbook to save in xlsx
  for (i in seq_along(RecoveryCov_Field)) { # for loop to bring all matrices into separate worksheets
    treatment_name <- names(RecoveryCov_Field)[i] # make sure that treatment names are used and not repeat first treatment
    sheet_name <- paste0(treatment_name)
    addWorksheet(RecoveryCovFieldWb, sheet_name)
    writeData(RecoveryCovFieldWb, sheet=sheet_name, x=RecoveryCov_Field[[i]], startRow=1, startCol=1, rowNames=TRUE)
  }
  saveWorkbook(RecoveryCovFieldWb, "Field_Recovery_CovMatrix.xlsx")
# Convert each covariance matrix to a dataframe
  RecoveryCovField_df <- lapply(seq_along(RecoveryCov_Field), function(i) {
    cov_mat1h <- as.matrix(RecoveryCov_Field[[i]])
    cov_mat1h <- setNames(cov_mat1h, FieldRecoveryCovVar)
    cov_df1h <- as.data.frame(cov_mat1h)
    cov_df1h$Var1 <- rownames(cov_df1h)
    cov_df1h_long <- reshape2::melt(cov_df1h, id.vars="Var1", varnames=c("Var2"), value.name="Covariance")
    cov_df1h_long$treatment <- names(RecoveryCov_Field)[i]
    return(cov_df1h_long)
  })
# Combine all dataframes into one and set the variable names as factors and in the correct order
  RecoveryCovField_dfAll <- do.call(rbind, RecoveryCovField_df)
  RecoveryCovField_dfAll$Var1 <- factor(RecoveryCovField_dfAll$Var1, levels=FieldRecoveryCovVar, 
                                        labels=c("Yield"="Yield", "NO3"="NO3", "PO4"="PO4", "WatSolP"="Soluble P",
                                                 "ResinP"="Resin P","pH"="pH", "EC"="EC", "OC"="% SOC"))
  RecoveryCovField_dfAll$variable <- factor(RecoveryCovField_dfAll$variable, levels=FieldRecoveryCovVar, 
                                            labels=c("Yield"="Yield", "NO3"="NO3", "PO4"="PO4", 
                                                     "WatSolP"="Soluble P",
                                                     "ResinP"="Resin P","pH"="pH", "EC"="EC", "OC"="% SOC"))
  RecoveryCovField_dfAll$treatment <- factor(RecoveryCovField_dfAll$treatment,
                                             levels=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha",
                                                      "Biochar10thaTSP", "Phosphorus"),
                                             labels=c("Control 1", "Control 2", "Biochar 25kg P/ha", 
                                                      "Biochar 10t/ha", "Biochar 10t/ha & TSP", 
                                                      "Phosphorus Fertilizer"))
  write_xlsx(RecoveryCovField_dfAll, path="Field_RecoveryCov.xlsx")
# Generate the heatmap for each treatment and facet wrap them
  (RecoveryCovFieldHeat <- ggplot(RecoveryCovField_dfAll, aes(x=Var1, y=variable, fill=Covariance)) +
      geom_tile() +
      scale_fill_gradientn(colors=brewer.pal(9, "YlOrRd"), limits=c(-1.6, 3.3), breaks=seq(-1.6, 3.3, by=1)) +
      facet_wrap(~ treatment, nrow=3, scales="fixed") +
      geom_text(aes(label=sprintf("%.2f", Covariance), color = ifelse(Covariance > 2, "white", "black")), size=6.5) +
      scale_color_manual(values=c("black", "white"), guide="none", labels=NULL)+
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
  ggsave(RecoveryCovFieldHeat, file="Field_RecoveryCovHeat.jpg", width=20, height=15, dpi=150)


# YIELD TO N & P ----
  Field$Treatment <- as.factor(Field$Treatment)
  Field$Yield <- as.numeric(Field$Yield)
  Field$Nrecovery <- as.numeric(Field$Nrecovery)
  Field$Precovery <- as.numeric(Field$Precovery)
  FieldContourSub <- subset(Field, Treatment != "Control1" & Treatment != "Control2", 
                            select=c(Block, Treatment, Yield, Nrecovery, Precovery))
  View(FieldContourSub)
  FieldContourSub$Treatment <- factor(FieldContourSub$Treatment,
                                      levels=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha",
                                               "Biochar10thaTSP", "Phosphorus"),
                                      labels=c("Control 1", "Control 2", "Biochar 25kg P/ha", 
                                               "Biochar 10t/ha", "Biochar 10t/ha & TSP", 
                                               "Phosphorus Fertilizer"))
  View(FieldContourSub)
  FieldContourExcl <- na.exclude(FieldContourSub)
  View(FieldContourExcl)
  FieldContourMod <- glmmTMB(Yield ~ Nrecovery + Precovery + Treatment + (1|Block), data=FieldContourExcl, 
                            na.action=na.exclude)
  summary(FieldContourMod)
  Anova(FieldContourMod)
#Set up N & P recovery grids per soil
  FieldNrecovery_grid <- seq(min(FieldContourExcl$Nrecovery, na.rm=TRUE), max(FieldContourExcl$Nrecovery, na.rm=TRUE),
                            length.out=100)
  FieldPrecovery_grid <- seq(min(FieldContourExcl$Precovery, na.rm=TRUE), max(FieldContourExcl$Precovery, na.rm=TRUE),
                            length.out=100)
# Set up expanded grids then assign yield - must include block as it was used in the model!!
  FieldContour_grid <- expand.grid(Block=unique(FieldContourExcl$Block), Treatment=unique(FieldContourExcl$Treatment), 
                                  Nrecovery=FieldNrecovery_grid, Precovery=FieldPrecovery_grid)
  FieldContour_grid$Yield <- predict(FieldContourMod, newdata=FieldContour_grid)
  FieldContour_grid <- FieldContour_grid[,-1] # remove block so it doesn't appear in the plot
  View(FieldContour_grid)
# develop contour plot
  (FieldContours <- ggplot(FieldContour_grid, aes(x=Nrecovery, y=Precovery, z=Yield)) +
      geom_raster(aes(fill=Yield)) + #use rastar to get smooth lines
      geom_contour(aes(z=Yield), color='gray30', binwidth=100) + #contour line, adjust binwidth depending on yield
      facet_wrap(~Treatment, nrow=2) +
      scale_fill_gradientn(colors=brewer.pal(9, "BuPu")) +
      labs(x="% N Recovery", y="% P Recovery", fill="Yield\n(kg/ha)") +
      theme(legend.title=element_text(size=25, face="bold"),
            legend.key.size=unit(15, "mm"),
            legend.text=element_text(size=20),
            strip.text=element_text(size=25, face="bold"),
            strip.placement="outside",
            strip.background=element_blank(),
            strip.text.x=element_text(vjust=1),
            axis.text.x=element_text(size=20),
            axis.text.y=element_text(size=20),
            axis.title.x=element_text(size=30, face="bold"),
            axis.title.y=element_text(size=30, face="bold"),
            panel.spacing=unit(0.5, "cm")))
  ggsave(FieldContours, file="Field_YieldContour.jpg", width=15, height=15, dpi=150)

  
  

# PCA & EIGENVALUES  ----
  FieldEigenMatrix <- Field[complete.cases(Field), c("Nuptake", "Nrecovery", "Puptake", "Precovery", "NO3_10", "PO4_10", 
                                                     "WatSolP_10", "ResinP_10","pH_10", "EC_10", "OC_10"),]
  FieldEigenCor <- cor(FieldEigenMatrix)
  FieldEigenPCA <- FactoMineR::PCA(FieldEigenMatrix, scale.unit = TRUE, ncp = length(FieldEigenMatrix)-1)
  FieldEigenPrin <- princomp(FieldEigenCor)
  summary(FieldEigenPrin, digits=3)
  round(FieldEigenPrin$loadings[, 1:2], 3)
  
  
# NUTRIENT USE EFFICIENCY ----
  # Nitrogen
  Field$NUE <- as.numeric(as.character(Field$NUE))
  FNUE_df <- subset(Field, select = c("Block", "Treatment", "NUE"), na.action=function(x) x[, complete.cases(x)], na.rm=FALSE)
  FNUE_df <- FNUE_df[!FNUE_df$Treatment %in% "Control1", ]
  print(FNUE_df)
  shapiro.test(FNUE_df$NUE)  #p=0.000313
  shapiro.test(log(FNUE_df$NUE))  #p=0.705 - it improves normality but cannot be applied as some values are negative
  hist(FNUE_df$NUE) # severe left skew
  hist(log(FNUE_df$NUE)) # normalish
  leveneTest(NUE ~ Treatment, data=FNUE_df)  # p=0.239
  FNUEmod1 <- glmmTMB(NUE~Treatment+(1|Block), data=FNUE_df, family=gaussian(), na.action = na.exclude)
  glmmTMB:::Anova.glmmTMB(FNUEmod1, type="III") # NO significant differences
  summary(FNUEmod1)
  performance::r2(FNUEmod1) # 0.293
  shapiro.test(resid(FNUEmod1)) # p= 0.015
  plot(fitted(FNUEmod1),resid(FNUEmod1),pch=16) # clustered below zero
  qqnorm(resid(FNUEmod1)) # heavy  tails
  qqline(resid(FNUEmod1))
  FNUEmodEm<- emmeans(FNUEmod1,~Treatment, subset=(Field$NUE), type="response",infer = TRUE)
  FNUEmod_cld <- cld(FNUEmodEm, Letters=trimws(letters), reversed=TRUE) 
  print(FNUEmod_cld)
  write_xlsx(FNUEmod_cld, path="Field_NUE.xlsx")
  # Phosphorus
  Field$PUE <- as.numeric(Field$PUE)
  shapiro.test(Field$PUE)  #p=0.026
  hist(Field$PUE) # slight left skew
  leveneTest(PUE ~ Treatment, data=Field)  # p=0.716
  FPUEmod1 <- glmmTMB(PUE~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  glmmTMB:::Anova.glmmTMB(FPUEmod1, type="III") # significant differences
  summary(FPUEmod1)
  performance::r2(FPUEmod1) # 0.591
  shapiro.test(resid(FPUEmod1)) # p= 0.032
  plot(fitted(FPUEmod1),resid(FPUEmod1),pch=16) # normal
  qqnorm(resid(FPUEmod1)) # slight  tails
  qqline(resid(FPUEmod1))
  FPUEmodEm<- emmeans(FPUEmod1,~Treatment, subset=(Field$PUE), type="response",infer = TRUE, alpha=0.1)
  FPUEmod_cld <- cld(FPUEmodEm, Letters=trimws(letters), reversed=TRUE) # no differences as SE is huge
  print(FPUEmod_cld)
  write_xlsx(FPUEmod_cld, path="Field_PUE.xlsx")
  
  
# EXTRACT ANOVA TABLES ----
  ModFStraw4 <- glmmTMB(FStraw~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
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
  
  ModFieldNup2 <- glmer(log(Nuptake)~Treatment+(1|Block),data=Field,family=gaussian(link="log"), na.action=na.omit)
  (FNupAN <- Anova(ModFieldNup2))
  FNupAN$RowNames <- row.names(FNupAN)
  rownames(FNupAN) <- NULL
  
  ModFieldNrec2 <- lme(Nuptake~Treatment,random=~1|Block, data=FNrec_out)
  FNrecAN <- anova(ModFieldNrec2) 
  FNrecAN$RowNames <- row.names(FNrecAN)
  rownames(FNrecAN) <- NULL
  
  ModFieldPup1 <- glmmTMB(Puptake~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.exclude)
  FPupAN <- glmmTMB:::Anova.glmmTMB(ModFieldPup1, type="III") 
  FPupAN$RowNames <- row.names(FPupAN)
  rownames(FPupAN) <- NULL
  
  ModFieldPrec2 <- lmer(Precovery~Treatment+(1|Block),data=Field)
  FPrecAN <- Anova(ModFieldPrec2, type="III") 
  FPrecAN$RowNames <- row.names(FPrecAN)
  rownames(FPrecAN) <- NULL
  
  ModFieldSNO33 <- glmmTMB(NO3~Treatment*Depth+(1|Block), data=NO3_long, family=gaussian(), na.action=na.exclude)
  FSNO3AN <- glmmTMB:::Anova.glmmTMB(ModFieldSNO33, type="III") 
  FSNO3AN$RowNames <- row.names(FSNO3AN)
  rownames(FSNO3AN) <- NULL
  
  ModFieldSPO44 <- lme(PO4~Treatment*Depth,random=~1|Block, data=PO4_long, na.action=na.exclude)
  FSPO4AN <- Anova(ModFieldSPO44, type="III")
  FSPO4AN$RowNames <- row.names(FSPO4AN)
  rownames(FSPO4AN) <- NULL
  
  ModFieldWSP3 <- glmmTMB(sqrt(WSP)~Treatment*Depth+(1|Block), data=WSP_long_sub, family=gaussian())
  FWSPAN <- glmmTMB:::Anova.glmmTMB(ModFieldWSP3, type="III")
  FWSPAN$RowNames <- row.names(FWSPAN)
  rownames(FWSPAN) <- NULL
  
  ModFieldResP1 <- lmer(sqrt(ResP)~Treatment*Depth + (1|Block), data=ResP_long_sub)
  FResPAN <- Anova(ModFieldResP1, alpha=0.1) 
  FResPAN$RowNames <- row.names(FResPAN)
  rownames(FResPAN) <- NULL
  
  ModFieldpH3 <- glmmTMB(pH~Treatment*Depth+(1|Block), data=pH_long_sub, family=gaussian())
  FpHAN <- glmmTMB:::Anova.glmmTMB(ModFieldpH3, type="III") 
  FpHAN$RowNames <- row.names(FpHAN)
  rownames(FpHAN) <- NULL
  
  ModFieldEC3 <- glmmTMB(log(EC)~Treatment*Depth+(1|Block), data=EC_long_sub, family=gaussian())
  FecAN <- glmmTMB:::Anova.glmmTMB(ModFieldEC3, type="III") 
  FecAN$RowNames <- row.names(FecAN)
  rownames(FecAN) <- NULL
  
  ModFieldOC1 <- lmer(OC~Treatment*Depth + (1|Block), data=OC_long_sub)
  FocAN <- Anova(ModFieldOC1, alpha=0.1) 
  FocAN$RowNames <- row.names(FocAN)
  rownames(FocAN) <- NULL
  
  ModFieldLNO3b <- lme(LNO3~Treatment,random=~1|Block, data=Field, na.action=na.omit)
  FLNO3AN <- Anova(ModFieldLNO3b, type="III")  
  FLNO3AN$RowNames <- row.names(FLNO3AN)
  rownames(FLNO3AN) <- NULL
  
  ModFieldLNH4c <- glmmTMB(log(LNH4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  FLNH4AN <- glmmTMB:::Anova.glmmTMB(ModFieldLNH4c, type="III") 
  FLNH4AN$RowNames <- row.names(FLNH4AN)
  rownames(FLNH4AN) <- NULL
  
  ModFieldResNO3a <- lmer((ResinNO3)~Treatment + (1|Block), data=Field, na.action=na.omit)
  FResNO3AN <- Anova(ModFieldResNO3a, type="III") 
  FResNO3AN$RowNames <- row.names(FResNO3AN)
  rownames(FResNO3AN) <- NULL
  
  ModFieldLPO4a <- lmer(log(LPO4)~Treatment + (1|Block), data=Field, na.action=na.omit)
  FLPO4AN <- Anova(ModFieldLPO4a, type="III") 
  FLPO4AN$RowNames <- row.names(FLPO4AN)
  rownames(FLPO4AN) <- NULL
  
  ModFieldResPO4c <- glmmTMB(log(ResinPO4)~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  FResPO4AN <- glmmTMB:::Anova.glmmTMB(ModFieldResPO4c, type="III") 
  FResPO4AN$RowNames <- row.names(FResPO4AN)
  rownames(FResPO4AN) <- NULL
  
  
  FNUEmod1 <- glmmTMB(NUE~Treatment+(1|Block), data=FNUE_df, family=gaussian(), na.action = na.exclude)
  FNUEAN <- glmmTMB:::Anova.glmmTMB(FNUEmod1, type="III")
  FNUEAN$RowNames <- row.names(FNUEAN)
  rownames(FNUEAN) <- NULL
  
  
  FPUEmod1 <- glmmTMB(PUE~Treatment+(1|Block), data=Field, family=gaussian(), na.action=na.omit)
  FPUEAN <- glmmTMB:::Anova.glmmTMB(FPUEmod1, type="III")
  FPUEAN$RowNames <- row.names(FPUEAN)
  rownames(FPUEAN) <- NULL
  
  
  FieldANOVAtables <- list(FStrawAN, FGrainAN, FYieldAN, FNupAN, FNrecAN, FPupAN, FPrecAN, FSNO3AN, FSPO4AN, FWSPAN, FResPAN, FpHAN, 
                           FecAN, FocAN, FLNO3AN, FLNH4AN, FResNO3AN, FLPO4AN, FResPO4AN, FNUEAN, FPUEAN)
  names(FieldANOVAtables) <- c("Straw", "Grain", "Yield", "Nuptake", "Nrecovery", "Puptake", "Precovery", "SoilNO3", "SoilPO4", "ResinP", 
                               "WaterSolP", "pH", "EC", "OC", "SnowNO3", "SnowNH4", "SnowPO4", "ResinPO4", "ResinNO3", "NUE", "PUE")
  write_xlsx(FieldANOVAtables, path="FieldANOVAtables.xlsx")

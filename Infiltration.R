# Loading data in to R& Summaries ----
  Infil<-read.csv("Infiltration.csv", fileEncoding="UTF-8-BOM") # full dataset with 8-10 observations per plot
  Infiltrationraw<-read.csv("Infiltrationraw.csv", fileEncoding="UTF-8-BOM")
  Infilsub <- read.csv("Infil.csv", fileEncoding="UTF-8-BOM") # #combined data with 24 observations total
  Infilsubraw <- read.csv("Infilraw.csv", fileEncoding="UTF-8-BOM")
  Field<-read.csv("Field.csv", fileEncoding="UTF-8-BOM")

  ## Loading libraries ----
  library(summarytools)
  library(lme4)
  library(nlme)
  library(lmerTest)
  library(doBy) # working with grouped data, specifically the do and by functions
  library(ggplot2)
  library(ggExtra) # add marginal layers to plot
  library(plotrix) # various plot options, I used it for ablines
  library(car) # general package with lots of uses, including Anova
  library(afex) # used to bring raw data into ggplot
  library(onewaytests) # for one way ANOVAs
  library(dplyr) # general package with lots of uses
  library(tidyr) # tidies up data
  library(broom) # cleans up messy output into tibbles
  library(magrittr) # provided pipe operator %>%
  library(readr) # easy way to read csv and other 'rectangular' files
  library(multcomp) # siultaneous test and confidence intervals
  library(multcompView) # for correlations
  library(emmeans)
  library(e1071) #skewness and kurtosis
  library(rsq)
  library(soilphysics) # bulkd density, compaction, soil water availability & retention curves (didn't use)
  library(soiltexture) # get soil texture triangles
  library(SoilHyP) # soil hydraulic conductivity and water retention (didn't use)
  library(HydroMe) # SSphilips function for extracting S & K
  library(stats)
  library(multcomp)
  library(DataVisualizations) # various type of analysis through visualisation
  library(aqp) #pedometrics
  library(writexl)# write xls or slsx files (specify path="xx.csv")

  ## Summary and ordering of data   ----
  # Summary data and missing values
    colSums(is.na(Infil[,]))
    colSums(is.na(Infiltrationraw[,]))
    descr(Infil) # gives full stats description of overall dataset
    descr(Infiltrationraw) # use stby to filter by grouping
    stby(data=Infiltrationraw, INDICES=Infiltrationraw$Treatment, FUN=descr, na.rm = TRUE, stats=c("min", "mean", "max"))  # stats="common", transpose=TRUE
    str(Infil) #displays the structure of the object   
  # Change columns in a dataframe to factors/categorical values, str displays 
    FieldTrt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP","Phosphorus")
    Infil$Block <- factor(Infil$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
    Infil$Treatment <- factor(Infil$Treatment,levels = FieldTrt_order)
    Infil$Time <- as.numeric(as.character(Infil$Time))
    Infil$CI <- as.numeric(as.character(Infil$CI))
    
  ## Check for outliers   ----
    Infiltrationraw$Block <- factor(Infiltrationraw$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
    Infiltrationraw$Treatment <- factor(Infiltrationraw$Treatment,levels = FieldTrt_order)
    Infiltrationraw$Time <- as.numeric(as.character(Infiltrationraw$TimeS))
    Infiltrationraw$CI <- as.numeric(as.character(Infiltrationraw$CI))
    Infilsubraw$Slope <- as.numeric(Infilsubraw$S)
    Infilsubraw$Block <- factor(Infilsubraw$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
    Infilsubraw$Treatment <- factor(Infilsubraw$Treatment,levels = FieldTrt_order)
# check data from Infilsub
    # Slopes calculated using philips model
    ggplot(Infilsubraw, aes(x=Treatment, y=Slope, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Slope (s/2)")
    ggsave("OutliersField_Slope.jpg", width=8, height=8, dpi=150)
    # Hydraulic conductivity calculated using philips model
    ggplot(Infilsubraw, aes(x=Treatment, y=K, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Hydraulic conductivity (K)")
    ggsave("OutliersField_K.jpg", width=8, height=8, dpi=150)
    # Initial infiltration rates (cm/h)
    ggplot(Infilsubraw, aes(x = Treatment, y = InitialRate, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Initial Infiltration")
    ggsave("OutliersField_Initial Infiltration.jpg", width = 8, height = 8, dpi = 150)
    # Final infiltration rates (cm/h)
    ggplot(Infilsubraw, aes(x = Treatment, y = FinalRate, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Final Infiltration")
    ggsave("OutliersField_Final infiltration.jpg", width = 8, height = 8, dpi = 150)
    # Moisture percentage
    ggplot(Infilsubraw, aes(x = Treatment, y = MoisturePerc, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Moisture percentage")
    ggsave("OutliersField_Moisture percentage.jpg", width = 8, height = 8, dpi = 150)
    # Sorptivity
    ggplot(Infilsubraw, aes(x = Treatment, y = Sorptivity, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Sorptivity (S)")
    ggsave("OutliersField_Sorptivity.jpg", width = 8, height = 8, dpi = 150)
# Check curves - raw data
    Infiltrationraw$Treatment <- factor(Infiltrationraw$Treatment,
                                        levels=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                                        labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                                 "TSP\nFertilizer"))
    (Infilcurve_MultiRaw <- ggplot(Infiltrationraw, aes(x = TimeH, y = CI/10, fill=Block)) + #fill by block shows individual lines for each plot
      facet_wrap(~ Treatment, scales="free")+
      geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) + # higher span reduces the curvature of the line
      geom_point() + # shows a smooth combined line for all plots
      labs(x = "Time (hours)", y = "Cumulative Infiltration (cm)")+
      theme(axis.title = element_text(size=18, face="bold"), axis.text = element_text(size=14, face="bold"),
            strip.text.x = element_text(size = 14, face="bold"), strip.background = element_blank(), legend.position = "bottom",
            legend.key.size=unit(9,"mm"), legend.text=element_text(size=14, face="bold"), legend.title = element_blank(),
            panel.border=element_blank(), panel.grid.major=element_blank(), panel.background = element_blank(),
            panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(Infilcurve_MultiRaw, file="Raw_InfiltrationMultiCurve.jpg", width=10, height=8, dpi=150) 
    (Infilcurve_SingleRaw <- ggplot(Infiltrationraw, aes(x = TimeH, y = CI/10)) +
        facet_wrap(~ Treatment, scales="free")+
        geom_smooth(method="loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) +
        geom_point() +
        labs(x = "Time (hours)", y = "Cumulative Infiltration (cm)")+
        theme(axis.title = element_text(size=18, face="bold"), axis.text = element_text(size=14, face="bold"),
              strip.text.x = element_text(size = 14, face="bold"), strip.background = element_blank(), legend.position = "bottom",
              legend.key.size=unit(9,"mm"), legend.text=element_text(size=14, face="bold"), legend.title = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(), panel.background = element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
    ggsave(Infilcurve_SingleRaw, file="Raw_InfiltrationSingleCurve.jpg", width=10, height=8, dpi=150)
# Check curves - outliers removed
    (Infilcurve_MultiFinal <- ggplot(Infil, aes(x = Time, y = CI, fill=Block)) + 
        geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) +
        geom_point() +
        labs(x = "Time (hours)", y = "Cumulative Infiltration (mm)"))
    ggsave(Infilcurve_MultiFinal, file="OutlierRemoved_InfiltrationMultiCurve.jpg", width=10, height=10, dpi=150) 
    (Infilcurve_SingleFinal <- ggplot(Infil, aes(x = Time, y = CI)) +
        facet_wrap(~ Treatment, scales="free")+
        geom_smooth(method="loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) +
        geom_point() +
        labs(x = "Time (hours)", y = "Cumulative Infiltration (mm)"))
    ggsave(Infilcurve_SingleFinal, file="OutlierRemoved_InfiltrationSingleCurve.jpg", width=10, height=10, dpi=150)
    
# Multivariate approach - I tried Mahalanobis but couldn't get it to work
      

# BULK DENSITY AND VOLUMETRIC WATER CONTENT ----
# Outlier checks done in Field R.script
Field$BDwet <- as.numeric(Field$BDwet)
Field$BDdry <- as.numeric(Field$BDdry)
Field$MoistWet <- as.numeric(Field$MoistWet)
Field$MoistDry <- as.numeric(Field$MoistDry)
  ##  Bulk density - wet   ----
  #NORMAL
    descr(Field$BDwet, na.rm=TRUE)
    # skewness -0.54; kurtosis -0.61
    shapiro.test(Field$BDwet) # p=0.51
    hist(Field$BDwet) # mostly normal, two spikes
    leveneTest(BDwet~Treatment, data=Field)  # 0.033
  ## Bulk density - dry   ----
    descr(Field$BDdry, na.rm=TRUE)
    # skewness 0.01; kurtosis -0.74
    shapiro.test(Field$BDdry) # p=0.43
    hist(Field$BDdry) # mostly normal
    leveneTest(BDdry~Treatment, data=Field)  # 0.55
  ## Volumetric moisture - wet   ----
    descr(Field$MoistWet, na.rm=TRUE)
    # skewness 0.35; kurtosis-0.71
    shapiro.test(Field$MoistWet) # p=0.63
    hist(Field$MoistWet) # slight left skew
    leveneTest(MoistWet~Treatment, data=Field)  # 0.76
  ## Volumetric moisture - dry   ----
  # Data non-normal, transformations did not improve it  
    descr(Field$MoistDry, na.rm=TRUE)
    # skewness 0.46; kurtosis 0.31
    shapiro.test(Field$MoistDry) # p=2.18e-06
    hist(Field$MoistDry) # mostly normal, gaps
    leveneTest(MoistDry~Treatment, data=Field)  # 0.16
    shapiro.test(log(Field$MoistDry)) # p=2.11e-06
    hist(log(Field$MoistDry)) # scattered bars
    leveneTest(log(MoistDry)~Treatment, data=Field)  # 0.15
    shapiro.test(sqrt(Field$MoistDry)) # p=2.33e-06
    hist(sqrt(Field$MoistDry)) # scattered bars
    leveneTest(sqrt(MoistDry)~Treatment, data=Field)  # 0.15



# MODEL AND GRAPH INFILTRATION DATA ----
  ## Model predicted data using Philips model ----
     ### Set up the new database to include values from both Infilstration.csv and Infil.csv
     ### DO NOT RUN THE DIVISION CODE MORE THAN ONCE!!!!!!
        Infil$CI <- Infil$CI/10 # change CI to cm
        Infil$Infiltration <- Infil$Infiltration/10 # set infiltration to the same scale as CI (cm)
        Infil$Time <- Infil$Time/3600 # csale time to hours
      # select columns from Infilsub to be combined with Infil
        InfilSelect <- dplyr::select(Infilsubraw, Treatment, Block, S, K) 
        InfilDF <- merge(Infil, InfilSelect, by = c("Treatment", "Block"), all.x = TRUE) # merge selected columns with Infil data frame
        View(InfilDF <- InfilDF[complete.cases(InfilDF), ])
        ### need to use a non-linear model to do a best fit and prediction - using Philips two term model as best option
        InfilInterpol <- function(data) {
          model <- nls(CI ~ S * Time^0.5 + K, data = data, start = list(S = 0.01, K = 0.01)) #, maxiter=100
          CappedInfilTime <- data %>%
            group_by(Treatment, Block) %>%
            summarise(Time = c(Time, 2)) %>% # interpolate data at time=2 hours (can specify in min/s depends on time scale)
                # mutate works better to get past the warning but I gave up trying to get everything to work
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
        ### set up new data frame with PredictedCI, CI and Infiltration columns along with new row for Time=5400
        empty_row <- InfilDF %>%
          distinct(Treatment, Block) %>%
          mutate(Time = 2, Infiltration = NA, CI = NA)
        InfilDF_updated <- bind_rows(InfilDF, empty_row) %>%
          arrange(Treatment, Block, Time)
        InfilCombined <- bind_cols(InfilPredicted, dplyr::select(InfilDF_updated, Infiltration, CI))
        print(InfilCombined, n=30)
        View(InfilCombined)
        write_xlsx(InfilCombined, path="Predicted infiltration data.xlsx")
    
  
  ## Visualizations ----
     # reshape the data into long format
        InfilReady <- filter(InfilCombined, Time <= 2)
        InfilReady_long <- InfilReady%>%
          pivot_longer(cols = c(CI, PredictedCI, Infiltration),
                       names_to = "Variable", values_to = "Value")
        View(InfilReady_long)
        color_palette <- c("darkred", "steelblue", "green4")
        #transparent_palette <- alpha(color_palette, alpha = 0.8)  # Adjust the alpha value as needed (0 = fully transparent, 1 = fully opaque)
        InfilLegend <- c("Cumulative infiltration (CI)", "Measured infiltration", "Predicted CI")
        InfilReady_long$Treatment <- factor(InfilReady_long$Treatment,
                                            levels=c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP", "Phosphorus"),
                                            labels=c("Control 1", "Control 2","Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                                                     "TSP\nFertilizer"))
        (InfilPlot <- ggplot(InfilReady_long, aes(x=Time, y=Value, color=Variable)) + #fill by block shows individual lines for each plot
            facet_wrap(~ Treatment, scales="free")+
            geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1, aes(fill=Variable)) + 
                #aes(fill) sets the colour of the error bands
                # higher span reduces the curvature of the line
                # SE in geom_smooth adds the error band, can also use confidence band
            scale_color_manual(values = color_palette, labels = InfilLegend, guide="none") + # sets the colour of the lines
            labs(x = "Time (hours)", y = "Infiltration (cm)")+
            theme(axis.title = element_text(size=18, face="bold"), axis.text = element_text(size=14, face="bold"),
              strip.text.x = element_text(size = 14, face="bold"), strip.background = element_blank(), legend.position = "bottom",
              legend.key.size=unit(9,"mm"), legend.text=element_text(size=14, face="bold"), legend.title = element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(), panel.background = element_blank(),
              panel.grid.minor=element_blank(), axis.line=element_line(colour="black")))
        ggsave(InfilPlot, file="Infiltration curves.jpg", width=10, height=8, dpi=150)
# warning related to blank outlier data for CI & I


# OBTAIN ALL SLOPE AND INTERCEPT VALUES ----    
  ## Initial and final infiltration rates ----
        InitFinRate <- InfilCombined %>% # Extract the initial and final rates (final rate at t=2h as per predicted modelling)
          group_by(Block, Treatment) %>%
          summarize( # summarize function has been deprecated, use mutate
            InitialRate = (PredictedCI[1]) / (Time[1]),
            FinalRate = (PredictedCI[Time == 2]) / (Time[Time == 2])
          ) %>%
          ungroup()
        View(InitFinRate)
        write_xlsx(InitFinRate, path="Initial and final rates.xlsx")
        
  ## Estimated slope and intercept values ----
        # Get estimated slope and intercept values from Philips starter model - used to obtain parameters necessary for Philips two term model
        InfilMod <- nlsList(log(CI) ~ SSphilip(Time,fc,S) | Treatment, data=Infil, na.action=na.omit)
        (InfilModSum <- coef(summary(InfilMod))) # prints values for fc (or) and S
        InfilModSumDF <- as.data.frame(InfilModSum)
        write.csv(InfilModSumDF, file="Infiltration Coefficients cm.s.csv")
        # cannot directly get anova and emmeans on the SSphilip output as the output is a list
        # coefficients vastly different from manual calculation - not sure if correct
        
        
  ## Estimate S & K values manually ----  
        ### Use Philips equation for rate of infiltration fp = 0.5*s*t^-0.5 + k where s/2 = slope of trendline and k= intercept
        ### this is also know as dI/dt=0.5St^-0.5 + A
        ## Create an empty data frame to store the slope and intercept values
        InfilNewData <- data.frame(Block = character(), Treatment = character(), Slope = numeric(), Intercept = numeric(), stringsAsFactors = FALSE)
        # Loop over each block and treatment
        for (block in unique(Infiltrationraw$Block)) {
          for (treatment in unique(Infiltrationraw$Treatment)) {
            # Subset the data for the current block and treatment
            InfilNewSub <- Infiltrationraw[Infiltrationraw$Block == block & Infiltrationraw$Treatment == treatment, ]
            # Fit the linear regression model
            InfilReg <- lm(fp ~ thalf, data = InfilNewSub, na.action=na.omit) # basic linear regression to get the trendline for fp (y-axus) and t^-0.5 (x-axis)
            # Extract the slope and intercept values
            InfilSlope <- coef(InfilReg)["thalf"]
            InfilIntercept <- coef(InfilReg)["(Intercept)"]
            # Append the values to the data frame
            InfilNewData <- rbind(InfilNewData, data.frame(Block = block, Treatment = treatment, Slope = InfilSlope, Intercept = InfilIntercept))
          }
        }
        print(InfilNewData)
        write_xlsx(InfilNewData, path="Slope&Intercept.xlsx") # values added manually to Infilraw.csv data set
        
  ## Model Infilsub for significant differences ----
    ### Initial infiltration rate - modelled on predicted CI values
        InfilModInit <- lme(InitialRate~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        ranef(InfilModInit)
        Anova(InfilModInit, type="III")  # NO significant differences
        summary(InfilModInit)
        hist(resid(InfilModInit)) # normal
        shapiro.test(resid(InfilModInit))  #0.9104
        plot(fitted(InfilModInit),resid(InfilModInit),pch=16)   # equal around 0
        qqnorm(resid(InfilModInit)) # almost no tails
        qqline(resid(InfilModInit))
        rsq(InfilModInit) # 0.237
        (InfilModInitEm <- emmeans(InfilModInit,~"Treatment", infer = TRUE)) #, alpha=0.1
        (InfilModInitEm_cld <- cld(InfilModInitEm, Letters=trimws(letters), reversed=TRUE)) #, alpha=0.1
        write_xlsx(InfilModInitEm_cld, path="InfilMod_initial rate.xlsx")
        
        ### Final infiltration rate  - modelled at Time=2 hours
        InfilModFinal <- lme(FinalRate~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        ranef(InfilModFinal)
        Anova(InfilModFinal, type="III")  # significant differences
        summary(InfilModFinal)
        hist(resid(InfilModFinal)) # normal
        shapiro.test(resid(InfilModFinal))  #0.356
        plot(fitted(InfilModFinal),resid(InfilModFinal),pch=16)   # equal around zero
        qqnorm(resid(InfilModFinal)) # small to moderate tails
        qqline(resid(InfilModFinal))
        rsq(InfilModFinal) # 0.563
        (InfilModFinalEm <- emmeans(InfilModFinal,~"Treatment", infer = TRUE)) #, alpha=0.1
        (InfilModFinalEm_cld <- cld(InfilModFinalEm, Letters=trimws(letters), reversed=TRUE)) #, alpha=0.1
        write_xlsx(InfilModFinalEm_cld, path="InfilMod_final rate.xlsx")
        
        ### Moisture percentage  
        InfilModMoist <- lme(MoisturePerc~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        ranef(InfilModMoist)
        Anova(InfilModMoist, type="III")  # NO significant differences
        summary(InfilModMoist)
        hist(resid(InfilModMoist)) # left skew
        shapiro.test(resid(InfilModMoist))  #0.191
        plot(fitted(InfilModMoist),resid(InfilModMoist),pch=16)   # equal around 0
        qqnorm(resid(InfilModMoist)) # moderate tails
        qqline(resid(InfilModMoist))
        rsq(InfilModMoist) # 0.094
        (InfilModMoistEm <- emmeans(InfilModMoist,~"Treatment", infer = TRUE)) #, alpha=0.1
        (InfilModMoistEm_cld <- cld(InfilModMoistEm, Letters=trimws(letters), reversed=TRUE)) #, alpha=0.1
        write_xlsx(InfilModMoistEm_cld, path="InfilMod_Moisture.xlsx")
        
        ### Slope
        InfilModSlope <- lme(Slope~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        ranef(InfilModSlope)
        Anova(InfilModSlope, type="III")  # NO significant differences
        summary(InfilModSlope)
        hist(resid(InfilModSlope)) # normalish
        shapiro.test(resid(InfilModSlope))  #0.027
        plot(fitted(InfilModSlope),resid(InfilModSlope),pch=16)   # heavier below 0
        qqnorm(resid(InfilModSlope)) # moderate right tail
        qqline(resid(InfilModSlope))
        rsq(InfilModSlope) # 0.141
        (InfilModSlopeEm <- emmeans(InfilModSlope,~"Treatment", infer = TRUE)) #, alpha=0.1
        (InfilModSlopeEm_cld <- cld(InfilModSlopeEm, Letters=trimws(letters), reversed=TRUE)) #, alpha=0.1
        write_xlsx(InfilModSlopeEm_cld, path="InfilMod_Slope.xlsx")
        
        ### Sorptivity
        InfilModSorp <- lme(Sorptivity~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        ranef(InfilModSorp)
        Anova(InfilModSorp, type="III")  # NO significant differences
        summary(InfilModSorp)
        hist(resid(InfilModSorp)) # normalish
        shapiro.test(resid(InfilModSorp))  #0.029
        plot(fitted(InfilModSorp),resid(InfilModSorp),pch=16)   # heavier below 0
        qqnorm(resid(InfilModSorp)) # moderate right tail
        qqline(resid(InfilModSorp))
        rsq(InfilModSorp) # 0.144
        (InfilModSorpEm <- emmeans(InfilModSorp,~"Treatment", infer = TRUE)) #, alpha=0.1
        (InfilModSorpEm_cld <- cld(InfilModSorpEm, Letters=trimws(letters), reversed=TRUE)) #, alpha=0.1
        write_xlsx(InfilModSorpEm_cld, path="InfilMod_Sorptivity.xlsx")
        
        ### Hydraulic conductivity
        InfilModK <- lme(K~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        ranef(InfilModK)
        Anova(InfilModK, type="III")  # NO significant differences
        summary(InfilModK)
        hist(resid(InfilModK)) # right skew
        shapiro.test(resid(InfilModK))  #0.0011
        plot(fitted(InfilModK),resid(InfilModK),pch=16)   # clustered above zero
        qqnorm(resid(InfilModK)) # huge left tail
        qqline(resid(InfilModK))
        rsq(InfilModK) # 0.247
        (InfilModKEm <- emmeans(InfilModK,~"Treatment", infer = TRUE, alpha=0.1)) 
        (InfilModKEm_cld <- cld(InfilModKEm, Letters=trimws(letters), reversed=TRUE, alpha=0.1)) 
        write_xlsx(InfilModKEm_cld, path="InfilMod_K.xlsx")

# EXTRACT ANOVA TABLES ----
        InfilModInit <- lme(InitialRate~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        InfilInitAN <- Anova(InfilModInit, type="III")
        InfilInitAN$RowNames <- row.names(InfilInitAN)
        rownames(InfilInitAN) <- NULL
        
        InfilModFinal <- lme(FinalRate~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        InfilFinalAN <- Anova(InfilModFinal, type="III")
        InfilFinalAN$RowNames <- row.names(InfilFinalAN)
        rownames(InfilFinalAN) <- NULL
        
        InfilModMoist <- lme(MoisturePerc~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        InfilMoistAN <- Anova(InfilModMoist, type="III") 
        InfilMoistAN$RowNames <- row.names(InfilMoistAN)
        rownames(InfilMoistAN) <- NULL
        
        InfilModSlope <- lme(Slope~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        InfilSlopeAN <- Anova(InfilModSlope, type="III")
        InfilSlopeAN$RowNames <- row.names(InfilSlopeAN)
        rownames(InfilSlopeAN) <- NULL
        
        InfilModSorp <- lme(Sorptivity~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        InfilSorpAN <- Anova(InfilModSorp, type="III")
        InfilSorpAN$RowNames <- row.names(InfilSorpAN)
        rownames(InfilSorpAN) <- NULL
        
        InfilModK <- lme(K~Treatment,random=~1|Block, data=Infilsub, na.action=na.omit)
        InfilKAN <- Anova(InfilModK, type="III") 
        InfilKAN$RowNames <- row.names(InfilKAN)
        rownames(InfilKAN) <- NULL
        
        InfilANOVAtables <- list(InfilInitAN, InfilFinalAN, InfilMoistAN, InfilSlopeAN, InfilSorpAN, InfilKAN)
        names(InfilANOVAtables) <- c("Initial Rate", "Final Rate", "Moisture", "Slope", "Sorptivity", "Hydraulic Conductivity")
        write_xlsx(InfilANOVAtables, path="Infiltration_ANOVAtables.xlsx")
#Loading data in to R ----
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
  library(soilphysics)
  library(soiltexture)
  library(SoilHyP)
  library(knitr)
  library(HydroMe)
  library(stats)
  library(multcomp)
  library(DataVisualizations)
  library(DatabionicSwarm)
  library(diptest)
  library(ggExtra)
  library(aqp)

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
    Infiltrationraw$Time <- as.numeric(as.character(Infiltrationraw$Time))
    Infiltrationraw$CI <- as.numeric(as.character(Infiltrationraw$CI))
    Infilsubraw$Slope <- as.numeric(Infilsubraw$S)
    Infilsubraw$Block <- factor(Infilsubraw$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
    Infilsubraw$Treatment <- factor(Infilsubraw$Treatment,levels = FieldTrt_order)
# check data from Infilub
    # Slopes calculated using philips model
    ggplot(Infilsubraw, aes(x=Treatment, y=S, fill=Treatment)) +
      geom_boxplot() +
      facet_wrap(~ Treatment, scales="free") +
      labs(x="Treatment", y="Slope (s)")
    ggsave("OutliersField_S.jpg", width=8, height=8, dpi=150)
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
# Check curves - raw data
    (Infilcurve_MultiRaw <- ggplot(Infiltrationraw, aes(x = Time, y = CI, fill=Block)) + #fill by block shows individual lines for each plot
      facet_wrap(~ Treatment, scales="free")+
      geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) + # higher span reduces the curvature of the line
      geom_point() +
      labs(x = "Time (hours)", y = "Cumulative Infiltration (mm)"))
    ggsave(Infilcurve_MultiRaw, file="Raw_InfiltrationMultiCurve.jpg", width=10, height=10, dpi=150) # shows a smooth combined line for all plots
    (Infilcurve_SingleRaw <- ggplot(Infiltrationraw, aes(x = Time, y = CI)) +
        facet_wrap(~ Treatment, scales="free")+
        geom_smooth(method="loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) +
        geom_point() +
        labs(x = "Time (hours)", y = "Cumulative Infiltration (mm)"))
    ggsave(Infilcurve_SingleRaw, file="Raw_InfiltrationSingleCurve.jpg", width=10, height=10, dpi=150)
# Check curves - outliers removed
    (Infilcurve_MultiFinal <- ggplot(Infil, aes(x = Time, y = CI, fill=Block)) + #fill by block shows individual lines for each plot
        facet_wrap(~ Treatment, scales="free")+
        geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) + # higher span reduces the curvature of the line
        geom_point() +
        labs(x = "Time (hours)", y = "Cumulative Infiltration (mm)"))
    ggsave(Infilcurve_MultiFinal, file="OutlierRemoved_InfiltrationMultiCurve.jpg", width=10, height=10, dpi=150) # shows a smooth combined line for all plots
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


# OBTAIN ALL SLOPE AND INTERCEPT VALUES ----    
    ## Initial and final infiltration rates ----
      InitFinRate <- Infiltrationraw %>% # Extract the initial and final rates
        group_by(Block, Treatment) %>%
        summarize(
          InitialRate = (CI[2]/10) / (Time[2]/60/60), #change to cm/h
          FinalRate = (CI[n()]/10) / (Time[n()]/60/60)
        ) %>%
        ungroup()
      View(InitFinRate)
      write.xlsx(InitFinRate, file="Initial and final rates.xlsx")

    ## Estimated slope and intercept values ----
        # Get estimated slope and intercept values from Philips starter model - used to obtain parameters necessary for Philips two term model
        # DO NOT RUN THE DIVISION CODE MORE THAN ONCE!!!!!!
        Infil$CI <- Infil$CI/10 # change CI to cm
        Infil$Infiltration <- Infil$Infiltration/10 # set infiltration to the same scale as CI (cm)
        InfilMod <- nlsList(log(CI) ~ SSphilip(Time,fc,S) | Treatment, data=Infil, na.action=na.omit)
        (InfilModSum <- coef(summary(InfilMod))) # prints values for fc (or) and S
        InfilModSumDF <- as.data.frame(InfilModSum)
        write.csv(InfilModSumDF, file="Infiltration Coefficients cm.s.csv")

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
          write.xlsx(InfilNewData, file="Slope&Intercept.xlsx") # values added manually to Infilraw.csv data set
          
    ## Model Infilsub for significant differences ----
        ##
        ModFieldLNO3b <- lme(LNO3~Treatment,random=~1|Block, data=Field, na.action=na.omit)
    
   
# MODEL AND GRAPH INFILTRATION DATA ----
    ## Interpolate time and depth data ----
        ### Set up the new database
        InfilSelect <- select(Infilsub, Treatment, Block, S, K) # select columns from Infilsub to be combined with Infil
        View(InfilDF <- merge(Infil, InfilSelect, by = c("Treatment", "Block"), all.x = TRUE)) # merge selected columns with Infil data frame
          # create empty data frame to hold extrapolated data
        CappedInfilTime <- tibble(Treatment=character(), Block=character(), Time=numeric(), CI=numeric(), IncrementDepth=numeric(), 
                                  TimeInterval=numeric(), thalf=numeric(), th=numeric(), S=numeric(), K=numeric())
        InfilExclude <- data.frame(Treatment = c("Control1", "Control2", "Biochar10tha"),
                                   Block = c("Block4", "Block1", "Block2"))
        for (treatment in unique(InfilDF$Treatment)) {
          for (block in unique(InfilDF$Block)) {
            # Check if the combination is in the exclusion list and skip the combination
            if (any(InfilExclude$Treatment == treatment & InfilExclude$Block == block)) {
              next
            }
            # Check if the combination already has a value at Time=5400
            if (!any(InfilDF$Time == 5400 & InfilDF$Treatment == Treatment & InfilDF$Block == Block)) {
              CappedInfilTime <- CappedInfilTime %>%
                add_row(Treatment=treatment, Block=block, Time=5400, CI=NA, IncrementDepth=NA, TimeInterval=NA, 
                        thalf=NA, th=NA, S=NA, K=NA) # Add a row with Time=5400 and NA for everything else
            }
          }
        }
        print(CappedInfilTime)
        InfilExpanded <- bind_rows(InfilDF, CappedInfilTime)%>%
               arrange(Treatment, Block, Time)
        InfilExpanded <- InfilExpanded[complete.cases(InfilExpanded), ]
        View(InfilExpanded)
    ## Model predicted data using Philips model ----
        ### need to use a non-linear model to do a best fit and prediction - using Philips two term model as best option
        InfilInterpol <- function(data) {
          model <- nls(CI ~ S * Time^0.5 + K, data = data, start = list(S = 0.01, K = 0.01)) #, maxiter=100
          PredictedCI <- predict(model, newdata = data.frame(Time = data$Time))
          data$InterpolatedCI <- PredictedCI
          return(data)
        }
         InfilExtrapolate <- InfilExpanded %>%
          group_by(Treatment, Block) %>%
          do(InfilInterpol(.))
        View(InfilExtrapolate)
        write.xlsx(InfilExtrapolate, file = "Predicted infiltration data.xlsx")
    
  
      ## Visualizations ----
        # reshape the data into long format
        InfilReady <- filter(InfilExtrapolate, Time <= 7200)
        InfilReady_long <- InfilReady%>%
          pivot_longer(cols = c(CI, InterpolatedCI, Infiltration),
                       names_to = "Variable", values_to = "Value")
        View(InfilReady_long)
        (InfilPlot <- ggplot(InfilReady_long, aes(x=Time, y=Value, color=Variable)) + #fill by block shows individual lines for each plot
            facet_wrap(~ Treatment, scales="free")+
            geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95, span = 1) + # higher span reduces the curvature of the line
              # SE in geom_smooth adds the error band, can also use confidence band
            scale_color_manual(values = c("darkred", "steelblue", "green4"), labels = c("Cumulative\ninfiltration (CI)", 
                                                                                        "Measured\ninfiltration", "Predicted CI"))+
            labs(x = "Time (seconds)", y = "Infiltration (cm)")+
            theme(legend.title = element_blank()))
        ggsave(InfilPlot, file="Infiltration curves.jpg", width=10, height=10, dpi=150)
        
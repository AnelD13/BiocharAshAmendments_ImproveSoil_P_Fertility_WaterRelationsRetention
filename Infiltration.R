##### Loading data in to R ####
Infil<-read.csv("Infiltration.csv", fileEncoding="UTF-8-BOM")
View(Infil)
Infilraw<-read.csv("Infiltrationraw.csv", fileEncoding="UTF-8-BOM")
Infilsub<-read.csv("Infil.csv", fileEncoding="UTF-8-BOM")

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

##### Summary and ordering of data   ####
#Check for missing values in a specific field
missing <- colSums(is.na(Infil[,]))
print(missing)
missing <- colSums(is.na(Infilraw[,]))
print(missing)

#Change columns in a dataframe to factors/categorical values, str displays 
Trt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP","Phosphorus")
Infil$Block <- factor(Infil$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Infil$Treatment <- factor(Infil$Treatment,levels = Trt_order)
Infil$Time <- as.numeric(as.character(Infil$Time))
Infil$CI <- as.numeric(as.character(Infil$CI))
Infil$mDry <- as.numeric(as.character(Infil$mDry))
Infil$mWet <- as.numeric(as.character(Infil$mWet))
summary(Infil)
str(Infil) #displays the structure of the object


#Canadian soil texture classification
TT.plot(class.sys="Ca.EN.TT")



#####   Check for outliers   ####
## Univariate approach = incorrect

ggplot(Infilraw, aes(x = Treatment, y = CI)) +
  geom_boxplot(outlier.color = "red") +
  labs(x = "Treatment", y = "Cumulative Infiltration")
ggsave("Outliers_infiltration.jpg", width = 10, height = 8, dpi = 600)
ggplot(Infilraw, aes(x = Treatment, y = Time)) +
  geom_boxplot(outlier.color = "red") +
  labs(x = "Treatment", y = "Time")
ggsave("Outliers_InfilTime.jpg", width = 10, height = 8, dpi = 600)
## Multivariate approach
InfilMal <- mahalanobis(Infilraw %>% # Calculate Mahalanobis distance for each obs
  group_by(Treatment) %>%
  summarise(mahalanobis=mahalanobis(select(., CI, Time), center = colMeans(select(., as.numeric(CI), as.numeric(Time))), 
                                    cov = cov(select(., as.numeric(CI), as.numeric(Time))))))
threshold <- 3 # Identify potential outliers using a threshold value (e.g., 3)
outliers <- which(mahalanobis > threshold)
cat("Potential outliers:", outliers, "\n") # Print the indices of potential outliers

MDplot(Infil)


#boxplot of summarized data
Trt_order <- c("Control1", "Control2", "Biochar25kgPha", "Biochar10tha", "Biochar10thaTSP","Phosphorus")
Infilsub$Block <- factor(Infilsub$Block, levels=c("Block1", "Block2", "Block3", "Block4"))
Infilsub$Treatment <- factor(Infilsub$Treatment,levels = Trt_order)
ggplot(Infilsub, aes(x = Treatment, y = Infil, pattern=Treatment)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Infiltration (cm/h")+
  scale_x_discrete(labels = c("Control 1", "Control 2", "Biochar\n25kgP/ha", "Biochar\n10t/ha", "Biochar\n10t/ha&TSP",
                              "Fertilizer\nPhosphorus"))+
  scale_pattern_manual(values = c("stripe", "crosshatch", "wave", "horizontal", "vertical", "dashed", "dotted", "zigzag")) +
  theme_bw() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=18, face="bold", colour="black"),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 22, face="bold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("Infiltration_rough.jpg", width = 10, height = 8, dpi = 150)



#####    Model infiltration data    #####

  
## Philips model
coef_list <- lapply(split(Infil, Infil$Treatment), function(sub) {
  Infil$CIAdj <- Infil$CI + 0.000001
  Infil$TimeAdj <- Infil$Time + 0.000001
  InfilMod1 <- nlme::gnls(CIAdj ~ (Kf * (TimeAdj ^ alpha)) / alpha, 
                              data = Infil, 
                              start = list(Kf = 0.05, alpha = 12),
                              weights = nlme::varIdent(form = ~1 | Treatment),
                              control = nlme::gnlsControl(maxIter = 1000),
                              correlation = nlme::corAR1(form = ~Time | Block/Treatment),
                              na.action = na.exclude)
  coef(InfilMod1)
})
View(coef_list)
# store coefficients in a table
coef_table <- coef(InfilMod1, unlist=FALSE)
colnames(coef_table) <- c("Estimate", "Std.Error")
coef_table$Std.Error <- summary(InfilMod1)$tTable[, "Std.Error"]
rownames(coef_table) <- names(coef(InfilMod1))
coef_table

coef_table <- cbind(coef_table, summary(InfilMod1)$tTable[, "Std.Error"])
rownames(coef_table) <- names(InfilMod1)


coef_table <- coef(InfilMod1, unlist=FALSE)
coef_list <- lapply(InfilMod1$model$Treatment, function(t) coef(update(InfilMod1, subset = Treatment == t)))
View(coef_list)


anova(InfilMod1)



# Horton model



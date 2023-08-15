# Biochar and ash amendments to improve soil phosphorus fertility, water relations and retention

![UofS](https://github.com/AnelD13/BiocharAshAmendments_ImproveSoil_P_Fertility_WaterRelationsRetention/assets/126522316/8cc3b6ec-fb83-4fa2-857b-0d7f4737181b) ![Rstudio](https://github.com/AnelD13/BiocharAshAmendments_ImproveSoil_P_Fertility_WaterRelationsRetention/assets/126522316/87a73af9-27d4-4cf9-b235-9c382bfdb5b2)  ![ChatGPT](https://github.com/AnelD13/BiocharAshAmendments_ImproveSoil_P_Fertility_WaterRelationsRetention/assets/126522316/d5756f16-2ec1-4040-9996-73b01d7d576b)

Project data and analytical code for the MSc thesis project titled: Biochar and ash amendments to improve soil phosphorus fertility, water relations and retention.

## Code:
**Pots1** - associated data files: Pots1, Pots1raw  
**RowStudy** - associated data files: Rows, Rowsraw  
**Pots2** - associated data files: Pots2, Pots2raw  
**Field** - associated data files: Field, Fieldraw, Fieldsplitraw  
**Infiltration** - associated data files: Infil, Infilraw, Infiltration, Infiltrationraw, Field  
**Soilsncropsv1** - outdated first attempt, absorbed into other files  

## General
- Most analysis was done at alpha level 0.05 or 5%, except where natural soil variation was excessive, in which case these variables were assessed at alpha level 0.1 or 10%.
- Scripts have been set up to 1) load data, 2) call libraries, 3) check initial summary data, 4) check for outliers and 5) analyze the data.
- Except for infiltration, scripts have been set up for plant analysis first, followed by soil analysis, and then varying analysis per study. The last component for all scripts is an extract of the ANOVA results.
- Outliers were checked using boxplots / quartile ranges and a separate grubbs test in JMP (not uploaded). The grubs test is less sensitive to this data and identified no outliers not identified in the quartlie ranges.
- Graphics are included with each analysis/model where appropriate / as used in the thesis.
- Notes have been made regarding summary and analysis outcomes for each variable.
- Except for infiltration, the assumption was made that all data is linear in nature. Infiltration data is curvilinear/exponential.
- Most analysis was univariate except for yield as a function of nitrogen and phosphorus, Principal Component Analysis, covariance and correlation analysis.
- All experiments had four replications.
- Data sets provided have been cleaned. For calculations refer to thesis.

## Pots 1
- Completely randomized design (CRD) with two factors (2x soil & 15x treatment). 
- Various mixed linear models used - whichever was the best fit was chosen to be used with emmeans. Best fit determined using R squared values and AIC/BIC.
- Plant analysis used in thesis: biomass, nitrogen and phosphorus uptake and recovery and Nutrient Use Efficiency for nitrogen and phosphorus. N & P concentrations were only used to calculate uptake.
- Soil analysis - all variables in dataset were used.
- Water holding capacity - data directly in R script. No replications & no analysis.
- Covariance heat maps done for yield (biomass), P uptake and P recovery for each soil type.
- Yield as a function of the interaction between N & P, displyed as a contour map.
- Principle component analysis (PCA) and eigenvalues for each soil.
- Correlation between the phosphorus content of the char, the soil residual phosphorus and phosphorus recovery.

## Row Study
- CRD with 8 Treatments.
- Crop failure in field due to grasshopper infestation.
- Plant and soil analysis - as per Pots 1 (no Nutrient Use Efficiency).
- Covariance heat maps, yield to N & P uptake and recovery, PCA & eigenvalues and correlation of char, soil and plant P fractions - as per Pots 1.

## Pots 2
- Soil cores used from row study. CRD with 8 treatments.
- Plant biomass analysis split into grain and straw. Only N & P uptake calculated - recovery not possible due to grasshoppers (see thesis).
- Soil analysis - all variable in dataset were used.
- Leachate study on cores - analysis PO4, NO3 and NH4.
- Covariance heat maps for yield (total biomass) and P uptake.
- Yield as a function of N & P uptake (instead of recovery).
- PCA, eigenvalues and correlation of char, soil and plant P fractions - as per Pots 1.

## Field
- Plant biomass analysis split into grain and straw. N and P uptake and recovery calculated along with Nutrient Use Efficiency for N & P.
- Soil analysis done at 3 depth increments (0-10, 10-20 & 20-30cm). Auto & cross correlation of soil PO4 showed no viable results due to limited information, thus this was not further investigated.
- Snowmelt data analysed for NO3, NH4, Resin NO3, PO4 and Resin PO4.
- Covariance heat maps, yield to N & P uptake recovery, PCA & eigenvalues and correlation of char, soil and plant P fractions - as per Pots 1.

## Infiltration
- Raw data was plotted to determine outliers, along with an lme model and emmeans of the infiltration rates (I & CI), slope (S/2), soprtivity (S), hydraulic conductivity (Kfs = A) and moisture.
- The Philip sorptivity model was used i = St^(1/2) + At  &  v_0=1/2 St^(-1/2)+A, the latter equation to determine S & A for use in the first model.
- Data analysis was reiterative - Slope and a parameter approaching hydraulic conductivity (A) were estimated and then fed back into the Philips model after outliers were removed to obtain a predicted curve.
- Predicted cumulative infiltration was capped at 2 hours as a standardized output.
- Moisture was reported on but not bulk density. Data is available in the data files.

## Thesis and license
Link to thesis: To be published   
License: MIT

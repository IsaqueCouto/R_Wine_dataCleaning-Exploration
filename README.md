# Wine Data Cleaning and Exploration in R

## Overview 

This dataset came with 14 columns and 7500 rows 

- [x] **Data.frame  :	7500 obs. of  14 variables:**
- [x] **SN**   : chr  : Serial number of the sample 
- [x] **NAME** : chr  : Winery name
- [x] **WINE** : chr  : Name of the wine
- [x] **YR**   : chr  : Year in which the grapes were harvested – indicator of the AGE of the wine. 
- [x] **REG**  : chr  : Region of the wine
- [x] **TP**   : chr  : Wine variety
- [x] **RT**   : num  : Average rating given to the wine by the test users [from 1-5]
- [x] **NUMR** : int  : Number of testers that reviewed the wine
- [x] **PR**   : num  : Price in euros [€]
- [x] **BD**   : int  : Body score, defined as the richness and weight of the wine in your mouth [from 1-5]
- [x] **ACD**  : int  : Acidity score, defined as wine's “pucker” or tartness; it's what makes a wine refreshing and your tongue salivate and want another sip [from 1-5]
- [x] **RSG**  : chr  : residual sugar level of the wine [from 0 -16] 
- [x] **AL**   : chr  : Alcohol percentage of the wine. 
- [x] **DN**   : chr  : The typical density or specific gravity of the wine is generally between 1.080 and 1.090.


### Charts from the analysis 

- Code to see the correlation between all the numeric values in a chart form using ggplot
'''javascript
Burgundy %>% 
  na.omit() %>%
  select_if(is.numeric) %>%
  chart.Correlation(); 
  '''
  
  
  



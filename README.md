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

- First we wanted to see which values correlated with one another if there were any correlations. 
- Code to see the correlation between all the numeric values in a chart form using ggplot
```
Burgundy %>% 
  na.omit() %>%
  select_if(is.numeric) %>%
  chart.Correlation(); 
```
<img width="774" alt="Screenshot 2023-01-02 at 9 05 25 PM" src="https://user-images.githubusercontent.com/120685725/210293369-9cbf2380-14d0-4959-89e7-5b1494942481.png">




***

### Cluster Graph with 3 levels to see if the year the wine was made impacted the price. 

- Final results after analysis is that the year did not impact the price. There were very cheap old wine as well as expensive and newer made wines that were both cheap, expensive and moderatly priced. 

<img width="779" alt="Screenshot 2023-01-02 at 11 22 15 PM" src="https://user-images.githubusercontent.com/120685725/210300067-af5abd0b-c0d7-4c38-9b85-5523774ac025.png">

```
#Cluster Year and Price 

str(Burgundy);
Year_PR <- Burgundy;

Year_PR <- Year_PR[-c(1,2,3,5,6,7,8,10,11,12,13,14,15,16)];
str(Year_PR);

Year_PR <- na.omit(Year_PR);
summary(Year_PR$YR);

plot(Year_PR, pch=21);

yearPrice <- kmeans(Year_PR, 3); # 4 clusters*
yearPrice;

Year_PR$Best_Year <- yearPrice$cluster;
Year_PR$Best_Year[Year_PR$Best_Year==1] <- "Best Year";
Year_PR$Best_Year[Year_PR$Best_Year==2] <- "Average Year";
Year_PR$Best_Year[Year_PR$Best_Year==3] <- "Worse Year";

plot(Year_PR[,c(1,2)], pch=21, bg = yearPrice$cluster * 2 + 3 );
```



***


### Cluster Graph to see which which region produced the most expensive wine 

- After doing the cluster we found that the number 59 repeated a total of 76 times in the more expensive class. More than any other number 
59 is the 59th level in the factor for Region which is Ribera del Duero. We can conclude that Ribera del Duero is the region that produces the most expensive wine 

<img width="779" alt="Screenshot 2023-01-02 at 11 27 41 PM" src="https://user-images.githubusercontent.com/120685725/210300620-8a22ee0d-0e00-437b-b1f5-e3d1b71f14dd.png">

```
#cluster to find Region with the most expesive wines 

str(Burgundy);

REG_PR<- Burgundy;

REG_PR <- REG_PR[-c(1,2,3,4,6,7,8,10,11,12,13,14,15,16)];

str(REG_PR);

REG_PR$REG <- factor(REG_PR$REG);
levels(REG_PR$REG);

REG_PR$REG <- as.numeric(REG_PR$REG);

apply(REG_PR, 2, missingprob);

levels(REG_PR$REG);
summary(REG_PR$REG);
plot(REG_PR, pch=21);

yearPrice <- kmeans(REG_PR, 3); # 3 clusters*
yearPrice;

REG_PR$Best_REG <- yearPrice$cluster;
REG_PR$Best_REG[REG_PR$Best_REG==1] <- "Least Expensive";
REG_PR$Best_REG[REG_PR$Best_REG==2] <- "Average Price";
REG_PR$Best_REG[REG_PR$Best_REG==3] <- "Most Expensive";

plot(REG_PR[,c(1,2)], pch=21, bg = yearPrice$cluster * 2 + 3 );



Most_Expensive <- subset(REG_PR, Best_REG=="Most Expensive");
View(Most_Expensive)

#Here im calculating which Region produces produces expensive wine 
# the most which is the 59th level in region. 

table(Most_Expensive$REG);

Region <- Burgundy;
Region <- Region[-c(1,2,3,4,6,7,8,10,11,12,13,14,15,16)];
str(Region);
Region$REG <- factor(Region$REG);

levels(Region$REG);
```



  
  



install.packages("ggcorrplot");
install.packages("dplyr");
library(dplyr);
library(ggplot2);
library(ggcorrplot);
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#data.frame':	7500 obs. of  14 variables:
# $ SN  : chr  Serial number of the sample 
# $ NAME: chr  Winery name
# $ WINE: chr  Name of the wine
# $ YR  : chr  Year in which the grapes were harvested – indicator of the AGE of the wine. 
# $ REG : chr  Region of the wine
# $ TP  : chr  Wine variety
# $ RT  : num  Average rating given to the wine by the test users [from 1-5]
# $ NUMR: int  Number of testers that reviewed the wine
# $ PR  : num  Price in euros [€]
# $ BD  : int  Body score, defined as the richness and weight of the wine in your mouth [from 1-5]
# $ ACD : int  Acidity score, defined as wine's “pucker” or tartness; it's what makes a wine refreshing and your tongue salivate and want another sip [from 1-5]
# $ RSG : chr  residual sugar level of the wine [from 0 -16] 
# $ AL  : chr  Alcohol percentage of the wine. 
# $ DN  : chr  The typical density or specific gravity of the wine is generally between 1.080 and 1.090.


Burgundy <- read.csv("/Users/isaquecouto/Desktop/final project/BurgundySip.csv", stringsAsFactors = F);
summary(Burgundy);
str(Burgundy);
View(Burgundy);
sum(is.na(Burgundy)); 

#__________________________________________________________________________________________________


#Changing variables to numeric / factors

#changing RSG, AL and DN into numeric integers
Burgundy$RSG <- as.numeric(Burgundy$RSG, na.rm = T);
Burgundy$AL <- as.numeric(Burgundy$AL, na.rm = T);
Burgundy$DN <- as.numeric(Burgundy$DN, na.rm = T);
str(Burgundy)

#changing year into integer
Burgundy$YR <- as.integer(Burgundy$YR)


#Making Type of wine into factor
summary(Burgundy$TP);

Burgundy$TP[Burgundy$TP %in% c("Chardonnay", "Montsant Red", "Albarino", 
                               "Sparkling", "Verdejo", "Cabernet Sauvignon", 
                               "Mencia" ,"Syrah", "Cava", "Monastrell", "Sauvignon Blanc", "")] <- "Other"


Burgundy$TP <- factor(Burgundy$TP);
summary(Burgundy$TP);
str(Burgundy$TP);
levels(Burgundy$TP)

#Ordering the Levels from The ones that repeated the most to the least and then others

Burgundy$TP <- factor(Burgundy$TP,
                      levels = c("Ribera Del Duero Red", "Rioja Red", "Priorat Red", 
                                 "Red", "Toro Red", "Tempranillo", "Sherry", "Rioja White",
                                 "Grenache", "Pedro Ximenez", "Other"));
summary(Burgundy$TP);
str(Burgundy$TP);
levels(Burgundy$TP)
str(Burgundy);


#_______________________________________________________________________________________________

#Here im checking the whole dataset if there are any duplicates
anyDuplicated(Burgundy);

#After analysing the data i noticed the repeating SN, and since SN is usually unique
#I decided to check for any duplicates with the same SN, PR and YR which i think 
#are the more important variables
dups <-
  duplicated(Burgundy$SN) |
  duplicated(Burgundy$SN, fromLast=TRUE);
dups;
View(Burgundy[dups,]);


#Removed the duplicates with the same "NUMR", "PR", "WINE"
Burgundy <- Burgundy[!duplicated(Burgundy$SN,fromLast = TRUE),];
View(Burgundy)


#___________________________________________________________________________________________________

#checking how many na values
sum(is.na(Burgundy)); 

#Function to calculate the % of NA values
missingprob <- function(x){
  return(sum(is.na(x))/length(x)*100);
}

apply(Burgundy, 2, missingprob);

#__________________________________________________________________________________________________


#Because SN and REG have a very low percent in NA i will remove them. 
apply(Burgundy, 2, missingprob);
View(Burgundy)

Burgundy <- Burgundy[!(is.na(Burgundy$SN) | is.na(Burgundy$REG)),]

apply(Burgundy, 2, missingprob);

#_________________________________________________________________________________________

#Removing all NA to check the correlation between variables 
Burgundy %>% 
  na.omit() %>%
  select_if(is.numeric) %>%
  chart.Correlation()

cleanBurgundy <- na.omit(Burgundy);

check <- cleanBurgundy[-c(1,2,3,4,5,6)];
str(check);
cor(check); 
View(cleanBurgundy)
boxplot(check);


#_________________________________________________________________________________________________

#Aggregation to treat RSG
#By looking at the correlation i see that RT and RSG has a almost 
#perfect correlation with a 0.934 correlation coefficient. to treat im using the aggregation 
#function to find the average between them and then implament the average into the missing values 

cor(Burgundy$RSG, Burgundy$RT)

RSG_RT <- aggregate(RSG~RT , data = Burgundy, FUN = mean, na.rm=T);
RSG_RT


Burgundy1 <- merge(Burgundy,RSG_RT, by = "RT");
View(Burgundy1);

#substituting all the NA values in RSG.x with the values in RSG.y
Burgundy1[is.na(Burgundy1$RSG.x),"RSG.x"] <-
  Burgundy1[is.na(Burgundy1$RSG.x),"RSG.y"];

#Removing the column RSG.y from dataframe since we dont need it anymore. 
Burgundy<-Burgundy1[,-15];

#changing the RSG.x column name to RSG
colnames(Burgundy)[12] <- "RSG";

#Reordering the colnames back to the original order.
Burgundy <- Burgundy[,c("SN","NAME","WINE","YR","REG","TP","RT",
                        "NUMR","PR","BD","ACD","RSG","AL","DN")];

#Checking the % of missing values 
apply(Burgundy, 2, missingprob);



#_____________________________________________________________________________________

#Regression to treat Alcohol level 

str(Burgundy)
ALdata <- Burgundy[-c(1,2,3,4,5,6,8,9,10,11,14)];
View(ALdata);

ALdata %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  chart.Correlation()

#Trained Alcohol data
trainAL <- na.omit(ALdata);
trainAL;

#Linear regression model TEST
ALLM <- lm(AL~.,data = trainAL);
summary(ALLM);

ALLM_RSG <- lm(AL~RSG,data = trainAL);
summary(ALLM_RSG);

ALLM_RT <- lm(AL~RT,data = trainAL);
summary(ALLM_RT);

#Non-Linear Regression model TEST 
ALnonLM <- loess(AL~.,data = trainAL);
summary(ALnonLM);

ALnonLM_RSG <- loess(AL~RSG,data = trainAL);
summary(ALnonLM_RSG);

ALnonLM_RT <- loess(AL~RT,data = trainAL);
summary(ALnonLM_RT);


#Final Prediction Model 
predictAL <- ALdata[is.na(ALdata$AL),];
predictAL;

prediction <- predict(ALnonLM_RT,newdata = predictAL);
prediction;

Burgundy$AL[is.na(Burgundy$AL)] <- prediction;

#checking missing prob
apply(Burgundy, 2, missingprob);

#re-checking correlation 
cleanBurgundy <- na.omit(Burgundy);

View(Burgundy)

#___________________________________________________________________________________________

# Logrithmic Regression to treat Price 

check <- cleanBurgundy[-c(1,2,3,4,5,6)];
cor(check); 

str(Burgundy);
pricedata <- Burgundy[-c(1,2,3,4,5,6,7,8,10,11,13,14)];
View(pricedata);

pricedata %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  chart.Correlation()

cleaned <- na.omit(pricedata);

logPrice <- log10(cleaned$PR);
logPrice;
logRSG <- log(cleaned$RSG);
logRSG;

cor(logPrice,logRSG);
plot(logPrice,logRSG);

pricedata$PR_LOG <- log10(pricedata$PR);
pricedata$RSG_LOG <- log10(pricedata$RSG);

str(cleaned);
plot(cleaned);
cor(trainedset);
View(pricedata)

#Trained Alcohol data
trainedset <- na.omit(pricedata);

#Linear regression model TEST
PRLinearM <- lm(PR_LOG ~ RSG_LOG, data = trainedset);
summary(PRLinearM);


View(trainedset)

#Final Prediction Model 
predict_PR <- pricedata[is.na(pricedata$PR_LOG),];
predict_PR;

prediction_2 <- predict(PRLinearM, newdata = predict_PR);
prediction_2;

#Because i used log10, to now convert the predicted missing values from log to 
#the original putting 10 to the power of the prictions to get original. 
prediction_2 = 10^prediction_2
prediction_2;

#Now im adding the missing values in price with this prediciton model. 
Burgundy$PR[is.na(Burgundy$PR)] <- prediction_2;

#checking missing prob
apply(Burgundy, 2, missingprob);

#__________________________________________________________________________________

check <- cleanBurgundy[-c(1,2,3,4,5,6)];
View(Burgundy);
cor(check); 

#Treating missing values of BD with median

boxplot(Burgundy$BD)

summary(Burgundy$BD)

median(Burgundy$BD);

medianBD <- median(Burgundy$BD, na.rm = TRUE);
medianBD;

Burgundy[is.na(Burgundy$BD), "BD"] <- medianBD

apply(Burgundy, 2, missingprob);

#_______________________________________________________________________________

#Treaing missing values in ACD 

boxplot(Burgundy$ACD)

summary(Burgundy$ACD)


#Treating Ouliers 
#because there are only 2 outliers i will treat the missing with median

median(Burgundy$ACD);

medianACD <- median(Burgundy$ACD, na.rm = TRUE);
medianACD;

Burgundy[is.na(Burgundy$ACD), "ACD"] <- medianACD

apply(Burgundy, 2, missingprob);

#____________________________________________________________________________

#Treaing missing values in TP

str(Burgundy)

Burgundy$TP <- as.integer(Burgundy$TP);
summary(Burgundy$TP);

boxplot(Burgundy$TP)

summary(Burgundy$TP)


#Treating Ouliers 
#because there are only 2 outliers i will treat the missing with median

MeanTP <- mean(Burgundy$TP, na.rm = TRUE);
MeanTP;

MeanTP <- as.integer(MeanTP);
MeanTP

Burgundy[is.na(Burgundy$TP), "TP"] <- MeanTP

Burgundy$TP <- factor(Burgundy$TP,
                      levels = 1:11,
                      labels = c("Ribera Del Duero Red", "Rioja Red", "Priorat Red", 
                                 "Red", "Toro Red", "Tempranillo", "Sherry", "Rioja White",
                                 "Grenache", "Pedro Ximenez", "Other")); 

str(Burgundy)
summary(Burgundy$TP)
levels(Burgundy$TP)

apply(Burgundy, 2, missingprob);

str(Burgundy)

#_____________________________________________________________________________________

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

View(Year_PR)


#____________________________________________________________________________________

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

# After doing the cluster we found that the number 59 repeated a total 
#of 76 times in the more expensive class. More than any other number 
# 59 is the 59th level in the factor for Region which is Ribera del Duero.
# We can conclude that Ribera del Duero is the region that produces the most 
# expensive wine 

#_______________________________________________________________________________________

#confidence Intervals of NUMR 

confINT <- Burgundy
mean(confINT$NUMR);

sample_Numr <- sample(confINT$NUMR, size = 200)
sample_mean <- mean(sample_Numr);

z_critical <- qnorm(0.975);

print("z-critical value:")
print(z_critical)

NUMR_SD <- sd(confINT$NUMR);

margins <- z_critical * (NUMR_SD / sqrt(200)); 

confidence_intervals <- c(sample_mean - margins,
                          sample_mean + margins);

print("confidence interval:")
print(confidence_intervals  );

# Using confidence interval calculation. By taking a sample of 200 We can determin with a 95% certanty 
#that the number of wine testers will be from the range of 421 - 707 who will 
# show up to test the wine. And after seeing the average wine testers which is 576 
# the confidence interval does fall confirm. 

#_____________________________________________________________________________________________________________________

#confidence interval of TP

WineVariety <- Burgundy

str(WineVariety$TP)
levels(WineVariety$TP);

WineVariety$TP <- as.integer(WineVariety$TP);
View(WineVariety);

mean(WineVariety$TP);


sample_TP <- sample(WineVariety$TP, size = 200);

sample_mean <- mean(sample_TP);

z_critical <- qnorm(0.975);

print("z-critical value:")
print(z_critical)

TP_SD <- sd(WineVariety$TP);

margins <- z_critical * (TP_SD / sqrt(200)); 

confidence_intervals <- c(sample_mean - margins,
                          sample_mean + margins);

print("confidence interval:")
print(confidence_intervals  );

#_____________________________________________________________________________________________________________

#cluster Analysis 
#Finding the quality of wine according to its Price, Rating and Residual sugar level.

Quality2 <- kmeans(Burgundy[,c(7,9,12)], 4); # 4 clusters*
Quality2;
summary(Burgundy);
Burgundy$WineQuality2 <- Quality2$cluster;
Burgundy$WineQuality2[Burgundy$WineQuality2==1] <- "High Quality";
Burgundy$WineQuality2[Burgundy$WineQuality2==2] <- "Good Quality";
Burgundy$WineQuality2[Burgundy$WineQuality2==3] <- "Average Quality";
Burgundy$WineQuality2[Burgundy$WineQuality2==4] <- "Bad Quality";


View(Burgundy);

library(plotly)
plot_ly(x=Burgundy$RT, y=Burgundy$PR, 
        z=Burgundy$RSG, type="scatter3d", mode="markers", 
        color=fit$cluster * 2 + 3);

#INSIGHTS FROM CLUSTER ANALYSIS

#We rated the wines according to there prices and assigned them to four categories (High, good,average and bad quality) , 
# in cluster analysis1 where the lower the price the worse the rating and the higher the price the better the rating, but..
#when we added a 3rd variable "RSG" we find that the cheaper wines where classified as average, and surprisingly a good amount of
#wines rated as bad quality fell in the middle to expensive range. 


#_____________________________________________________________________________________________________________________________________________________________________________

#Descriptive analysis
#1. What is the average price of the Wines according to their Residual Sugar Levels.
PRbyRSG <- aggregate(PR~RSG , data = Burgundy, FUN = mean, na.rm=T); 
PRbyRSG;

cor(Burgundy$RSG, Burgundy$PR);
#We can see from the analysis that the higher Price of the wine the lower its RSG 
#(expensive wine is less sweet than cheaper wine)


#2. What is the average Residual Sugar Level of the Wines according to the Average rating given to the wine.
averageRSG <- aggregate(RSG~RT , data = Burgundy, FUN = mean, na.rm=T); 
averageRSG;
cor(Burgundy$RSG, Burgundy$RT);
#We can see from the analysis that the higher RSG the lower the rating (sweeter wine is rated less)


#3. What is the Max, Min Alcohol Level of the Wines according to their Price.

maxAL <-aggregate(AL~RT , data = Burgundy, FUN = max, na.rm=T); 
maxAL;
minAL <- aggregate(AL~RT , data = Burgundy, FUN = min, na.rm=T); 
minAL;

#We can see from the analysis that higher rated win tend to contain more alcohol


#4. What is the average Alcohol Level of the Wines according to their Price.

ALbyPR  <- aggregate(AL~PR , data = Burgundy, FUN = mean, na.rm=T); 
ALbyPR;

#We can see from the analysis that there is very little correlation between a wine's alcohol and its price

#5. What is the Max,Min Price of Wine according to the Average rating given to the wine.

maxPR <-aggregate(PR~RT , data = Burgundy, FUN = max, na.rm=T); 
maxPR;
minPR <- aggregate(PR~RT , data = Burgundy, FUN = min, na.rm=T); 
minPR;

#This analysis shows that correlation between the price of a win and its rating is not definitive and need further analysis










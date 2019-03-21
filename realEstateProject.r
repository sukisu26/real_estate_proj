
# input house data
getwd()
rm(list=ls())
houseData <- read.csv('top8Data.csv', as.is=TRUE, header=TRUE)
houseData

#########################Listwise Deletion##########################################
DataListwiseDelete<-na.omit(houseData)
DataListwiseDelete$Suite<-as.factor(DataListwiseDelete$Suite)
DataListwiseDelete$Renovated<-as.factor(DataListwiseDelete$Renovated)

# Now no missing value in the new dataset CONSTRUSTION LINEAR REGRESSION MODEL
fitSellPrice<-lm(SellPrice~DaysOnMarket+Age+HouseSize+LotSize+Bedrooms+Renovated+Suite, data=DataListwiseDelete)
summary(fitSellPrice) 
newPrice<-data.frame(DaysOnMarket=14,Age=20,HouseSize=3000,LotSize=7200,Bedrooms=6,Bathrooms=4,Renovated=factor(0),Suite=factor(0))
predictedPrice<-round(predict(fitSellPrice,newPrice))
predictedPrice
predictInterval<-predict(fitSellPrice,newPrice,interval="predict")
predictInterval
#s is the standard deviation of the residuals
sqrt(deviance(fitSellPrice)/df.residual(fitSellPrice))
par(mfrow=c(2,2))
plot(fitSellPrice)


##########################Imputation methods########################################
houseData
DataImputation<-houseData
DataImputation

# Because Renovated and Suite are binary varaibles, we can't not use mean and regression
# Therefore, we treat Binary variables as factors
DataImputation$Suite<-as.factor(DataImputation$Suite)
DataImputation$Renovated<-as.factor(DataImputation$Renovated)

#guess binary missing value Suite and Renovated
DataImputation$Suite[is.na(DataImputation$Suite)] <- factor(0)
DataImputation$Renovated[is.na(DataImputation$Renovated)] <- factor(sample(0:1,4,replace=T))

#predict daysOnMarket--b/c all variables has a relationship with DaysOnMarket
daysMarketPredict<-lm(DaysOnMarket~SellPrice+Age+HouseSize+LotSize+Bedrooms+Bathrooms+Renovated+Suite, data=DataImputation)
summary(daysMarketPredict) 
newdaysMarket<-data.frame(SellPrice=655000,Age=6,HouseSize=4632,LotSize=9639,Bedrooms=8,Bathrooms=5,Renovated=factor(0),Suite=factor(0))
predictedmarket<-round(predict(daysMarketPredict,newdaysMarket))
predictedmarket
DataImputation$DaysOnMarket[is.na(DataImputation$DaysOnMarket)]<-predictedmarket

#check outliers
boxplot(DataImputation$Age, main="AGE") # one outlier on Age
boxplot(DataImputation$LotSize, main="LotSize") # one outlier on Lotsize

#predict Age--b/c age is independent with other independent variables, we use MEAN SUBSTITUTION
is.na(DataImputation$Age)
predictAge<- round(mean(DataImputation$Age, na.rm = TRUE))
predictAge
DataImputation$Age[is.na(DataImputation$Age)]<-predictAge
is.na(DataImputation$Age)

#predict LotSize--for LotSize we use MEAN SUBSTITUTION
is.na(DataImputation$LotSize)
predictLotSize<- round(mean(DataImputation$LotSize, na.rm = TRUE))
predictLotSize
DataImputation$LotSize[is.na(DataImputation$LotSize)]<-predictLotSize
is.na(DataImputation$LotSize)

# REGRESSION IMPUTATION--bathrooms =b0+b1bedrooms+b2houseSize+b3suite
fitBathroom<-lm(Bathrooms~Bedrooms+HouseSize+Suite,data=DataImputation, na.action=na.omit)
summary(fitBathroom)
newdata<-data.frame(Bedrooms = 5, HouseSize = 4427, Suite = factor(1))
predictBathroom<- round(predict(fitBathroom,newdata))
predictBathroom
DataImputation$Bathrooms[is.na(DataImputation$Bathrooms)]<- predictBathroom
is.na(DataImputation$Bathrooms)

DataImputation

# Now no missing value in the new dataset CONSTRUSTION LINEAR REGRESSION MODEL
fitSellPrice<-lm(SellPrice~DaysOnMarket+Age+HouseSize+LotSize+Bedrooms+Bathrooms+Renovated+Suite, data=DataImputation)
summary(fitSellPrice) 
newPrice<-data.frame(DaysOnMarket=14,Age=20,HouseSize=3000,LotSize=7200,Bedrooms=6,Bathrooms=4,Renovated=factor(0),Suite=factor(0))
predictedPrice<-round(predict(fitSellPrice,newPrice))
predictedPrice
predictInterval<-predict(fitSellPrice,newPrice,interval="predict")
predictInterval

par(mfrow=c(2,2))
plot(fitSellPrice)

#check correlation From the correlation, Bathrooms and HouseSize are strong correlated.
#having a fair amount of missing data and you would be looking for a sensible multiple imputation strategy 
#to fill in the spaces
cor(as.matrix(DataImputation[,c(1,2,3,4,5,6,7)]), use="pairwise.complete.obs")
#Based on the result, Bathrooms and HouseSize are strongly corrleted, so ignore bathrooms in regression


# do regression again with bathroom
fitSellPrice2<-lm(SellPrice~DaysOnMarket+Age+HouseSize+LotSize+Bedrooms+Renovated+Suite, data=DataImputation)
summary(fitSellPrice2) 
newPrice2<-data.frame(DaysOnMarket=14,Age=20,HouseSize=3000,LotSize=7200,Bedrooms=6,Renovated=factor(0),Suite=factor(0))
predictedPrice2<-round(predict(fitSellPrice2,newPrice2))
predictedPrice2
predictInterval2<-predict(fitSellPrice2,newPrice2,interval="predict")
predictInterval2

par(mfrow=c(2,2))
plot(fitSellPrice2)




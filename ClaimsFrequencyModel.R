#### Importing required packages ####
#setwd("D:/HomeProtect/ModellingProject")
#install.packages("insuranceData")
#install.packages("insuranceData_1.0.tar.gz", repos = NULL, type = "source")
library(insuranceData)
library(ggplot2)
library(dplyr)
library(ParBayesianOptimization)
#library(doParallel)
library(caret)
library(xgboost)
#library(dummies)
library(DiagrammeR)

#### Data Loading and Analysis ####
data(dataCar)
summary(dataCar)
# 378 duplicate records removed
dataCar = unique(dataCar)
100*prop.table(table(dataCar$numclaims))
#plot(100*prop.table(table(dataCar$numclaims)), type = "h")
numClaimsFreq = as.data.frame(table(dataCar$numclaims), stringsAsFactors = F) 
numClaimsFreq$Var1 = as.numeric(numClaimsFreq$Var1)
numClaimsFreq["Percentage"] = as.numeric(100*prop.table(table(dataCar$numclaims)))
print(paste("Number of policies with claims:", 
            sum(numClaimsFreq[which(numClaimsFreq$Var1>0),"Freq"]), sep = " "))
# Plot - Distribution of Claim Counts 
ggplot(numClaimsFreq, aes(Var1,Percentage)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_minimal()+
  geom_text(aes(label = Freq), nudge_y = 3)+
  ggtitle("Distribution of Claim Counts")+
  labs(x = "Number of Claims")+
  theme(plot.title = element_text(hjust = 0.5))

# Plot - Distribution of Driver Gender 
genderFreq = as.data.frame(table(dataCar$gender), stringsAsFactors = F) 
genderFreq["Percentage"] = 100*genderFreq$Freq/sum(genderFreq$Freq)
ggplot(genderFreq, aes(Var1,Percentage)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_minimal()+
  geom_text(aes(label = Freq), nudge_y = 3)+
  ggtitle("Distribution of Driver Gender")+
  labs(x = "Gender")+
  theme(plot.title = element_text(hjust = 0.5))

# Plot - Distribution of Vehicle Body Type
vehBodyFreq = as.data.frame(table(dataCar$veh_body), stringsAsFactors = F) 
vehBodyFreq["Percentage"] = 100*vehBodyFreq$Freq/sum(vehBodyFreq$Freq)
ggplot(vehBodyFreq, aes(Var1,Percentage)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_minimal()+
  geom_text(aes(label = Freq), nudge_y = 1.5)+
  ggtitle("Distribution of Policies by Vehicle Body")+
  labs(x = "Vehicle Body")+
  theme(plot.title = element_text(hjust = 0.5))

# Plot - Distribution of Policies by Area 
areaFreq = as.data.frame(table(dataCar$area), stringsAsFactors = F) 
areaFreq["Percentage"] = 100*areaFreq$Freq/sum(areaFreq$Freq)
ggplot(areaFreq, aes(Var1,Percentage)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_minimal()+
  geom_text(aes(label = Freq), nudge_y = 1.5)+
  ggtitle("Distribution of Policies by Area")+
  labs(x = "Area")+
  theme(plot.title = element_text(hjust = 0.5))

# Plot - Distribution of Policies by Driver Age Category 
ageCatFreq = as.data.frame(table(dataCar$agecat), stringsAsFactors = F) 
ageCatFreq["Percentage"] = 100*ageCatFreq$Freq/sum(ageCatFreq$Freq)
ggplot(ageCatFreq, aes(Var1,Percentage)) +
  geom_bar(stat = "identity", fill="steelblue")+
  theme_minimal()+
  geom_text(aes(label = Freq), nudge_y = 1.5)+
  ggtitle("Distribution of Policies by Driver Age Category")+
  labs(x = "Driver Age Category")+
  theme(plot.title = element_text(hjust = 0.5))

# Computing claim frequency, severity and loss cost 
dataCar["clmFreq"] = dataCar$numclaims/dataCar$exposure
dataCar["clmSev"] = dataCar$claimcst0/dataCar$numclaims
dataCar["lossCost"] = dataCar$claimcst0/dataCar$exposure

#Generate bins for Claimed Amounts
dataCar = dataCar %>% mutate(
  binCost = case_when(
    claimcst0 == 0 ~ "0",
    claimcst0 <= 500 ~ "0 - 500",
    claimcst0 <= 1000 ~ "500 - 1000",
    claimcst0 <= 2000 ~ "1000 - 2000",
    claimcst0 <= 5000 ~ "2000 - 5000",
    claimcst0 <= 10000 ~ "5000 - 10000",
    TRUE ~ "10k+" 
  )
)
dataCar$binCost <- factor(dataCar$binCost,
                          levels = c("0","0 - 500","500 - 1000","1000 - 2000","2000 - 5000","5000 - 10000","10k+"))
#Plot - Distribution of Claim Costs
ggplot(dataCar, aes(x = binCost)) + geom_histogram(stat = "count", fill="steelblue")+
  theme_minimal()+
  #geom_text(aes(label = Freq), nudge_y = 3)+
  geom_text(stat='count',aes(label = after_stat(count)), nudge_y = 3000)+
  ggtitle("Distribution of Claim Costs")+
  labs(x = "Claim Cost", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))

# Other Plots 
ggplot(dataCar, aes(x=veh_body, y=clmFreq)) +
  geom_boxplot(fill='#A4A4A4', color="black")+  theme_classic()

ggplot(dataCar, aes(x=veh_body, y=clmSev)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

ggplot(dataCar, aes(x=veh_body, y=lossCost)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

dataNonZero = dataCar[which(dataCar$claimcst0>0),]
ggplot(dataNonZero, aes(x=veh_body, y=lossCost)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

ggplot(dataCar, aes(x=area, y=lossCost)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

# Plot - Boxplot for Vehicle Value
ggplot(dataCar, aes(x=veh_value)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

# Plot - Boxplot for Exposure
ggplot(dataCar, aes(x=exposure)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()


#### Data Pre-processing Steps ####
dataCar$veh_body = as.character(dataCar$veh_body)
dataCar["vehBody"] = ifelse(dataCar$veh_body %in% c("BUS","CONVT","COUPE","MCARA",
                                                    "MIBUS","PANVN","RDSTR"),
                            "Others", dataCar$veh_body)

data = dataCar[which(dataCar$veh_value<30),]
data = data[which(dataCar$lossCost < 1900000),]
data = data[-which(is.na(data$clm)==TRUE),]

ggplot(data, aes(x=area, y=lossCost)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

# one-hot-encoding categorical features
oheFeatures = c('vehBody', 'gender', 'area')
dummies <- dummyVars(~ vehBody +  gender + area, data = data)
dataOHE <- as.data.frame(predict(dummies, newdata = data))
dataCombined <- cbind(data[,-c(which(colnames(data) %in% oheFeatures))],dataOHE)
#### Train Test Split ####
set.seed(123)
#data$clm = as.factor((data$clm))
trainIndex <- createDataPartition(dataCombined$clm, p = .8,list = FALSE,times = 1)
trainData <- dataCombined[trainIndex,]
testData <- dataCombined[-trainIndex,]

remFeatures = c("veh_body", "X_OBSTAT_","binCost","vehBodyOthers",
                "gender.M","area.F")
claimVars = c("exposure","clm","numclaims","claimcst0",
              "clmFreq","clmSev","lossCost")
trainX = data.matrix(trainData[, -c(which(colnames(dataCombined) %in%
                                            c(remFeatures,claimVars)))])
testX = data.matrix(testData[, -c(which(colnames(dataCombined) %in%
                                          c(remFeatures,claimVars)))])

xgbTrain = xgb.DMatrix(data = trainX, label = trainData$numclaims)
setinfo(xgbTrain, "base_margin", log(trainData$exposure))
xgbTest = xgb.DMatrix(data = testX, label = testData$numclaims)
setinfo(xgbTest, "base_margin", log(testData$exposure))

#### Bayesian Optimization of XGBoost Poisson Regression Tree ####
# Scoring function for Bayesian Optimization
scoringFunction <- function(max_depth, colsample_bytree, subsample,eta) 
{
  xgbcv <- xgb.cv(params = list(
    booster          = "gbtree",
    eta              = eta,
    max_depth        = max_depth,
    subsample        = subsample,
    colsample_bytree = colsample_bytree,
    objective        = 'count:poisson', 
    eval_metric     = "poisson-nloglik"),
    data = xgbTrain, 
    nround = 5000,
    nthread = 4,
    nfold =  5,
    prediction = FALSE,
    showsd = TRUE, maximize = FALSE,
    early_stopping_rounds = 25, ## If evaluation metric does not improve on out-of-fold sample for 25 rounds, stop
    verbose = 1,
    print_every_n = 100)  
  return(
    list( 
      Score = -1*min(xgbcv$evaluation_log[[4]]), 
      nrounds = xgbcv$best_iteration
    )
  )
}

# Bounds of search space - Hyper parameters
bounds <- list(max_depth = c(3L, 10L), 
               colsample_bytree = c(0.4, 0.9), 
               subsample = c(0.4, 0.9),
               eta = c(0.01,0.2))
set.seed(1234)
# Running 100 iterations of search
optObj <- bayesOpt(FUN = scoringFunction, 
                   bounds = bounds, 
                   initPoints = 5, 
                   iters.n = 100)

# Printing results
optObj$scoreSummary
plot(optObj)
getBestPars(optObj)
bestIter = which(optObj$scoreSummary$Score==max(optObj$scoreSummary$Score))
bayesResults = optObj
optObj$scoreSummary$eta[bestIter]
optObj$scoreSummary$max_depth[bestIter]
optObj$scoreSummary$subsample[bestIter]
optObj$scoreSummary$colsample_bytree[bestIter]


#### Preparation of best Model ####
# Final CV to find optimal number of rounds
xgbcvRounds <- xgb.cv(params = list(
  booster          = "gbtree",
  eta              = optObj$scoreSummary$eta[bestIter],
  max_depth        = optObj$scoreSummary$max_depth[bestIter],
  subsample        = optObj$scoreSummary$subsample[bestIter],
  colsample_bytree = optObj$scoreSummary$colsample_bytree[bestIter],
  objective        = 'count:poisson', 
  eval_metric     = "poisson-nloglik"),
  data = xgbTrain, 
  nthread = 4,
  nround = 5000, 
  nfold =  5,
  prediction = FALSE,
  showsd = TRUE, maximize = FALSE,
  early_stopping_rounds = 25, ## If evaluation metric does not improve on out-of-fold sample for 25 rounds, stop
  verbose = 1,
  print_every_n = 500)
# Final model
xgbFinalModel <- xgb.train(
  params =  list(
    booster          = "gbtree",
    eta              = optObj$scoreSummary$eta[bestIter],
    max_depth        = optObj$scoreSummary$max_depth[bestIter],
    subsample        = optObj$scoreSummary$subsample[bestIter],
    colsample_bytree = optObj$scoreSummary$colsample_bytree[bestIter],
    objective        = 'count:poisson', 
    eval_metric     = "poisson-nloglik"),
  data = xgbTrain,
  nrounds = 1059,
  verbose = 1
) 
xgbFinalModel

importanceMatrix <- xgb.importance(
  feature_names = colnames(xgbTrain), 
  model = xgbFinalModel
)
xgb.plot.importance(importanceMatrix)
# plot the first tree
xgb.plot.tree(model = xgbFinalModel, trees = 1)
xgb.plot.tree(model = xgbFinalModel, trees = 2)


#### Evaluation on Test Data ####
#use model to make predictions on test data
predY = predict(xgbFinalModel, xgbTest)
#mean((log(testData$numclaims) - log(predY))^2)
#mean(((testData$numclaims - predY)^2)/predY) 

MAPE = function(yActual,yPredict){
  mean(abs((yActual-yPredict)/yActual))*100
}
MAE = function(yActual,yPredict){
  mean(abs(yActual-yPredict))
}
#R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(yActual,yPredict){
  cor(yActual,yPredict)^2
}
mean((testData$numclaims - predY)^2) #mse - Mean Squared Error
RSQUARE(testData$numclaims,predY)
MAE(testData$numclaims,predY) #Mean Absolute Error
MAPE(testData$numclaims,predY) #Mean Abs Percentage Error
ModelMetrics::msle(testData$numclaims,predY) #Mean square log error
ModelMetrics::rmse(testData$numclaims,predY)

#test = data.frame(pred = predict(xgbFinalModel, xgbTest), numClaims = testData$numclaims,
#                  freq = testData$clmFreq, predExp = predict(xgbFinalModel, xgbTest)*testData$exposure)

#train = data.frame(pred = predict(xgbFinalModel, xgbTrain), numClaims = trainData$numclaims,
#                   freq = trainData$clmFreq, predExp = predict(xgbFinalModel, xgbTrain)*trainData$exposure)

hist(predY, main = "Histogram of Predictions",
     xlab = "Predicted Means")

hist(test$numClaims-predY, main = "Histogram of Residuals",
     xlab = "Residuals")
plot(density(as.numeric(test$numClaims-predY)), xlab="Residuals", ylab="",main="Density plot for Residuals")

rm(dataOHE,dummies, numClaimsFreq)

save.image("freqModelImage.RData")
#load("freqModelImage.RData")

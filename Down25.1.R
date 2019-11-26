library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(caTools)
library(ggplot2)
library(labeling)
library(farver)
library(rmarkdown)
library(C50)
library(inum)

#datasets####
complete<- read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task2/data/CompleteResponses.csv")
incomplete<- read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task2/data/SurveyIncomplete.csv")

#exploration####
summary(complete)
is.na(complete)
str(complete)

summary(incomplete)
str(incomplete)



#Data type conversion

complete$elevel <- as.factor(complete$elevel)
complete$car <- as.factor(complete$car)
complete$zipcode <- as.factor(complete$zipcode)
complete$brand <- as.factor(complete$brand)
incomplete$elevel <- as.factor(incomplete$elevel)
incomplete$car <- as.factor(incomplete$car)
incomplete$zipcode <- as.factor(incomplete$zipcode)
incomplete$brand <- as.factor(incomplete$brand)

str(complete)
str(incomplete)



#Class imbalance####
brands <- table(complete$brand)
brand0 <- length(complete$brand[complete$brand == 0]) / length(complete$brand) * 100
brand1 <- length(complete$brand[complete$brand == 1]) / length(complete$brand) * 100

brand0
brand1



ggplot(complete,aes(brand, fill=brand)) +
  geom_bar(col='black') +
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony')) +
  labs(title="Brand distributiion", subtitle = 'Class imbalance') + 
  scale_x_discrete(labels=c("0" = 'Acer', '1'= 'Sony')) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.., group=1 ), stat= "count", vjust = -.5) 




#####Start model ####

# Treat class imbalance

#We have 62-38 (aprox.) brand election data. If we want to correct this factor to 50-50 for a better prediction, we have to modify our sample

brand0count<-subset(complete, complete$brand==0)
brand1count<-subset(complete, complete$brand==1)

branddif<-nrow(brand1count)-nrow(brand0count)
branddif
brand1_down<-brand1count[-sample(nrow(brand1count), branddif), ]
newcomplete<-rbind(brand1_down, brand0count)
complete_down<-newcomplete[sample(nrow(newcomplete)),] #we shuffle the rows, because the dataset merge gave that "in order"
complete_down$brand


ggplot(complete_down,aes(brand, fill=brand)) +
  geom_bar(col='black') +
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony')) +
  labs(title="Brand distributiion", subtitle = 'Class balanced wiht down sampling') + 
  scale_x_discrete(labels=c("0" = 'Acer', '1'= 'Sony')) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.., group=1 ), stat= "count", vjust = -.5) 



#Data splitting

set.seed(998)
#create a 20% sample of the data HOLD OUT
complete_downH <- complete_down[sample(1:nrow(complete_down), 5990,replace=FALSE),]
complete_downH
holdo_down <- complete_down[-sample(1:nrow(complete_down), 5990,replace=FALSE),]
holdo_down

# define an 75%/25% train/test split of the dataset
inTrainingdown <- createDataPartition(complete_downH$brand, p = .75, list = FALSE)
training_down <- complete_downH[inTrainingdown,]
testing_down <- complete_downH[-inTrainingdown,]


# Set up control function for training

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 1)

#original####
#train random forest model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
down_mod <- train(brand~., 
              data = training_down, 
              method = "rf", 
              trControl=fitControl, 
              tuneLength = 2)

# Explote variable importance

variableimportance_down <- varImp(down_mod)
plot(variableimportance_down)


# Choose variables and train model
trainingvarimp_down <- training_down[,c(1,2,7)]


# Random Forest with down Sample
down_mod_rf <- train(brand ~ .,
                  data = trainingvarimp_down,
                  method='rf',
                  trControl = fitControl,
                  tuneLength=2)


# orig_fit is my model with which i will predict

# Test prediction
predictions_TDo_rf <- predict(down_mod_rf,testing_down)
predictions_TDo_rf

accuracy_TDo_rf <- postResample(testing_down$brand,predictions_TDo_rf)
accuracy_TDo_rf

# holdout predictions 

predictions_HDo_rf <- predict(down_mod_rf,holdo_down)
predictions_HDo_rf

accuracy_HDo_rf <- postResample(holdo_down$brand,predictions_HDo_rf)
accuracy_HDo_rf

# C5.0 with down Sample

down_mod_c50 <- train(brand ~ .,
                   data = trainingvarimp_down,
                   method='C5.0Tree',
                   trControl = fitControl,
                   tuneLength=2)


# orig_fit is my model with which i will predict

# Test prediction
predictions_TDo_c50 <- predict(down_mod_c50,testing_down)
predictions_TDo_c50

accuracy_TDo_c50 <- postResample(testing_down$brand,predictions_TDo_c50)
accuracy_TDo_c50

# holdout predictions 

predictions_HDo_c50 <- predict(down_mod_c50,holdo_down)
predictions_HDo_c50

accuracy_HDo_c50 <- postResample(holdo_down$brand,predictions_HDo_c50)
accuracy_HDo_c50


#LOAD THE INCOMPLETE DATASET####
#predictions random forest
# incompletedatasetpredictions <- predict(down_fit2,NAMEOFTHEINCOMPLETEDATASET)

predictionsincomplete_rf <- predict(down_mod_rf,incomplete)
predictionsincomplete_rf

incomplete$brand_rf <- predictionsincomplete_rf

str(predictionsincomplete_rf)

ggplot(incomplete,aes(brand_rf, fill=brand_rf)) +
  geom_bar(col='black') +
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony')) +
  labs(title="Brand distributiion", subtitle = 'Predicted class distribution, Random Forest') + 
  scale_x_discrete(labels=c("0" = 'Acer', '1'= 'Sony')) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.., group=1 ), stat= "count", vjust = -.5) 


#predictions C5.0
predictionsincomplete_c50 <- predict(down_mod_c50,incomplete)
predictionsincomplete_c50

incomplete$brands_c50 <- predictionsincomplete_c50


str(incomplete)

ggplot(incomplete,aes(brands_c50, fill=brands_c50)) +
  geom_bar(col='black') +
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony')) +
  labs(title="Brand distributiion", subtitle = 'Predicted class distribution, C 5.0') + 
  scale_x_discrete(labels=c("0" = 'Acer', '1'= 'Sony')) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.., group=1 ), stat= "count", vjust = -.5) 



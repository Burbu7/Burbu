library("caret")
library("lattice")
set.seed(998)
WholeYear<-read.csv("C:/Users/Burbu/Downloads/WholeYear.csv")
WholeYear
WholeYear<-WholeYear[sample(1:nrow(WholeYear), 7000, replace=FALSE),]
inTraining<-createDataPartition(WholeYear$SolarRad, p=.75, list = FALSE)
training<-WholeYear[inTraining, ]
testing<-WholeYear[-inTraining, ]
fitControl<-trainControl(method = "repeatedcv", number = 10, repeats = 1)
fitControl
rfFit1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
rfFit1
rfFit2 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)
rfFit2
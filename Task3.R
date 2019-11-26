#install and call caret package and import our data set csv files
library("caret")
CR<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task2/data/CompleteResponses.csv")
IS<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task2/data/SurveyIncomplete.csv")

#analize and preprocess our data
str(CR)
str(IS)
getOption("max.print")
options(max.print=1000000) #I saw and increased the number of prints to detect missing values
is.na(CR) #there isn't missing values in our data set
is.na(IS) #We could see that in brand column all the values are 0 (but aren't missing), because is the data that we're gonna predict. 
sum(is.na(CR))
sum(is.na(IS))

#as I saw in the structure, I need to change some integral variables (numerical) to factors (categorical)
CR$elevel<-as.factor(CR$elevel)
CR$car<-as.factor(CR$car)
CR$zipcode<-as.factor(CR$zipcode)
CR$brand<-as.factor(CR$brand)
str(CR)
IS$elevel<-as.factor(IS$elevel)
IS$car<-as.factor(IS$car)
IS$zipcode<-as.factor(IS$zipcode)
IS$brand<-as.factor(IS$brand)
str(IS)

#summary

#Now, I want a bar plot about Acer and Sony frequencies 
#CR$brand=ifelse(CR)
ggplot(data = CR, mapping = aes(x = brand,fill=brand))+geom_bar()+
  geom_text(stat="count",aes(label=..count..,y=..count..), vjust=10)


#In next step, we will see the distribution of our variables in a graphic way
#If variables have similar distribution in both data sets and we don't observe outliers, we can extrapolate our predictive model to Incomplete Survey data set

#We have 62-38 (aprox.) brand election data. If we want to correct this factor to 50-50 for a better prediction, we have to modify our sample 
CRT<-subset(CR, CR$brand==1)
CRF<-subset(CR, CR$brand==0)
CRdif<-nrow(CRT)-nrow(CRF)
CorrectedCRT<-CRT[-sample(nrow(CRT), CRdif), ]
CRcorrected<-rbind(CorrectedCRT, CRF)
NewCR<-CRcorrected[sample(nrow(CRcorrected)),] #we shuffle the rows, because the dataset merge gave that "in order"
NewCR$brand

CRcorrected$brand

boxplot(CR$salary, IS$salary, main = "Salary comparation boxplot", ylab = "Salary($)", 
        names = c("Complete", "Incomplete"), col=c("lightblue","lightgreen"))
#legend("bottom", inset=.02, col=c("lightblue", "lightgreen"),legend = c("Complete Responses", "Incomplete Survey"), fill=topo.colors(3), horiz=TRUE, cex=0.8)

boxplot(CR$age, IS$age, main = "Age comparation boxplot", ylab = "age", 
        names = c("Complete", "Incomplete"), col=c("lightblue","lightgreen"))
boxplot(CR$credit, IS$credit, main = "Credit comparation boxplot", ylab = "credit($)", 
        names = c("Complete", "Incomplete"), col=c("lightblue","lightgreen"), digits=-4)


boxplot(NewCR$salary, IS$salary, main = "Salary comparation boxplot (down sampling)", ylab = "Salary($)", 
        names = c("Complete", "Incomplete"), col=c("lightblue","lightgreen"))
#legend("bottom", inset=.02, col=c("lightblue", "lightgreen"),legend = c("Complete Responses", "Incomplete Survey"), fill=topo.colors(3), horiz=TRUE, cex=0.8)

boxplot(NewCR$age, IS$age, main = "Age comparation boxplot (down sampling)", ylab = "age", 
        names = c("Complete", "Incomplete"), col=c("lightblue","lightgreen"))
boxplot(NewCR$credit, IS$credit, main = "Credit comparation boxplot (down sampling)", ylab = "credit($)", 
        names = c("Complete", "Incomplete"), col=c("lightblue","lightgreen"), digits=-4)

ggplot(data=NewCR, aes (NewCR$salary))+
  geom_histogram(color="darkblue", fill="lightblue", bins=20, xlab = "Salary")
ggplot(data=IS, aes (IS$salary))+
  geom_histogram(color="darkblue", fill="lightblue", bins=20)

ggplot(CR, aes(x=age, fill=brand)) + 
  geom_histogram(color="black", bins=10) +
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony')) + 
  labs(title="Relationship between brand and age")+
  scale_x_continuous(breaks=seq(20000, 150000, 15000))+
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  scale_y_continuous(labels = scales::percent)

ggplot(CR, aes(x=salary, fill=brand)) + geom_histogram(color="black", bins=20)+
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony'))

ggplot(CR, aes(x=age, fill=brand)) + geom_histogram(color="black", bins=20)+
  scale_fill_discrete("Legend", labels=c('0'= 'Acer', '1'= 'Sony'))

#histogram age
#acersony brand plot
#car, zipcode, elevel->factor
#change brand

#correlation matrix to undersantd the colineality (mirar al ser un factor binario)
#attributes and attributes
#chi squared. 
#check continus variable and categorical, logistic regresion or ANOVA
#varimp



#We need to do the features selection (which variables will predict the brand election)

set.seed(998)

CR<-CR[sample(1:nrow(WholeYear), 7000, replace=FALSE),]
inTraining<-createDataPartition(CR$brand, p=.75, list = FALSE)
training<-CR[inTraining, ]
testing<-CR[-inTraining, ]
fitControl<-trainControl(method = "repeatedcv", number = 10, repeats = 1)
fitControl
rfFit1<-train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
rfFit1


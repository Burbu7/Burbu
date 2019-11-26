library("caret")
SurveyIncomplete<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task2/data/SurveyIncomplete.csv")
CompleteResponses<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task2/data/CompleteResponses.csv")
x<-CompleteResponses$salary
y<-SurveyIncomplete$salary
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
ggplot(CompleteResponses, aes(x=brand, fill=brand)) + geom_bar()
ggplot(CompleteResponses, aes(x=CompleteResponses$salary, fill=brand))+
  geom_histogram(color = "darkblue", bins=20)
#boxplot(x, y, main = "Salary's comparation boxplot", ylab = "Salary($)", names = c("Complete", "Incomplete"))
ggplot(CompleteResponses, aes(x=salary)) + geom_histogram(color="darkblue", fill="lightblue", bins=20 ) + facet_wrap(~zipcode, scales = "free_x")
ggplot(CompleteResponses, aes(x=age, y=salary, col=brand)) + 
  geom_point() +
  geom_smooth(method="lm") 
ggplot(CR, aes(x=brand, y=salary, fill=brand)) + 
  geom_boxplot()+
  stat_summary(fun.y=median, colour="black", geom="text", 
               vjust=-0.7, aes(label=round(..y.., digits=1)))
ggplot(CompleteResponses, aes(x="",fill=brand)) + geom_bar() + coord_polar(theta="y")


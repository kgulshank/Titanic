#Titanic prediction

# Reading the trainset and test set
titanic.train<-read.csv("train.csv",stringsAsFactors = FALSE)
str(titanic.train)

titanic.test<-read.csv("test.csv",stringsAsFactors = FALSE)

#Combining the test and train test 
titanic.train$Istrainset<-TRUE
titanic.test$Istrainset<-FALSE

#Adding the survives columnn so that we can merge the two sets
titanic.test$Survived<-NA
titanic.full<-rbind(titanic.test,titanic.train)

#checking for null values in Embarked col, and rectifying it
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

#Checking for the NA in Age 
table(is.na(titanic.full$Age))
age.median<-median(titanic.full$Age,na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median
install.packages("randomForest")
# Checking for outliers and filtering it

boxplot(titanic.full$Fare)
upper.whisker<-boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter<-titanic.full$Fare<upper.whisker

#Modeling
fare.equation=" Fare ~ Pclass + Sex  + Age + SibSp + Parch + Embarked "
fare.model<-lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)
#replacing the missing value using model
fare.row<-titanic.full[is.na(titanic.full$Fare),c("Pclass","Sex","Age","SibSp","Parch","Embarked")]
fare.predictions<-predict(fare.model,newdata = fare.row)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.predictions

#categorical casting
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#Splitting the data
titanic.train<-titanic.full[titanic.full$Istrainset==TRUE,]
titanic.test<-titanic.full[titanic.full$Istrainset==FALSE,]

titanic.train$Survived<-as.factor(titanic.train$Survived)
#modeling using randomForest
library(randomForest)
survived.equation="Survived ~ Fare + Pclass + Sex  + Age + SibSp + Parch + Embarked "
survived.formula<-as.formula(survived.equation)
titanic.model<-randomForest(formula=survived.formula,data=titanic.train,ntree = 500, mtry = 3,nodesize = 0.01*nrow(titanic.test))
features.equation<-"Fare + Pclass + Sex  + Age + SibSp + Parch + Embarked"
Survived<-predict(titanic.model,newdata = titanic.test)
PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived
output.df
write.csv(output.df,"final_survive.csv",row.names = FALSE)


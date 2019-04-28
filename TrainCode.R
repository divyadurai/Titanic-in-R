#Import the data(Train and test)
setwd('D:\\R\\Titanic')
train <- read.csv('Titanic train.csv',na.strings = c(""))
test <- read.csv('Titanic test.csv',na.strings = c(""))

#Audit the data
str(train)

#To check for skewness
install.packages('e1071')
library('e1071')
skewness(train$Fare)
skewness(train$Age)

#to Build boxplot 
boxplot(train$Age)
boxplot(train$Fare)

describe(train)


#Convert to factor
train$Survived <- as.factor(train$Survived)
train$Pclass - as.factor(train$Pclass)
str(train)

# Missing values for age and replacing with 
#median na.rm= TRUE denotes that we are ignore all missing values while calculating median

train$Age <- ifelse(is.na(train$Age),median(train$Age,na.rm = TRUE),train$Age)

# To calculate the count of missing values
sum(is.na(train$Age))

sum(is.na(train$Embarked))
#factor to character This step is done as its an bug in R
train$Embarked <- as.character(train$Embarked)
# Imputation as in converting the missing values to another value
train$Embarked <- ifelse(is.na(train$Embarked),'S',train$Embarked)
#Again changing to factor
train$Embarked <- as.factor(train$Embarked)
sum(is.na(train$Embarked))
str(train)

cor(train$Age,train$Fare)
cor(train$SibSp,train$Parch)

names(train)

model <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, family = 'binomial', data = train)
summary(model)
model

train$preds <- predict(model,train,type = 'response')
View(train)
train$outcome <- ifelse(train$preds>=0.5,1,0)

table(train$Survived,train$outcome)

#another model in order to refine it
train$ln_fare <- log(train$Fare)
View(train)
train$ln_fare <- ifelse(train$ln_fare==-Inf,0,train$ln_fare)
model2 <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+ln_fare+Embarked, family = 'binomial', data = train)
train$preds_model2 <- predict(model2,train,type = 'response')
train$outcome_model2 <- ifelse(train$preds_model2>=0.5,1,0)
table(train$Survived,train$outcome_model2)


###test data

View(test)
str(test)

test$Pclass <- as.factor(test$Pclass)
str(test)
test$Age <- ifelse(is.na(test$Age),median(test$Age,na.rm = TRUE),test$Age)
sum(is.na(test$Age))
sum(is.na(test$Embarked))    
sum(is.na(test$Fare))
test$Fare <- ifelse(is.na(test$Fare),median(test$Fare,na.rm = TRUE),test$Fare)
sum(is.na(test$Fare))
summary(test)

test$preds <- predict(model,test,type = 'response')
test$outcome <- ifelse(test$preds>=0.5,1,0)
View(test)

write.csv(test,'finalpreds.csv')
getwd()
###------SVM Model --------#####
install.packages('e1071')
library(e1071)
model_svm <- svm(Survived~Pclass+Sex+Age+SibSp+Parch+ln_fare+Embarked, data = train)

model_svm
train$pred_svm <- predict(model_svm,train)
train_1 <-subset(train,select = -c(PassengerId,Name,Ticket,Cabin))
train_1$pred_svm <- predict(model_svm,train_1)




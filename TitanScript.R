train <- read.csv("F:/Francis/R-Studio/Titanic/train.csv", stringsAsFactors = F) 
test<- read.csv("F:/Francis/R-Studio/Titanic/test.csv", stringsAsFactors = F)
full  <- dplyr::bind_rows(train, test) #binding both datasets
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
View(train)
prop.table(table(train$Survived)) #Gives percentage of survivors and non survivors
table(train$Survived) #gives no of survivors and non survivors
table(train$Sex,train$Survived) #no of male and female survivors and non survivors
prop.table(table(train$Sex,train$Survived))

train$child<-0 #Creating a new variable to find number of children
train$child[train$Age<18]<-1 #Assigning value 1 to represent children
aggregate(Survived ~ child + Sex, data=train, FUN=sum) #gives aggeregate for survival of children
aggregate(Survived ~ child + Sex, data=train, FUN=length)#Gives survival aggregate for children

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1 
# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

#Graphical Reprsentation of survivors wrt family size
ggplot2::ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

#Fare Variables
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#Decision Trees
my_tree<-rpart(Survived ~ Sex + Age, data = train,method = "class")
library(rpart.plot)
library(RColorBrewer)
new.fit <- prp(my_tree,snip=FALSE)$obj

#2nd decision tree
fit <- rpart(Survived ~ Sex + Age,data=train,method="class")
plot(fit)
text(fit)
fancyRpartPlot(fit)
prp(fit)

fit2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
prp(fit2)

data("segmentationData")
data <- segmentationData[,-c(1,2)]
form <- as.formula(Class ~ .)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))
prp(tree.1)

tree.2 <- rpart(form,data)
prp(tree.2)

binary.model <- rpart(Survived~Sex + Age, data=train, method="class",cp=.02)
rpart.plot(binary.model)
#Decision Tree
rpart.plot(fit, # middle graph
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)
rpart.plot(fit2,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray",tweak = 1.3, nn=TRUE)
fit3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit = 2,cp=0.5))
new.fit <- prp(fit3,snip=FALSE)$obj

#Feature Engeenering
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

#Prediction of age null values
sum(is.na(full$Age))
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Fsize,
                data=full[!is.na(full$Age),], 
                method="anova")
full$Age[is.na(full$Age)] <- predict(Agefit, full[is.na(full$Age),])

#After Prediction Of age:
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

#For Random Forest
#Decision Tree for Pclass
Pclass.model <- rpart(Survived~Pclass, data=train, method="class",cp=.03)
rpart.plot(Pclass.model)
rpart.plot(Pclass.model,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)
#Decision Trees for Sex
sex.model <- rpart(Survived~Sex, data=train, method="class",cp=.03)
rpart.plot(sex.model)
rpart.plot(sex.model,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)
#Decision Tree for embarked
embarked.model <- rpart(Survived~Embarked, data=train, method="class",cp=.03)
rpart.plot(embarked.model)
rpart.plot(embarked.model,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

#Example
fit_temp <- randomForest(as.factor(Survived) ~ Age,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
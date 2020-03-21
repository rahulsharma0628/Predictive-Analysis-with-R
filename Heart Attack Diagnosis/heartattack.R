install.packages('C50')
install.packages("readxl")
library(C50)
library(readxl)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(mlbench)
library(tidyverse)
library(caret)

#Data Exploration
data <- read_excel("Patient_Data-3.xlsx")
head(data)
str(data)
summary(data)

#Missing Vale
sum(is.na(data))

#duplicate Records
nrow(data[duplicated(data),])
mydata <- data %>% distinct()
summary(mydata)

#plots
ggplot(data = mydata, aes(x = gender)) + geom_bar(fill="#FF9999") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = diabetes)) + geom_bar(fill="#D55E00") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = smoker)) + geom_bar(fill="#F0E442") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = active)) + geom_bar(fill="#009E73") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = obesity)) + geom_bar(fill="#56B4E9") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = heartattack_s)) + geom_bar(fill="#E69F00") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = bp)) + geom_bar(fill="#CC79A7") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(x = cholesteral)) + geom_bar(fill="#FF9999") + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
ggplot(data = mydata, aes(y = age)) + geom_boxplot(fill="#009E73") 

#ready for model - c5.0

drop <- c("heartattack_s")
mydata[, !(names(mydata) %in% drop)]
mydata[,1] <- as.numeric(mydata[[1]])
mydata[,7] <- as.factor(mydata[[7]])
mydata[,6] <- as.factor(mydata[[6]])
mydata[,5] <- as.factor(mydata[[5]])
mydata[,4] <- as.factor(mydata[[4]])
mydata[,3] <- as.factor(mydata[[3]])
mydata[,2] <- as.factor(mydata[[2]])
mydata[,8] <- as.factor(mydata[[8]])
mydata[,9] <- as.factor(mydata[[9]])

c50_tree <- C5.0(heartattack_s ~ ., data = mydata)
summary(c50_tree)

c50_tree_rules <- C5.0(heartattack_s ~ ., data = mydata, rules=TRUE)
summary(c50_tree_rules)

str(mydata[,-7])
plot(c50_tree)

prediction <- predict(c50_tree, mydata[,-7], type = "class")
prediction.rules <- predict(c50_tree_rules, mydata[,-7], type = "class")


confusionMatrix(prediction, mydata$heartattack_s)
confusionMatrix(prediction.rules, mydata$heartattack_s)


#CART
cart_tree <- rpart(heartattack_s ~ ., data = mydata, method = 'class')
summary(cart_tree)
rpart.plot(cart_tree)

printcp(cart_tree)
plotcp(cart_tree)
which.min(cart_tree$cptable[,"xerror"])
cart_tree$cptable[which.min(cart_tree$cptable[,"xerror"]),"CP"]

prediction.cart <- predict(cart_tree, mydata[,-7], type = "class")

confusionMatrix(prediction.cart, mydata$heartattack_s)

#################################################################################################
#################################################################################################

#Data Partitioning
set.seed(123)
sample <- mydata$heartattack_s %>% createDataPartition(p = 0.8, list = FALSE)

nrow(sample)
#train test creation
train <- mydata[sample, ]
test <- mydata[-sample, ]

train_c50tree <- C5.0(heartattack_s ~ ., data = train, type="class" )
summary(train_c50tree)
plot(train_c50tree)

prediction.test <- predict(train_c50tree, test[,-7], type = "class")
prediction.train <- predict(train_c50tree, train[,-7], type = "class")

confusionMatrix(prediction.test, test$heartattack_s)
confusionMatrix(prediction.train, train$heartattack_s)


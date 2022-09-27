bank <- read.delim("bank.csv", sep = ";", header = TRUE)
library(dplyr)
library(ggplot2)
library(stringr)
library(assertive)
View(bank)
head(bank)
summary(bank)
assert_all_are_not_na(bank)
sum(is.na(bank))
library(visdat)
vis_miss(bank)

# Since there is no missing values, there is no need to clean the data. 

# EDA
levels(bank$y)
levels(bank$job)
levels(bank$marital)
levels(bank$education)
levels(bank$default)
levels(bank$housing)
levels(bank$loan)
levels(bank$contact)
levels(bank$month)
levels(bank$poutcome)

table(bank$y)
table(bank$job)
table(bank$marital)
table(bank$education)
table(bank$default)
table(bank$housing)
table(bank$loan)
table(bank$contact)
table(bank$month)
table(bank$poutcome)

bank %>% group_by(age, job, education) %>% summarise(mean_balance = mean(balance))
ggplot(bank, aes(x = job)) + geom_bar()
ggplot(bank, aes(x = education)) + geom_bar()
ggplot(bank, aes(x = housing, y = age, color = marital)) + geom_boxplot()       
ggplot(bank, aes(x = education, y = age, color = loan)) + geom_boxplot()
ggplot(bank, aes(x = job, y = age, color = marital)) + geom_boxplot()
ggplot(bank, aes(x = default, y = age)) + geom_boxplot()
ggplot(bank, aes(x = default, y = age, color = education)) + geom_boxplot()
ggplot(bank, aes(x = default, y = age, color = marital)) + geom_boxplot()
ggplot(bank, aes(x = housing, y = age)) + geom_boxplot()
ggplot(bank, aes(x = housing, y = age, color = education)) + geom_boxplot()
ggplot(bank, aes(x = housing, y = age, color = marital)) + geom_boxplot()
ggplot(bank, aes(x = housing, y = age, color = default)) + geom_boxplot()
ggplot(bank, aes(x = loan, y = age)) + geom_boxplot()
ggplot(bank, aes(x = loan, y = age, color = education)) + geom_boxplot()
ggplot(bank, aes(x = loan, y = age, color = marital)) + geom_boxplot()
ggplot(bank, aes(x = loan, y = age, color = default)) + geom_boxplot()

#Train-Test Split
install.packages('caTools')
library(caTools)
set.seed(101)
sample <- sample.split(bank$y, SplitRatio = 0.70)

#Training Data
train <- subset(bank, sample == TRUE)

#Testing Data
test <- subset(bank, sample == FALSE)

#Training the Model
model <- glm(as.factor(y) ~ ., family = binomial(logit), data = train)
summary(model)

test$predicted.y <- predict(model, newdata = test, type = "response")
table(test$y, test$predicted.y > 0.5)

#Accuracy
(45+1174)/(1174+26+111+45)

#Therefore, this logistic regression model is 90% accurate.

#Precision
(45)/(45+111)

#Therefore, this model is 29% precise.

#Recall
(45)/(45+26)

#Therefore, this model is 63% recall. 
# Loading the dataset

advertising <- read.csv("C:\\Users\\HP\\Downloads\\advertising.csv", header = TRUE)
head(advertising)

# Previewing the dataset

View(advertising)
str(advertising)
dim(advertising)
class(advertising)

Our data frame has 1000 rows and 10 columns

# Data Cleaning
# Looking for null values

is.na(advertising)

#Our data had no null values though we still went ahead to comfirm the same as follows

#Finding out the number of missing values in each column

colSums(is.na(advertising))

# None of the columns had missing values as confirmed below;


sum(is.na(advertising))


#Screening for outliers on the numerical columns using boxplots

boxplot(advertising$Area.Income)
boxplot(advertising$Daily.Time.Spent.on.Site)
boxplot(advertising$Age)
boxplot(advertising$Daily.Internet.Usage)

# Only one variable had oultiers, Area.Income.
# Removing outliers from it by winsorizing as follows;
# Setting the benchmark


bench <- 47032 - 1.5 * IQR(advertising$Area.Income) 
bench


# Winsorizing

advertising$Area.Income <- advertising$Area.Income
advertising$Area.Income[advertising$Area.Income< bench]
advertising$Area.Income[advertising$Area.Income < bench]<- bench

summary(advertising$Area.Income)

boxplot(advertising$Area.Income)


# The boxplot shows that the outliers in that column have been winsorized

# To remove duplicates if any

new_adv = unique(advertising)
new_adv
dim(new_adv)

# Our data frame had no dulpicates that's why we the number of rows and columns remained the same. 1000 rows and 10 columns just as the original one.

# UNIVARIATE ANALYSIS

summary(advertising)

# We got the summary statistics as above that gave some of the univariate analysis; mean, quantiles, median, min and max values, and ranges of the various column variables.

# Further univariate statistical analysis.
# Filtering the numerical variables only as follows;

install.packages("tidyverse")
library(tidyverse)

numerical_adv <- advertising %>% select(1, 2, 3, 4, 7, 10)
head(numerical_adv)

View(numerical_adv)
str(numerical_adv)


# The mode, kurtosis and skewness are as below;

# We get the mode of the variables as follows

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

Daily.Time.Spent.on.Site.mode <- getmode(numerical_adv$Daily.Time.Spent.on.Site.mode)
Daily.Time.Spent.on.Site.mode

Age.mode <- getmode(numerical_adv$Age.mode)
Age.mode

Area.Income.mode <- getmode(numerical_adv$Area.Income.mode)
Area.Income.mode

Daily.Internet.Usage.mode <- getmode(numerical_adv$Daily.Internet.Usage.mode)
Daily.Internet.Usage.mode

Male.mode <- getmode(numerical_adv$Male.mode)
Male.mode

Clicked.on.Ad.mode <- getmode(numerical_adv$Clicked.on.Ad.mode)
Clicked.on.Ad.mode

# Our variables do not have any modal values

# We then get the kurtosis of the different variables

install.packages("e1071")
library(e1071)

Daily.Time.Spent.on.Site.kurtosis <- numerical_adv$Daily.Time.Spent.on.Site
kurtosis(Daily.Time.Spent.on.Site.kurtosis)

Age.kurtosis <- numerical_adv$Age
kurtosis(Age.kurtosis)

Area.Income.kurtosis <- numerical_adv$Area.Income
kurtosis(Area.Income.kurtosis)

Daily.Internet.Usage.kurtosis <- numerical_adv$Daily.Internet.Usage
kurtosis(Daily.Internet.Usage.kurtosis)

Male.kurtosis <- numerical_adv$Male
kurtosis(Male.kurtosis)

Clicked.on.Ad.kurtosis <- numerical_adv$Clicked.on.Ad
kurtosis(Clicked.on.Ad.kurtosis)

# We then get the skewness of the variables

Daily.Time.Spent.on.Site.skewness <- numerical_adv$Daily.Time.Spent.on.Site
skewness(Daily.Time.Spent.on.Site.skewness)

Age.skewness<- numerical_adv$Age
skewness(Age.skewness)

Area.Income.skewness <- numerical_adv$Area.Income
skewness(Area.Income.skewness)

Daily.Internet.Usage.skewness <- numerical_adv$Daily.Internet.Usage
skewness(Daily.Internet.Usage.skewness)

Male.skewness <- numerical_adv$Male
skewness(Male.skewness)

Clicked.on.Ad.skewness <- numerical_adv$Clicked.on.Ad
skewness(Clicked.on.Ad.skewness)

# Graphical analysis
# Barplots 

age_frequency <- table(advertising$Age)
barplot(age_frequency)

male_freq <- table(advertising$Male)
barplot(male_freq)

click_freq <- table(advertising$Clicked.on.Ad)
barplot(click_freq)

# Observations
# 1. Most of the people who click on the ads are of the ages between 28 years and 40 years.
# 2. From the above barplots we observe that those who click on the ads are mostly female as represented by 0.
# 3. The number of ads that are clicked on are the same as the ads that are not clicked on which is not good as the number of clicked on ads should be higher for good business.

# Histograms

hist(advertising$Area.Income)

hist(advertising$Daily.Time.Spent.on.Site)

hist(advertising$Daily.Internet.Usage)

# Observations
# 1. Most of those who click on the ads earn salaries between 50,000 and 70,000.
# 2. Most of those who click on the ads mostly spend between 70 to 85 minutes on the ad sites.
# 3. Daily internet usage varies with the ads so there is no much consisteny.

# BIVARIATE ANALYSIS
#Encode the categorical variables to be numerical so we can check for correlation

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}
table(advertising[["Ad.Topic.Line"]], encode_ordinal(advertising[["Ad.Topic.Line"]]), useNA = "ifany")
table(advertising[["City"]], encode_ordinal(advertising[["City"]]), useNA = "ifany")
table(advertising[["Country"]], encode_ordinal(advertising[["Country"]]), useNA = "ifany")
table(advertising[["Timestamp"]], encode_ordinal(advertising[["Timestamp"]]), useNA = "ifany")
new_advert <- advertising
new_advert[["Ad.Topic.Line_encoded"]] <- encode_ordinal(advertising[["Ad.Topic.Line"]])
new_advert[["City_encoded"]] <- encode_ordinal(advertising[["City"]])
new_advert[["Country_encoded"]] <- encode_ordinal(advertising[["Country"]])
new_advert[["Timestamp_encoded"]] <- encode_ordinal(advertising[["Timestamp"]])
head(new_advert)

# Drop the categorical columns

new_advert1 = subset(new_advert, select = -c(Ad.Topic.Line, City, Country, Timestamp) )
head(new_advert1)



# Confirm the datatypes of the encoded columns

sapply(new_advert1, class)

# To get the covariance between all the variables

cov(new_advert1, y=new_advert1, use="all.obs")

# To get the correlation matrix between the variables

install.packages("GGally")
library(ggplot2)
ggcorr(new_advert1, label = TRUE, label_alpha = 4)

# We then do a scatter plot to further show the relationship between the variables

plot(new_advert1)
# Observations
# 1. There are no correlation between a clicked ad and any of the other variables.
# 2. The Ad.Topic.Line has a perfect correlation with Timestamp
# 3. The Ad.Topic.Line has another perfect correlation with City.Code.
# 4. Then City.code has another perfect correlation with Timestamp

# REGRESSION
# Multiple Linear Regression
# Applying the linear regression function lm().

multiple_lm <- lm(Clicked.on.Ad ~ ., new_advert1)

# Generating the anova table

anova(multiple_lm)

# Then performing our prediction
# Printing out our result
prediction

# Getting the summary of the model

summary(multiple_lm)

# KNN Algorithm
# We are going to normalize the data first

data_norm <- function(x){((x - min(x))/(max(x) - min(x)))}
new_advert1_norm <- as.data.frame(lapply(new_advert1[, -6], data_norm))

# Confirming if our data has been normalized as follows

summary(new_advert1_norm[,2:5])

# Well our max and minimun values are the same on all the chosen variables, therefore our dataset has been normalized.

# Splitting our data into training and test sets
# Our data has 1000 observations and splitting into 80% and 20% ratio will render, 800 on train and 200 on test as follows

advert_train <- new_advert1_norm[1:800,]
advert_test <- new_advert1_norm[801:1000,]

#Running the knn function

library(class)
advert_pred <- knn(advert_train, advert_test, new_advert1[1:800,6], k = 32)


# Validating the predicted labels with the actual labels by getting the confusion matrix

table(advert_pred, new_advert1[801:1000,6])

# This shows that out of 99 (0) observations 12 were misclassified, and out of 101 (1) observations only 1 was misclassified.

# Checking the accuracy

tb <- table(advert_pred, new_advert1[801:1000,6])
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

# Our KNN model is 93.5% which is not so bad. 
# To reduce misclassification we could modify the value of k by optimization.

# DECICION TREE Algorithm

# Renaming our dataset and giving it a new name

data <- new_advert1


# Looking at the structure of the data again

str(data)

# Our target variable click on ad is given as an integer so we will make it categorical by changing it to be as a factor by creating a new variable as folows

data$Clicked.on.AdF <- factor(data$Clicked.on.Ad)

# A new column will be created in the dataset with the variables as a factor instead of integers which will bw labelled as Clicked.on.AdF.
# We confirm the above as follows

str(data)

# Again partitioning the data into training and validation sets

set.seed(42)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[pd==1,]
test <- data[pd==2,]

# Decision Tree with party

library(party)
tree <- ctree(Clicked.on.AdF~Daily.Time.Spent.on.Site+Age+Area.Income+Daily.Internet.Usage+Male+Ad.Topic.Line_encoded+City_encoded+Country_encoded+Timestamp_encoded, data = train,controls = ctree_control(mincriterion = 0.95,minsplit = 200))
tree

# We then plot the tree as below after pruning the tree by adding some controls as above; 95% confidence level and 20 minimum splits

dev.off()
plot(tree)

# From the tree we observe that the most important variable in classifying whether an individual will click on the add or not is the Daily internet usage
# The more we inrease the number of minimum splits the lesser the number of nodes and the easier the interpretation of the tree.

# Making predictions based on our tree

predict(tree, test, type = "prob")

# Out of the 196 observations, the first one is the probability that a person will not click an ad as represented by 0, and the second one is the probability that a person will click an ad as represented by 1.
# If we remove the parameter type, this is what we get for the 196 observations

predict(tree, test)


# We find that the prediction classifies the observations as either clicked on ad or not.

# Decision Tree with rpart

library(rpart)
tree1 <- rpart(Clicked.on.AdF~Daily.Time.Spent.on.Site+Age+Area.Income+Daily.Internet.Usage+Male+Ad.Topic.Line_encoded+City_encoded+Country_encoded+Timestamp_encoded, data = train)
tree1

# Plotting the tree

library(rpart.plot)
rpart.plot(tree1, extra = 4)

# This gives the following outcomes;
# If the daily internet usage is greater than 178, then an individual has a 52% chance of not clicking on an ad.
# If the daily internet usage is less than 178, then an individual has 48% chance of clicking on an ad.
# On the other hand, if an individual spends more than 178 internet usage and 60 minutes, then he/she has a 47% chance of not clicking on an ad.
# And if they spend more than 178 internet usage and less than 60 minutes on a site, then they have 5%cnave of clicking on ad.


predict(tree1,test)

# Getting the misclassification error for our train data

tab <- table(predict(tree), train$Clicked.on.AdF)
tab

# From the table we see that out of the 430 (0) observations, 38 were misclassified. 
# Again out of the 374 (1) onservations 17 were misclassified.
# We obtain the misclassification error based on the train data as follows

1-sum(diag(tab))/sum(tab)

# We get 6.84% error
# We then get the misclasiffication error based on test data as follows;

testpred <- predict(tree, newdata = test)
tab <- table(testpred,test$Clicked.on.AdF)
tab

# From the table we see that out of the 99 (0) observations, 12 were misclassified. 
# Again out of the 97 (1) onservations 4 were misclassified. 


# We obtain the misclassification error based on the test data as follows

1-sum(diag(tab))/sum(tab)

# We get 8.16% misclassification error which if faily small.

# SUPPORT VECTOR MACHINE Algorithm

# Renaming our dataset as follows

data <- new_advert1

# Normalizing the data

data_norm <- function(x){((x - min(x))/(max(x) - min(x)))}
data_normal <- as.data.frame(lapply(data, data_norm))

# Splitting our data into training and test sets

install.packages('caret')
library(caret)

intrain <- createDataPartition(y = data$Clicked.on.Ad, p= 0.8, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]


# We check the dimensions of out training dataframe and testing dataframe

dim(training); 
dim(testing);

# Then check the summary of our data by using the summary() function

summary(data)

# Factorizing our categorical variable

training$Clicked.on.Ad <- factor(training$Clicked.on.Ad)
str(training)


# Training our model

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(Clicked.on.Ad ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# Checking for the result

svm_Linear

# Making predicton

test_pred <- predict(svm_Linear, newdata = testing)
test_pred


# Now checking for our accuracy of our model by using a confusion matrix 

confusionMatrix(table(test_pred, testing$Clicked.on.Ad))

# From our confusion matrix, we observe that out of 107 (0) observations, 7 were misclassified.
# Out of 93 (1) observations non were misclassified.
# Our SVM model has a performed better than the previous ones giving an accuracy of 96.5%

# NAIVE BAYES Algorithm
# Installing and loading required packages

install.packages('tidyverse')
library(tidyverse)

install.packages('caretEnsemble')
library(caretEnsemble)

install.packages('psych')
library(psych)

install.packages('Amelia')
library(Amelia)

install.packages('mice')
library(mice)

install.packages('rpart')
library(rpart)

install.packages('randomForest')
library(randomForest)

# We are going to describe our data

describe(data)

# Normalizing the data

data_norm <- function(x){((x - min(x))/(max(x) - min(x)))}
data_normal <- as.data.frame(lapply(data, data_norm))

# Splitting data into training and test data sets

indxTrain <- createDataPartition(y = data$Clicked.on.Ad,p = 0.80,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

# Factorizing our categorical variable

training$Clicked.on.Ad <- factor(training$Clicked.on.Ad)
str(training)

# Creating objects x which holds the predictor variables and y which holds the response variables

x = training[,-6]
y = training$Clicked.on.Ad

# Now building our model 

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

# Model Evalution
# Predicting our testing set

Predict <- predict(model,newdata = testing )

# Getting the confusion matrix to see accuracy value and other parameter values

testing$Clicked.on.Ad <- factor(testing$Clicked.on.Ad)
str(testing)
confusionMatrix(Predict, testing$Clicked.on.Ad)


# 1).From our model predictions, we observe that out of the 104 (0) observations, 8 were misclassified.
# 2).Out of 96 (1) observations, 4 were misclassified.
# 3). Our model is 94% accurate which is fairly lower than the SVM model but we can say that it has performed well.


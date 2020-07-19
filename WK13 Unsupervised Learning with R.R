# Loding the dataset
customers <- read.csv("C:\\Users\\HP\\Downloads\\online_shoppers_intention.csv", header = TRUE)

# Previewing the dataset
head(customers)

View(customers)
str(customers)
dim(customers)
class(customers)

# Data Cleaning
# Looking for null values

is.na(customers)

# Finding out the number of missing values in each column

colSums(is.na(customers))


# We visualize our dataset by checking how many missing values
install.packages('mice')
library(mice)

missmap(customers)

# We can learn from the above dataset that there are many missing values 
# thus removing them wouldn't be better options since we'd be left with a smaller dataset. 
# Thus we resort to performing imputations by using the mice package in R.
# We use mice package to predict missing values

mice_mod <- mice(customers[, c("Administrative","Administrative_Duration","Informational","Informational_Duration","ProductRelated","ProductRelated_Duration","BounceRates","ExitRates")], method='rf')
mice_complete <- complete(mice_mod)

# We transfer the predicted missing values into the main data set

customers$Administrative <- mice_complete$Administrative
customers$Administrative_Duration <- mice_complete$Administrative_Duration
customers$Informational <- mice_complete$Informational
customers$Informational_Duration<- mice_complete$Informational_Duration
customers$ProductRelated <- mice_complete$ProductRelated
customers$ProductRelated_Duration <- mice_complete$ProductRelated_Duration
customers$BounceRates <- mice_complete$BounceRates
customers$ExitRates <- mice_complete$ExitRates

# Checking if there are still missing values
missmap(customers)
# There are no more missing values

boxplot(customers$Administrative)
boxplot(customers$Administrative_Duration)
boxplot(customers$Informational)
boxplot(customers$Informational_Duration)
boxplot(customers$ProductRelated)
boxplot(customers$ProductRelated_Duration)
boxplot(customers$BounceRates)
boxplot(customers$ExitRates)
boxplot(customers$PageValues)
boxplot(customers$SpecialDay)
boxplot(customers$OperatingSystems)
boxplot(customers$Browser)
boxplot(customers$Region)

# Removing outliers using variable transformation
customers$Administrative <- log(customers$Administrative)
customers$Administrative_Duration <- log(customers$Administrative_Duration)
customers$Informational <- log(customers$Informational)
customers$Informational_Duration <- log(customers$Informational_Duration)
customers$ProductRelated <- log(customers$ProductRelated)
customers$ProductRelated_Duration <- log(customers$ProductRelated_Duration)
customers$BounceRates <- log(customers$BounceRates)
customers$ExitRates <- log(customers$ExitRates)
customers$PageValues <- log(customers$PageValues)
customers$SpecialDay <- log(customers$SpecialDay)
customers$OperatingSystems <- log(customers$OperatingSystems)
customers$Browser <- log(customers$Browser)
customers$Region <- log(customers$Region)

# To confirm if the outliers have been removed
boxplot(customers$Administrative)
boxplot(customers$Administrative_Duration)
boxplot(customers$Informational)
boxplot(customers$Informational_Duration)
boxplot(customers$ProductRelated)
boxplot(customers$ProductRelated_Duration)
boxplot(customers$BounceRates)
boxplot(customers$ExitRates)
boxplot(customers$PageValues)
boxplot(customers$SpecialDay)
boxplot(customers$OperatingSystems)
boxplot(customers$Browser)
boxplot(customers$Region)

# To remove duplicates if any

customers = unique(customers)
customers

dim(customers)
# our dataset has remained with 12212 from 12230 after the duplicates have been removed.

# K-MEANS CLUSTERING

# Label encoding the categorical variables

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}
table(customers[["Month"]], encode_ordinal(customers[["Month"]]), useNA = "ifany")
table(customers[["VisitorType"]], encode_ordinal(customers[["VisitorType"]]), useNA = "ifany")
table(customers[["Weekend"]], encode_ordinal(customers[["Weekend"]]), useNA = "ifany")
table(customers[["Revenue"]], encode_ordinal(customers[["Revenue"]]), useNA = "ifany")
new_data <- customers
new_data[["Month"]] <- encode_ordinal(customers[["Month"]])
new_data[["VisitorType"]] <- encode_ordinal(customers[["VisitorType"]])
new_data[["Weekend"]] <- encode_ordinal(customers[["Weekend"]])
new_data[["Revenue"]] <- encode_ordinal(customers[["Revenue"]])
head(new_data)

str(new_data)


# We are going to normalize the data first

data_norm <- function(x){((x - min(x))/(max(x) - min(x)))}
data_normal <- as.data.frame(lapply(new_data[, -6], data_norm))

# Confirming if our data has been normalized as follows
summary(data_normal[,2:5])


# Preprocessing the dataset
# Loading the necessary libraries

install.packages("stats")
library(stats)
library(ggfortify)

#Converting the normal_data into unlabeled data since k means clustering is an unsupervised learning algorithm
data <- select(data_normal, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))

data

# Choosing the optimum number of clusters using WSS plot

wssplot <- function (data, nc = 15, seed = 1234)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of clusters",
       ylab = "Within groups sum of squares")
}

wssplot(data) 
# From our plot we see a kink where the value is 2 hence we take the number oc cluaters to be 2

# KMeans cluster when number of clusters is 2

KM <- kmeans(data,2)

# Evaluating cluster analysis
# Using Cluster plot

autoplot(KM, data, frame = TRUE)
# From the plot we see that all the observations have been classified into two clusters, 1 and 2.
# There is no overlapping hence we can say that the cluster analysis has been successfully deployed.

# Evaluating the performance of the model using cluster centers

KM$centers
# From the output we observe that the centers of these clusters are not overlapping hence we conclude that these clusters are distinct in nature.

# HIERACHICAL CLUSTERING
# Previewing the data
head(data)

# The data is already normalized
# First we use the dist() function to compute the Euclidean distance between observations, 
# d will be the first argument in the hclust() function dissimilarity matrix

d <- dist(data, method = "euclidean")
d

# Hierarchical clustering using the Ward's method

res.hc <- hclust(d, method = "ward.D2" )

#  plotting the obtained dendrogram

plot(res.hc, cex = 0.6, hang = -4)
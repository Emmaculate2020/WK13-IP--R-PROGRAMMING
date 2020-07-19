#### 1.1 Simple Linear Regression Code Example
## Example 
# ---
# Simple linear regression example 
 

# Let's first import ggplot
library(ggplot2) 

# Examining the data before fitting models by creating a scatter plot 
# Scatter plots can help visualize any linear relationships between the dependent (response) variable 
# and independent (predictor) variables.

ggplot(mtcars, aes(hp, mpg))+
  geom_point()+
  labs(title = "Gross Horse Power VS Miles Per Gallon",
       x = "hp",
       y = "mpg")

# We can also find the coreelation between the variables as follows


cor(mtcars$hp, mtcars$mpg)

# The linear model function lm, used below will create the relationship model between the predictor and the response variable. 
# mpg~hp presenting the relation between x and y and mtcars the vector on which the formula will be applied.

simple_lm <- lm(mpg~hp, mtcars)
simple_lm

# Generating the anova table. This table will 
# contain the sums of squares, degrees of freedom, F statistic, and p value
# 
anova(simple_lm)

# Predicting the response variables
# 
pred1 <- predict(simple_lm, mtcars)
pred1

#### 1.2 Simple Linear Regression Code Example 


```R
## Example 
# ---
# 
# Considering two vectors
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Examining the data by also finding the correlation coefficient
cor(height, weight)

# Apply the lm() function.
relation <- lm(weight~height)
relation

# Then getting the Summary of the relationship
summary(relation)

# Then wrapping the parameters inside a new data frame named 
# and later finding the weight of a person with height 170 and 
# 
a <- data.frame(height = 170)
result <-  predict(relation,a)

result

#### 1.3 Simple Linear Regression Code Example


```R
# Challenge
# ---
# Apply simple linear regression to the faithful dataset 
# and estimate the next eruption duration if the waiting time 
# since the last eruption has been 80 minutes
# ---
# OUR CODE GOES BELOW
# 

# Previewing the database
# 
head(faithful)

# Examining the data before fitting models by creating a scatter plot
# 
ggplot(faithful, aes(eruptions,waiting))+
         geom_point() +
         labs(title = "Eruptions vs Waiting",
              x = "eruptions",
              y = "waiting")

# Examining the data by also finding the correlation coefficient

cor(faithful$eruptions,faithful$waiting)

# Applying the lm() function.

faithful_lm <- lm(eruptions~waiting, faithful)
faithful_lm

# Working on the solution
# Then getting the Summary of the relationship
summary(faithful_lm)


#### 2.1 Multiple Linear Regression Code Example


```R
## Example  
# ---
# Multiple Linear Regression Example 
# ---
# Use the diamonds dataset
#

head(diamonds)

# Applying the lm() function.
multiple_lm <- lm(price ~ ., diamonds)
multiple_lm

# Generating the anova table
anova(multiple_lm)

# Then performing our prediction 
pred2 <- predict(multiple_lm, diamonds)

# Printing out our result
pred2

#### 2.2 Multiple Linear Regression Code Example
# Having "disp","hp" and "wt" as predictor variables - we've selected these variables from the mtcars database

input <- mtcars[,c("mpg","disp","hp","wt")]

# Previewing these selected input predictor variables
head(input)

# Creating the relationship model
model <- lm(mpg~disp+hp+wt, data = input)
model

# Getting the summary of the model 

summary(model)

# For a car with disp = 221, hp = 102 and wt = 2.91 the predicted mileage is 
# 
a <- data.frame(disp = 221, hp = 102, wt = 2.91)
predicted_mileage <- predict(model, a)

# printing the predicted mileage
predicted_mileage

#### 3.1 Cross Validation Code Example

## Example 
# ---
# Cross Validation Example 
# ---
# OUR CODE GOES BELOW
# 

# Using the previous diamonds dataset 
set.seed(42)

multiple_lm2 <- train(price ~ ., diamonds,
                      method = "lm", 
                      trControl = trainControl(method = "cv", 
                                               number = 10, 
                                               verboseIter = FALSE))
summary(multiple_lm2)

multiple_lm2

# Once we have trained our model, we can directly use this train object as input to the predict method:

pred3 <- predict(multiple_lm2, diamonds)

error <- pred3 - diamonds$price

rmse_xval <- sqrt(mean(error^2)) ## xval RMSE

rmse_xval

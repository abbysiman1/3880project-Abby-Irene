source("http://www.uvm.edu/~rsingle/stat3880/data/scripts-3880.R")
install.packages("glmnet")
library(glmnet)
setwd("/Users/abigailsimanjuntak")


data <- read.csv("2015satisfactiondataset.csv")
colnames(data) <- tolower(colnames(data))

colSums(is.na(data))
# 393 missing values in arrival.delay.in.minutes

#dropping incomplete cases
data <- na.omit(data)


# creating dummy variable for 
data$satisfaction01 <- as.factor(ifelse(data$satisfaction_v2 == "satisfied", 1, 0))

#logistic regression using all variables 
log.reg.mod <- glm(satisfaction01~.,family=binomial,dat=data)

#summarizing the findings
summary(log.reg.mod)

# create training and test data
set.seed(1)
train <- sample(1:nrow(data), 0.8 * nrow(data))
training_data <- data[train, ]
testing_data <- data[-train, ]

# defining predictor and response variables
x_train <- training_data[, 8:24]
x_train <- as.data.frame(sapply(x_train, as.numeric))
y_train <- training_data$satisfaction01

x_train <- as.matrix(training_data[, 8:24])
x_test <- as.matrix(testing_data[, 8:24])

x_test <- testing_data[, 8:24]
x_test <- as.data.frame(sapply(x_test, as.numeric))

lasso_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

# Print the optimal value of lambda
print(lasso_model$lambda.min)

# Extract coefficients of the selected variables
selected_coefficients <- coef(lasso_model, s = "lambda.min")

# Print the coefficients
print(selected_coefficients)

# Make predictions on new data
# Assuming 'X_test' is your matrix of predictor variables for the testing set
predictions <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")

predictions

# create models from this and compute the accuracy

# Evaluate model performance
#use appropriate metrics such as accuracy, AUC, etc.

# Higher magnitude means more of an impact

# which variables are correlated with each other



# area under the roc
# mcfaddens 
# training





# do this after predicting
# comparing findings to that of null model
nullmod = glm(satisfaction01 ~ 1, family=binomial, dat=data)
logLik(nullmod)
logLik(log.red.mod)
1-logLik(log.reg.mod)/logLik(nullmod)              
as.numeric( 1-logLik(log.reg.mod)/logLik(nullmod) )


# Exhaustive searches 
# Forward / backwards selection
#psuedo r squared mcfaddens

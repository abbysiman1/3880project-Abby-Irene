source("http://www.uvm.edu/~rsingle/stat3880/data/scripts-3880.R")
install.packages("glmnet")
library(glmnet)


data <- read.csv("/Users/irenediaz-perez/Desktop/UVM/spring 2024/STAT3880/final project/satisfaction_2015.csv")
colnames(data) <- tolower(colnames(data))

colSums(is.na(data))
# 393 missing values in arrival.delay.in.minutes

#dropping incomplete cases
data <- na.omit(data)


# creating dummy variable for 
data$satisfaction01 <- as.factor(ifelse(data$satisfaction_v2 == "satisfied", 1, 0))

# creating a continuous variable using factors that are not on the flight to predict satisfaction based off on flight experience
columns_to_sum <- c("departure.arrival.time.convenient", "ease.of.online.booking",
                    "gate.location", "online.boarding", "baggage.handling",
                    "departure.delay.in.minutes", "arrival.delay.in.minutes")

# Create a new column called "score" containing the row sums
continuous.data <- data
continuous.data$score <- rowSums(data[columns_to_sum], na.rm = TRUE)
continuous.data <- continuous.data[, !(names(continuous.data) %in% columns_to_sum)]


# Print the updated data frame
#print(data)
# Departure/Arrival time convenient Ease of Online booking Gate location Online boarding Baggage handling Departure Delay in Minutes Arrival Delay in Minutes

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

# multiple linear regression
lm_model <- lm(score ~ flight.distance + inflight.wifi.service + food.and.drink + seat.comfort + 
                 inflight.entertainment + on.board.service + leg.room.service + checkin.service + inflight.service + cleanliness, data = continuous.data)
summary(lm_model)
# low p-values: wifi, food and drink, seat comfort, enterainment, leg room, in flight service, cleanliness

# look at correlation matrix 

# Define the subset of predictor variables with adjusted column names
selected_predictors <- continuous.data[, c("flight.distance", "inflight.wifi.service", "food.and.drink", 
                                       "seat.comfort", "inflight.entertainment", "on.board.service", 
                                       "leg.room.service", "checkin.service", "inflight.service", 
                                       "cleanliness")]

# Compute the correlation matrix
correlation_matrix <- cor(selected_predictors)

# Print the correlation matrix
print(correlation_matrix)

# cleanliness correlated with food and drink, seat comfort, inflight entertainment
# food and drink correlated with seat comfort and in flight entertainment


library(randomForest)
library(gbm)


# Random Forest Model
rf_model <- randomForest(x_train, y_train, ntree=500, mtry=3, importance=TRUE)
rf_predictions <- predict(rf_model, newdata=testing_data)
rf_accuracy <- mean(rf_predictions == testing_data$satisfaction01)
print(paste("Accuracy of Random Forest:", rf_accuracy))
importance(rf_model)
varImpPlot(rf_model)

# Boosting Model
gbm_model <- gbm.fit(x_train, as.numeric(y_train) - 1, # as.numeric - 1 for binary 0/1 response
                     distribution = "bernoulli", 
                     n.trees = 1000, 
                     interaction.depth = 3, 
                     shrinkage = 0.01,
                     verbose=FALSE
                    )

# Prediction and evaluation on testing set
gbm_predictions_prob <- predict(gbm_model, newdata = x_test, n.trees = 1000, type = "response")
gbm_predictions <- ifelse(gbm_predictions_prob > 0.5, 1, 0)


accuracy <- mean(gbm_predictions == y_test)
print(paste("Accuracy of GBM model:", accuracy))
source("http://www.uvm.edu/~rsingle/stat3880/data/scripts-3880.R")
install.packages("glmnet")
library(glmnet)
setwd("/Users/abigailsimanjuntak")


data <- read.csv("2015satisfactiondataset.csv")

#cleaning column names
colnames(data) <- tolower(colnames(data))

# checking to see variables that are N/A
colSums(is.na(data))
# 393 missing values in arrival.delay.in.minutes

flight_data <- data

# creating dummy variable for 
flight_data$satisfaction01 <- as.factor(ifelse(data$satisfaction_v2 == "satisfied", 1, 0))

#dropping incomplete cases and creating a cleansed data set
flight_data <- na.omit(flight_data)

#dropping columns with insignificant variables
flight_data <- subset(flight_data, select = -c(id, satisfaction_v2, gender, customer.type, age, type.of.travel, class))

# Define the columns that have missing values (aka 0)
columns_to_check <- c("inflight.wifi.service","departure.arrival.time.convenient","ease.of.online.booking",
                      "gate.location", "food.and.drink","online.boarding","seat.comfort","inflight.entertainment",
                      "on.board.service","leg.room.service","baggage.handling","checkin.service","inflight.service",
                      "cleanliness")


# Loop through each column and drop the case if any of the columns equal 0
for (col in columns_to_check) {
  # Drop observations where the column equals 0
  flight_data <- flight_data[flight_data[[col]] != 0, ]
}

zero_counts <- sapply(flight_data[columns_to_check], function(x) sum(x == 0))
print(zero_counts)

# creating a continuous variable using factors that are not on the flight to predict satisfaction based on flight experience
columns_to_sum <- c("inflight.wifi.service","departure.arrival.time.convenient","ease.of.online.booking",
                    "gate.location", "online.boarding", "baggage.handling",
                    "departure.delay.in.minutes", "arrival.delay.in.minutes")

# Create a new column called "score" containing the row sums
continuous_data <- data
continuous_data <- na.omit(continuous_data)
continuous_data$score <- rowSums(continuous_data[columns_to_check], na.rm = TRUE)
continuous_data <- continuous_data[, !(names(continuous_data) %in% columns_to_check)]
continuous_data <- subset(continuous_data, select = -c(id, satisfaction_v2, gender, customer.type, type.of.travel, class))


# LASSO REGRESSION

# create training and test data
set.seed(1)
train <- sample(1:nrow(flight_data), 0.8 * nrow(flight_data))
training_data <- flight_data[train, ]
testing_data <- flight_data[-train, ]

# defining predictor and response variables
x_train <- training_data[, 1:17]
x_train <- as.data.frame(sapply(x_train, as.numeric))
x_train_scaled <- scale(x_train)
y_train <- training_data$satisfaction01

x_train <- as.matrix(training_data[, 1:17])
x_test <- as.matrix(testing_data[, 1:17])

x_test <- testing_data[, 1:17]
x_test <- as.data.frame(sapply(x_test, as.numeric))
x_test <- as.matrix(x_test)


lasso_model <- cv.glmnet(x_train_scaled, y_train, family = "binomial", alpha = 1)

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

#absolute value of coefficients
abs(selected_coefficients)

# top six are online.boarding, departure.arrival.time.convenient, inflight.wifi.service, flight.distance, leg.room.service, on.board.service

# creating a logistic regression model from top six predictors
log_mod <- glm(satisfaction01~online.boarding+departure.arrival.time.convenient+inflight.wifi.service+flight.distance+leg.room.service+on.board.service, family=binomial, data=flight_data)
pred.prob=predict(log_mod,type="response")
pred.satisfaction01=rep( "absent", length(pred.prob))
pred.satisfaction01[pred.prob>.5]="present"
tab = table(pred.satisfaction01,flight_data$satisfaction01)
tab
sum(diag(tab))/sum(tab)
#0.852

#Mcfadden's R square
nullmod = glm(satisfaction01 ~ 1, family=binomial, dat=flight_data)
logLik(nullmod)
logLik(log_mod)
1-logLik(log_mod)/logLik(nullmod)              
as.numeric( 1-logLik(log_mod)/logLik(nullmod) )
#0.48


#AUC

install.packages("pROC")
library(pROC)

# Assuming 'predictions' contains the predicted probabilities and 'y_test' contains the true labels
pred.prob=predict(log_mod,type="response")
roc_obj <- roc(flight_data$satisfaction01, pred.prob)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "blue")

# Add AUC value to the plot
text(0.8, 0.2, paste("AUC =", round(auc(roc_obj), 2)), col = "blue", cex = 1.2)

# Calculate the AUC value
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

#AUC: 0.92



# multiple linear regression
lm_model <- lm(score ~ age + flight.distance + departure.delay.in.minutes + arrival.delay.in.minutes, data = continuous_data)
summary(lm_model)

# R^2 of 0.03375

# RSE: 9.257

# Over response data not explained by predictor variables in linear regression model


library(randomForest)
library(gbm)
set.seed(1)
train <- sample(1:nrow(flight_data), 0.8 * nrow(flight_data))
training_data <- flight_data[train, ]
testing_data <- flight_data[-train, ]

# defining predictor and response variables
x_train <- training_data[, 1:17]
x_train <- as.data.frame(sapply(x_train, as.numeric))
y_train <- training_data$satisfaction01

x_train <- as.matrix(training_data[, 1:17])
x_test <- as.matrix(testing_data[, 1:17])

x_test <- testing_data[, 1:17]
x_test <- as.data.frame(sapply(x_test, as.numeric))
x_test <- as.matrix(x_test)

y_test <- testing_data$satisfaction01

# Random Forest Model
rf_model <- randomForest(x_train, y_train, ntree=500, mtry=3, importance=TRUE)
rf_predictions <- predict(rf_model, newdata=testing_data)
rf_accuracy <- mean(rf_predictions == testing_data$satisfaction01)
print(paste("Accuracy of Random Forest:", rf_accuracy))
importance(rf_model)
varImpPlot(rf_model)

#0.947

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
#0.914


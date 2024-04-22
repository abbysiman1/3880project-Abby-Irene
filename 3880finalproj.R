source("http://www.uvm.edu/~rsingle/stat3880/data/scripts-3880.R")
flight_data <- read.csv("satisfaction_2015.csv")
colnames(flight_data) <- tolower(colnames(data))

colSums(is.na(flight_data))
# 393 missing values in arrival.delay.in.minutes

#dropping incomplete cases
data <- na.omit(flight_data)

# Assuming satisfaction_score calculation is as before
flight_data$score <- rowSums(satisfaction_score, na.rm = TRUE)

# Ensure that satisfaction_v2 is a factor with two levels
flight_data$satisfaction_v2 <- factor(flight_data$satisfaction_v2, levels = c("neutral or dissatisfied", "satisfied"))

# Re-create the independent_vars to include all needed variables
independent_vars <- flight_data[, c("gender", "customer.type", "type.of.travel", "class", "flight.distance", "score")]
independent_vars$satisfaction_v2 <- flight_data$satisfaction_v2  # Adding the response variable to the predictors' data frame

# Sample data partitioning with a seed for reproducibility
train_indices <- sample(seq_len(nrow(independent_vars)), size = floor(0.5 * nrow(independent_vars)))

# Split data into train and test
train_data <- independent_vars[train_indices, ]
test_data <- independent_vars[-train_indices, ]

# Fit the Logistic Regression Model
fit.glm <- glm(satisfaction_v2 ~ ., data = train_data, family = binomial())

# Check the model summary
summary(fit.glm)

# Optionally, predict and evaluate the model's performance on test data
probabilities <- predict(fit.glm, newdata = test_data, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, "satisfied", "neutral or dissatisfied")

# Create a confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$satisfaction_v2)
conf_matrix

# Calculate error rate
error_rate <- mean(predicted_classes != test_data$satisfaction_v2)
error_rate

# Load necessary library
library(rpart)

# Fit a Decision Tree model
tree_model <- rpart(satisfaction_v2 ~ ., data = train_data, method = "class")

# Print the tree structure
print(tree_model)

# Plot the tree
plot(tree_model, main = "Decision Tree for Flight Satisfaction")
text(tree_model, use.n = TRUE)

# Predict using the tree model on test data
tree_predictions <- predict(tree_model, newdata = test_data, type = "class")

# Create a confusion matrix
conf_matrix_tree <- table(Predicted = tree_predictions, Actual = test_data$satisfaction_v2)
conf_matrix_tree

# Calculate error rate
error_rate_tree <- mean(tree_predictions != test_data$satisfaction_v2)
error_rate_tree

# Load the necessary library
library(randomForest)

# Fit a Random Forest model
forest_model <- randomForest(satisfaction_v2 ~ ., data = train_data, ntree = 500, importance = TRUE)

# Summary of the random forest model
print(forest_model)

# Predict using the Random Forest model on test data
forest_predictions <- predict(forest_model, newdata = test_data)

# Create a confusion matrix
conf_matrix_forest <- table(Predicted = forest_predictions, Actual = test_data$satisfaction_v2)
conf_matrix_forest

# Calculate error rate
error_rate_forest <- mean(forest_predictions != test_data$satisfaction_v2)
error_rate_forest

# Optional: Plot variable importance
varImpPlot(forest_model)

# Load necessary library
library(gbm)

# Assuming independent_vars already includes satisfaction_v2 and has been preprocessed as discussed
set.seed(123)
train_indices <- sample(seq_len(nrow(independent_vars)), size = floor(0.5 * nrow(independent_vars)))
train_data <- independent_vars[train_indices, ]
test_data <- independent_vars[-train_indices, ]

# Fit a Gradient Boosting model
gbm_model <- gbm(satisfaction_v2 ~ ., data = train_data, 
                 distribution = "bernoulli", # Since it's a binary classification problem
                 n.trees = 500,             # Number of boosting iterations
                 interaction.depth = 3,     # Depth of each tree
                 shrinkage = 0.01,          # Learning rate
                 cv.folds = 5,              # 5-fold cross-validation
                 n.minobsinnode = 10        # Minimum number of observations in the nodes
)

# Summary of the gradient boosting model
summary(gbm_model)

# Making predictions on the test set
gbm_predictions <- predict(gbm_model, newdata = test_data, n.trees = 500, type = "response")
gbm_pred_class <- ifelse(gbm_predictions > 0.5, "satisfied", "neutral or dissatisfied")

# Create a confusion matrix
conf_matrix_gbm <- table(Predicted = gbm_pred_class, Actual = test_data$satisfaction_v2)
conf_matrix_gbm

# Calculate error rate
error_rate_gbm <- mean(gbm_pred_class != test_data$satisfaction_v2)
error_rate_gbm



library(caret)
library(rsample)
library(tidyverse)
library(modeldata)
library(mclust)

# Load Song datasets

Song_dataset <- read.csv("C:/Users/PRANAV/Desktop/Conestoga 8971403/Multivariate/MV Project 2/Song_dataset.csv")

data(Song_dataset) 

# Cleaning and Manipulation for Modeling.

Song_dataset <- Song_dataset %>% select(-song, -Mood, -genre, -artist)

Song_dataset$year <- as.numeric(Song_dataset$year)
Song_dataset$duration_ms <- as.numeric(Song_dataset$duration_ms)
Song_dataset$speechiness <- as.numeric(Song_dataset$speechiness)
Song_dataset$liveness <- as.numeric(Song_dataset$liveness)
Song_dataset$danceability <- as.numeric(Song_dataset$danceability)
Song_dataset$valence <- as.numeric(Song_dataset$valence)
str(Song_dataset)

Song_dataset <- na.omit(Song_dataset)

# Converting response variable in factor

Song_dataset$Rating_popularity <- as.factor(Song_dataset$Rating_popularity)

# Set a seed for reproducibility

set.seed(123)

# Split the data into training and test sets

splitData <- createDataPartition(Song_dataset$Rating_popularity, p = 0.7, list = FALSE)
train_set <- Song_dataset[splitData, ]
test_set <- Song_dataset[-splitData, ]

# Checking the dimensions of the training and test sets

dim(train_set)
dim(test_set)

### Modal 1 : K-Nearest Neighbors (KNN)

# Train the KNN model on the training set

knn_song_popularity_model <- train(
  Rating_popularity ~ . - valence - duration_ms,
  data = train_set,
  method = "knn",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 5)
)

# Making predictions on the test set and 
# Evaluating the KNN Model performance on the test set

knn_predictions <- predict(knn_song_popularity_model, newdata = test_set)
confusionMatrix(knn_predictions, test_set$Rating_popularity)

### Model 2 : Boosting Modal

boosting_song_popularity_model <- train(
  Rating_popularity ~ . - speechiness - valence - duration_ms,
  data = train_set,  
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 8)
)

# Making predictions on the test set and Evaluating the Boosting Model
# performance on the test set

boosting_predictions <- predict(boosting_song_popularity_model, newdata = test_set)
confusionMatrix(boosting_predictions, test_set$Rating_popularity)

### Model 3 : Logistic Regression 

logistic_model_song <- train(
  Rating_popularity ~ . - speechiness - duration_ms,
  data = train_set, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 13)
)

# Making predictions on the test set and Evaluating the Logistic Regression Model
# performance on the test set

logistic_predictions <- predict(logistic_model_song, newdata = test_set)
confusionMatrix(logistic_predictions, test_set$Rating_popularity)

### Model 4 : Random Forest

random_forest_model <- train(
  Rating_popularity ~ .,
  data = train_set,
  method = "ranger",
  trControl = trainControl(method = "cv",
                           number = 10,
                           verboseIter = TRUE,
                           classProbs = TRUE),
  num.trees = 1000,
  importance = "impurity"
)

# Making predictions on the test set and Evaluating the Random Forest Model
# performance on the test set

forest_predictions <- predict(random_forest_model, newdata = test_set)
confusionMatrix(forest_predictions, test_set$Rating_popularity)


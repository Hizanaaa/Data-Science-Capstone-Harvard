# Install all needed libraries if it is not present
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(caret)) install.packages("caret")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(forcats)) install.packages("forcats")

#loading required Libraries
library(tidyverse)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(caret)
library(forcats)
library(RColorBrewer)

#############################################################
# Create edx set, validation set, and submission file
#############################################################

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
```

# Data Summary

#dimensions of the dataset 
dim(edx)

#class of the attributes in the dataset
sapply(edx,class) 

#structure of data
str(edx)

#first and last 6 rows of the dataset
head(edx)
tail(edx)

# Use kable to display the first 6 rows of data set
head(edx) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)



#summary of the edx dataset
summary(edx) |> 
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

edx |>
  group_by(title) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  head(n=25) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


## Data Cleaning and Preprocessing


#checking for missing values
sapply(edx, function(x) sum(is.na(x)))

#checking for duplicates
duplicated_rows <- edx[duplicated(edx), ]
nrow(duplicated_rows)

#checking for inconsistent data
inconsistent_titles <- edx |>
  group_by(movieId) |>
  summarise(n_titles = n_distinct(title)) |>
  filter(n_titles > 1)
nrow(inconsistent_titles)

#checking for outliers
outlier_ratings <- edx[abs(edx$rating - mean(edx$rating)) > 3 * sd(edx$rating), ]
nrow(outlier_ratings)
```


# Create a data frame to store the results
results <- data.frame(
  Check = c("Missing Values", "Duplicates", "Inconsistent Data", "Outliers"),
  Count = c(
    sum(sapply(edx, function(x) sum(is.na(x)))),
    nrow(duplicated_rows),
    nrow(inconsistent_titles),
    nrow(outlier_ratings)))

# Use kable to display the results
results |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

# Extracting the year and month

#Before extraction
edx |>
  select(timestamp) |>
  slice(1:5) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Extracting Year and Month from Timestamp from edx
edx$rating_date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
edx$rating_year <- format(edx$rating_date,"%Y")
edx$rating_month <- format(edx$rating_date,"%m")

edx$rating_year <- as.numeric(edx$rating_year)
edx$rating_month <- as.numeric(edx$rating_month)

#Extracting Year and Month from Timestamp from final_holdout_test set
final_holdout_test$rating_date <- as.POSIXct(final_holdout_test$timestamp, origin="1970-01-01")
final_holdout_test$rating_year <- format(final_holdout_test$rating_date,"%Y")
final_holdout_test$rating_month <- format(final_holdout_test$rating_date,"%m")

final_holdout_test$rating_year <- as.numeric(final_holdout_test$rating_year)
final_holdout_test$rating_month <- as.numeric(final_holdout_test$rating_month)

#After extraction:
edx |>
  select(rating_year,rating_month) |>
  slice(1:5) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


##Extracting Release year and Movie titles

#Before extraction
edx |>
  select(title) |>
  slice(1:5) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Extracting release years and movie titles from edx dataset
edx <- 
  edx |>
  mutate(title = str_trim(title)) |>
  extract(title,c("Temptitle", "release_year"), 
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) |>
  mutate(release_year = if_else(str_length(release_year) > 4,
                                as.integer(str_split(release_year, "-",simplify = T)[1]),as.integer(release_year))) |>
  mutate(title = if_else(is.na(Temptitle),title,Temptitle)) |>
  select(-Temptitle)

#Changing the datatype of the extracted release year
edx$release_year <- as.numeric(edx$release_year)

#Extracting release years and movie titles from final_holdout dataset
final_holdout_test <- 
  final_holdout_test |>
  mutate(title = str_trim(title)) |>
  extract(title,c("Temptitle", "release_year"), 
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) |>
  mutate(release_year = if_else(str_length(release_year) > 4,
                                as.integer(str_split(release_year, "-",simplify = T)[1]),as.integer(release_year))) |>
  mutate(title = if_else(is.na(Temptitle),title,Temptitle)) |>
  select(-Temptitle)

#Changing the datatype of the extracted release year
final_holdout_test$release_year <- as.numeric(final_holdout_test$release_year)

#After Extraction:
edx |>
  select(title,release_year) |>
  slice(1:5) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

## Extracting Genre

#Before extraction:
edx |>
  select(genres) |>
  slice(1:5) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)
edx <- 
  edx |>
  mutate(genre = fct_na_value_to_level(genres)) |>
  separate_rows(genre,sep = "\\|")

final_holdout_test <- 
  final_holdout_test |>
  mutate(genre = fct_na_value_to_level(genres)) |>
  separate_rows(genre,sep = "\\|")


#After extraction:
edx |>
  select(genre) |>
  slice(1:5) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

## Preprocessed Data

#Remove unnecessary columns from the datasets
edx <- 
  edx |>
  select(userId,movieId,rating,title,genre,release_year,rating_year,rating_month)

final_holdout_test <-
  final_holdout_test |>
  select(userId,movieId,rating,title,genre,release_year,rating_year,rating_month)

#preprocessed data
head(edx) |>
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

## Data Visualization 

#setting the no. of colors to expand the color palette  
nb.cols <- 25
my_cols <- colorRampPalette(brewer.pal(9,"Blues"))(nb.cols)

## Popularity of Movies

#Top 10 most rated movies
edx %>% 
  group_by(movieId) %>% 
  summarise(n_ratings = n()) %>% 
  arrange(desc(n_ratings)) %>% 
  top_n(10) %>% 
  ggplot(aes(x = reorder(movieId, n_ratings), y = n_ratings, fill = factor(movieId))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = my_cols) + 
  theme(panel.background = element_rect(fill = 'lightgray', color = 'lightblue')) +
  labs(title = "Top 10 Most Rated Movies", x = "Movie ID", y = "Number of Ratings")

## Distribution of rating 

edx |> 
  ggplot(aes(x = rating)) + 
  geom_histogram(binwidth = 0.5, fill= "lightblue",color = "black") + 
  labs(title = "Rating Distribution", x = "Rating", y = "Frequency")


## User Activity
edx |>
  group_by(userId) |>
  summarise(n_ratings = n()) |>
  arrange(desc(n_ratings)) |>
  top_n(10) |>
  ggplot(aes(x = reorder(userId, n_ratings), y = n_ratings, fill = factor(userId))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = my_cols) + 
  theme(panel.background = element_rect(fill = 'lightgray', color = 'lightblue')) +
  labs(title = "Top 10 Most Active Users", x = "User ID", y = "Number of Ratings")

```
## Genre Distribution 
edx |> 
  group_by(genre) |>
  summarise(n_movies = n()) |>
  arrange(desc(n_movies)) |>
  ggplot(aes(x = reorder(genre, n_movies), y = n_movies,fill = factor(genre))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_cols) + 
  theme(panel.background = element_rect(fill = 'lightgray', color = 'lightblue'),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Genre Distribution", x = "Genre", y = "Number of Movies")

#Genre distribution 
edx |> 
  group_by(genre) |>
  summarise(n_movies = n()) |>
  arrange(desc(n_movies)) |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

## Correlation of the Movie Rating 

edx |> 
  group_by(movieId) |>
  summarise(avg_rating = mean(rating)) |>
  ggplot(aes(x = avg_rating)) + 
  geom_histogram(binwidth = 0.1, fill = "lightblue", color= "black") + 
  labs(title = "Movie Rating Correlation", x = "Average Rating", y = "Frequency")


## Average Rating by Movie Genre

#Movie Genre vs. Average Rating
edx |>
  group_by(genre) |> 
  summarise(avg_rating = mean(rating)) |>
  ggplot(aes(x = genre, y = avg_rating,fill = factor(genre))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = my_cols) +
  theme(panel.background = element_rect(fill = 'lightgray', color = 'lightblue'),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Average Rating by Movie Genre", x = "Genre", y = "Average Rating")


# MODEL BUILDING ANG EVALUATION 


## Mean Baseline Model

# Function for building the model
mean_baseline_model <- function() {
  mean_rating <- mean(edx$rating)
  predictions <- rep(mean_rating, nrow(final_holdout_test))
  return(predictions)
}

# Train and test model
mean_baseline_pred <- mean_baseline_model()

# Evaluate models using RMSE
rmse_mean_baseline <- sqrt(mean((final_holdout_test$rating - mean_baseline_pred)^2))

  
## Movie-based Model

# Function for building the model
movie_based_model <- function() {
  movie_ratings <- edx %>%
    group_by(movieId) %>%
    summarise(mean_rating = mean(rating))
  
  predictions <- final_holdout_test %>%
    left_join(movie_ratings, by = "movieId") %>%
    pull(mean_rating)
  
  return(predictions)
}

# Train and test model
movie_based_pred <- movie_based_model()

# Evaluate model using RMSE
rmse_movie_based <- sqrt(mean((final_holdout_test$rating - movie_based_pred)^2))
  

## Movie + User based Model
  
  
# global mean rating
mu_hat <- mean(edx$rating)

# Calculate the average rating for each movie
movie_ratings <- 
  edx |>
  group_by(movieId) |>
  summarise(mean_rating = mean(rating - mu_hat))

# Calculate the average rating for each user
user_ratings <- 
  edx |>
  left_join(movie_ratings, by='movieId') |>
  group_by(userId) |>
  summarise(mean_ratingn = mean(rating - mu_hat - mean_rating))

# Make predictions on the final_holdout set
movie_user_based_model <- 
  final_holdout_test |>
  left_join(movie_ratings, by='movieId') |>
  left_join(user_ratings, by='userId') |>
  mutate(predicted_rating = mu_hat + mean_rating + mean_ratingn) %>%
  pull(predicted_rating)


# Train and test the model
movie_user_based_pred <- movie_user_based_model

# Evaluate model using RMSE
rmse_movie_user_based <- sqrt(mean((final_holdout_test$rating - movie_user_based_pred)^2))

## RMSE Comparison 
  
rmse_results <- data.frame(
  Models = c("Mean Baseline Model RMSE",
             "Movie-Based Model RMSE",
             "Movie + User-Based Model RMSE" ), 
  RMSE = c(rmse_mean_baseline, 
           rmse_movie_based,
           rmse_movie_user_based))

rmse_results |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


# REGULARIZATION

# Define a range of lambda values
lambdas <- seq(0, 10, 0.1)
```

## Regularized Movie-based Model

# Initialize a vector to store the RMSE values
rmses <- numeric(length(lambdas))

# Perform hyperparameter tuning
for (i in seq_along(lambdas)) {
  lambda <- lambdas[i]
  
  # Calculate the average rating for each movie, regularized by lambda
  movie_ratings <- edx %>%
    group_by(movieId) %>%
    summarise(mean_rating = sum(rating) / (n() + lambda))
  
  # Make predictions on the final_holdout_test set
  predictions <- final_holdout_test %>%
    left_join(movie_ratings, by = "movieId") %>%
    pull(mean_rating)
  
  # Calculate the RMSE on the final_holdout_test set
  rmse <- RMSE(final_holdout_test$rating, predictions)
  
  # Store the RMSE value
  rmses[i] <- rmse
}

# Plot the RMSE vs lambda values
rmse_lambda_value <- data.frame(RMSE = rmses, lambdas = lambdas)


rmse_lambda_value |>
  ggplot(aes(lambdas, rmses)) +
  theme_classic() +
  geom_point() +
  labs(title = "RMSEs vs Lambdas - Movie-Based Model",
       y = "RMSEs",
       x = "lambdas")


# Find the optimal lambda value
min_lambda <- lambdas[which.min(rmses)]

# Refit the model with the optimal lambda value
movie_ratings <- edx %>%
  group_by(movieId) %>%
  summarise(mean_rating = sum(rating) / (n() + min_lambda))

movie_based_model_regularized <- final_holdout_test %>%
  left_join(movie_ratings, by = "movieId") %>%
  pull(mean_rating)

rmse_movie_model_regularized <- sqrt(mean((final_holdout_test$rating - movie_based_model_regularized)^2))


## Regularized Movie + User based Model

# Initialize a vector to store the RMSE values
rmses <- numeric(length(lambdas))

# Perform hyperparameter tuning
for (i in seq_along(lambdas)) {
  lambda <- lambdas[i]
  
  # Calculate the global mean rating
  mu_hat <- mean(edx$rating)
  
  # Calculate the average rating for each movie, regularized by lambda
  movie_ratings <- 
    edx |>
    group_by(movieId) |>
    summarise(mean_rating = sum(rating - mu_hat) / (n() + lambda))
  
  # Calculate the average rating for each user, regularized by lambda
  user_ratings <- 
    edx |>
    left_join(movie_ratings, by='movieId') |>
    group_by(userId) |>
    summarise(mean_ratingn = sum(rating - mu_hat - mean_rating) / (n() + lambda))
  
  # Make predictions on the validation set
  predictions <- 
    final_holdout_test |>
    left_join(movie_ratings, by='movieId') |>
    left_join(user_ratings, by='userId') |>
    mutate(predicted_rating = mu_hat + mean_rating + mean_ratingn) %>%
    pull(predicted_rating)
  
  # Calculate the RMSE on the validation set
  rmse <- RMSE(final_holdout_test$rating, predictions)
  
  # Store the RMSE value
  rmses[i] <- rmse
}

# Plot the RMSE vs lambda values
rmse_lambda_value <- data.frame(RMSE = rmses, lambdas = lambdas)


rmse_lambda_value |>
  ggplot(aes(lambdas, rmses)) +
  theme_classic() +
  geom_point() +
  labs(title = "RMSEs vs Lambdas - Movie-User Based Model",
       y = "RMSEs",
       x = "lambdas")

# Find the optimal lambda value
min_lambda <- lambdas[which.min(rmses)]


# Refit the model with the optimal lambda value
mu_hat <- mean(edx$rating)

movie_ratings <- 
  edx |>
  group_by(movieId) |>
  summarise(mean_rating = sum(rating - mu_hat) / (n() + min_lambda))

user_ratings <- 
  edx |>
  left_join(movie_ratings, by='movieId') |>
  group_by(userId) |>
  summarise(mean_ratingn = sum(rating - mu_hat - mean_rating) / (n() + min_lambda))

movie_user_based_model_regularized <- 
  final_holdout_test |>
  left_join(movie_ratings, by='movieId') |>
  left_join(user_ratings, by='userId') |>
  mutate(predicted_rating = mu_hat + mean_rating + mean_ratingn) %>%
  pull(predicted_rating)

rmse_movie_user_model_regularized <- sqrt(mean((final_holdout_test$rating - movie_user_based_model_regularized)^2))



## RMSE after Regularization 

rmse_results_regularized <- data.frame(
  Regularized_Models= c("Movie-Based Model RMSE",
                        "Movie + User-Based Model RMSE" ), 
  RMSE = c(rmse_movie_model_regularized,
           rmse_movie_user_model_regularized))

rmse_results_regularized |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


#RESULTS
#The following table compares the RMSE before and after regularization 

RMSE_table <- data.frame(
  Models = c("Mean Baseline Model RMSE","Movie-Based Model RMSE", "Movie + User-Based Model RMSE"), 
  RMSE (Non-Regularized) = c(rmse_mean_baseline, rmse_movie_based, rmse_movie_user_based), 
  RMSE (Regularized) = c("NA", rmse_movie_model_regularized,
                         rmse_movie_user_model_regularized))

RMSE_table |> 
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


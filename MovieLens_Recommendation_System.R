##### Load the Libraries #####

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(ggplot2)
library(psych)


##### Pull Data from MovieLens Website ####

dl <- tempfile()  # create temporary file location
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl) # download dataset to temporary file location
ratings <- str_split_fixed(readLines(unzip(dl,"ml-10M100K/ratings.dat")), "\\::", 4)  # parse downloaded data to create ratings dataset
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")  # name columns
ratings <- as.data.frame(ratings) %>% # coerce dataset into data frame and...
  mutate(userId = as.numeric(userId), # ...coerce userId column into numeric data type and...
         movieId = as.numeric(levels(movieId))[movieId], # ...coerce movieId column into numeric data type and...
         rating = as.numeric(levels(rating))[rating], # ...coerce rating column into numeric data type and...
         timestamp = as.numeric(levels(timestamp))[timestamp])  # ...coerce timestamp column into numeric data type 
movies<- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3) # parse downloaded data to create movie dataset
colnames(movies) <- c("movieId", "title", "genres") # name columns
movies <- as.data.frame(movies) %>% # coerce dataset into data frame and...
  mutate(movieId = as.numeric(levels(movieId))[movieId], # ...coerce the movieId column into numeric data type and...
         title = as.character(title), # ...coerce title column into character data type and...
         genres = as.character(genres)) # ...coerce genre column into character data type
movielens <- left_join(ratings, movies, by = "movieId") # join ratings and movies data tables to create movielens dataset
rm(dl, movies, ratings) # remove unneeded variables in global environment

head(movielens)


##### Create Train (edx) and Test (validation) Datasets #####

set.seed(1) # set seed to 1
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE) # create index with 10% of data for test set
edx <- movielens[-test_index,]  # create edx (train) dataset from test index
temp <- movielens[test_index,]  # create temporary (test) dataset from test index
validation <- temp %>%  # create final validation (test) dataset from temporary dataset...
  semi_join(edx, by = "movieId") %>%  # ...by identifying movies...
  semi_join(edx, by = "userId") # ...and users that only appear in temporary test set and not in edx train set
removed <- anti_join(temp, validation)  # identify movies and users that were removed from temporary test set
edx <- rbind(edx, removed)  # add movies and users removed from temporary test set to edx train set
rm(test_index, temp, movielens, removed)  # remove unneeded data from global environment

head(edx) # display first six lines of data

head(validation)  # display first six lines of data


##### Data Cleaning #####

head(edx) # display first six lines of data

edx <- edx %>%  # take data and...
  mutate(timestamp = as_datetime(timestamp)) %>%  # ...coerce timestamp into date_time data type for better analysis and...
  mutate(year = substring(title, nchar(title) - 6)) %>% # ...pull year released from movie title and add it as new column and...
  mutate(year = as.numeric(substring(year, regexpr("\\(", year) + 1, regexpr("\\)", year) - 1))) %>%  # ...remove parenthesis and set as numeric and...
  mutate(yearsbetween = as.numeric(year(timestamp)) - year) %>%  # ...calculate time between movie's release and review and...
  separate_rows(genres, sep = "\\|")  # ...separate out rows with multiple genres into multiple duplicate rows
head(edx, 19) # display first 19 lines of data, which is the first 6 ratings with one line per genre

validation <- validation %>%  # take data and...
  mutate(timestamp = as_datetime(timestamp)) %>%   # ...coerce timestamp into date_time data type for better analysis
  mutate(year = substring(title, nchar(title) - 6)) %>% # ...pull year released from movie title and add it as a new column and...
  mutate(year = as.numeric(substring(year, regexpr("\\(", year) + 1, regexpr("\\)", year) - 1))) %>%  # ...remove parenthesis and set as numeric and...
  mutate(yearsbetween = as.numeric(year(timestamp)) - year) %>%  # ...calculate the time between release and review and...
  separate_rows(genres, sep = "\\|")  # ...separate out rows with multiple genres into multiple duplicate rows


##### Cursory Data Analysis and Visualizations #####

avg_rating <- mean(edx$rating)  # calculate average rating
med_rating <- median(edx$rating)  # calculate median rating

edx_ratings <- edx %>%  # take data and...
  group_by(rating) %>%  # ...group data by rating and...
  summarize(num_ratings = n()) %>% # ...summarize frequency of each rating and...
  arrange(desc(num_ratings)) # ...arrange data in descending order
edx_ratings # display rating frequencies
edx_ratings %>% # for each rating, plot frequency
  ggplot(aes(rating, num_ratings, color = rating)) +
  geom_point(aes(size = num_ratings)) +
  scale_color_gradientn(colours = rainbow(5)) +
  scale_size_continuous(limits = c(0, 7e+06)) +
  xlim(0,5) +
  labs(x = "Rating", y = "Number of Ratings", title = "Ratings by Rating", color = "Rating", size = "Number of Ratings")

edx_movies <- edx %>% # take data and...
  group_by(movieId) %>% # ...group by movie and...
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and...
  arrange(desc(num_ratings)) # ...arrange data in descending order
headTail(edx_movies)  # display top and bottom movies by number of ratings
edx_movies %>% # for each movie, plot the number of ratings v num of ratings
  ggplot(aes(movieId, num_ratings, color = avg_rating)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  labs(x = "MovieId", y = "Number of Ratings", title = "Ratings by Movie", color = "Average Rating")

edx_users <- edx %>% # take data and...
  group_by(userId) %>% # ...group by user and...
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and...
  arrange(desc(num_ratings)) # ...arrange data in descending order
headTail(edx_users) # display top and bottom users by number of ratings
edx_users %>% # for each movie, plot the number of ratings v num of ratings
  ggplot(aes(userId, num_ratings, color = avg_rating)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  labs(x = "UserId", y = "Number of Ratings", title = "Ratings by User", color = "Average Rating")

edx_genres <- edx %>% # take data and...
  group_by(genres) %>% # ...group data by genre and...
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and...
  arrange(desc(num_ratings)) # ...arrange data in descending order
edx_genres # display genre data
edx_genres %>% # for each genre, plot the number of ratings and average rating
  ggplot(aes(genres, avg_rating)) +
  geom_point(aes(size = num_ratings)) +
  scale_size_continuous(limits = c(0, 7e+06)) +
  labs(x = "Genre", y = "Average Rating", title = "Ratings by Genre", size = "Number of Ratings") +
  theme(axis.text.x = element_text(angle = 90))

edx_years <- edx %>% # take data and...
  group_by(year) %>% # ...group data by years between movie release and rating and...
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and...
  arrange(desc(num_ratings)) # ...arrange data in descending order
headTail(edx_years) # display year data
edx_years %>% # for each year, plot the number of ratings and average rating
  ggplot(aes(year, avg_rating, size = num_ratings)) +
  geom_point(aes(size = num_ratings)) +
  scale_size_continuous(limits = c(0, 7e+06)) +
  labs(x = "Year", y = "Average Rating", title = "Ratings by Year", size = "Number of Ratings")

edx_yearsbetween <- edx %>% # take data and...
  group_by(yearsbetween) %>% # ...group data by number of years between release and rating and...
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and...
  arrange(desc(num_ratings)) # ...arrange data in descending order
headTail(edx_yearsbetween) # display year data
edx_yearsbetween %>% # for each number of years between release and review, plot the number of ratings and average rating
  ggplot(aes(yearsbetween, avg_rating, size = num_ratings)) +
  geom_point(aes(size = num_ratings)) +
  scale_size_continuous(limits = c(0, 7e+06)) +
  labs(x = "Years between Movie Release and Rating", y = "Average Rating", title = "Ratings by Years between Release and Rating", size = "Number of Ratings")


##### Define Function to Calculate RMSE ######

rmse <- function(true_ratings, predicted_ratings){  # define function that takes true ratings and predicted ratings and...
  sqrt(mean((true_ratings - predicted_ratings)^2))  # ...calculates residual mean squared error
}


##### Model 1 - Average #####

average <- mean(edx$rating)  # calculate the average rating
rmse_average <- rmse(validation$rating, average) # calculate rmse for model
model_rmses <- tibble(model = "Average", rmse = rmse_average) # create a table to display all the calculated rmses

##### Model 2 - Movie Effect #####

movie_effect <- edx %>% # take data and...
  group_by(movieId) %>% # ...group it by movie and...
  summarize(e_m = mean(rating - average)) # calculate e_m for each movie by taking average of difference between each rating and average rating

movie_pred_ratings <- validation %>% # take testing dataset and...
  left_join(movie_effect, by ="movieId") %>% # ...join it to training data set by movie and...
  mutate(predicted_rating = average + e_m) %>% # ...calculate predicted ratings and...
  pull(predicted_rating)  # ...pull predicted ratings

rmse_movie_effect <- rmse(validation$rating, movie_pred_ratings)  # calculate rmse for model
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + Movie Effect", rmse = rmse_movie_effect)) # add calculated rmse to rmse table


##### Model 3 - User Effect #####

user_effect <- edx %>% # take data and...
  group_by(userId) %>%  # ...group by user and...
  summarize(e_u = mean(rating - average)) # ...calculate e_u for each user by taking average of difference between each rating and average rating

user_pred_ratings <- validation %>% # take test dataset and...
  left_join(user_effect, by = "userId") %>% # ...join it to training dataset by user and...
  mutate(predicted_rating = average + e_u) %>% #...calculate predicted ratings and...
  pull(predicted_rating)  # ...pull predicted ratings

rmse_user_effect <- rmse(validation$rating, user_pred_ratings)  # calculate rmse for model
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + User Effect", rmse = rmse_user_effect)) # add calculated rmse to rmse table


##### Model 4 - Genre Effect #####

genre_effect <- edx %>% # take data and...
  group_by(genres) %>% # ...group by genre and...
  summarize(e_g = mean(rating - average)) # calculate e_g for each genre by taking average of difference between each rating and average rating

genre_pred_ratings <- validation %>% # take test dataset and...
  left_join(genre_effect, by = "genres") %>%  # ...join it to training dataset by genre and...
  mutate(predicted_rating = average + e_g) %>% # ...calculate predicted ratings and...
  pull(predicted_rating)  # pull predicted ratings

rmse_genre_effect <- rmse(validation$rating, genre_pred_ratings)  # calculate rmse for model
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + Genre Effect", rmse = rmse_genre_effect)) # add calculated rmse to rmse table


##### Model 5 - Year Effect #####

year_effect <- edx %>% # take data and...
  group_by(year) %>% # ...group by year released and...
  summarize(e_y = mean(rating - average)) # calculate e_y for each year by taking average of difference between each rating and average rating

year_pred_ratings <- validation %>% # take test dataset and...
  left_join(year_effect, by = "year") %>% # join it to training dataset by time between release and ratings and...
  mutate(predicted_rating = average + e_y) %>% # ...calculate predicted ratings and...
  pull(predicted_rating)  # ...pull predicted ratings

rmse_year_effect <- rmse(validation$rating, year_pred_ratings)  # calculate rmse for model
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + Year Effect", rmse = rmse_year_effect)) # add calculated rmse to rmse table


##### Model 6 - Years between Effect #####

yearsbetween_effect <- edx %>% # take data and...
  group_by(yearsbetween) %>% # ...group by time between release and ratings and...
  summarize(e_yb = mean(rating - average)) # calculate e_y for each time frame by taking average of difference between each rating and average rating

yearsbetween_pred_ratings <- validation %>% # take test dataset and...
  left_join(yearsbetween_effect, by = "yearsbetween") %>% # join it to training dataset by time between release and ratings and...
  mutate(predicted_rating = average + e_yb) %>% # ...calculate predicted ratings and...
  pull(predicted_rating)  # ...pull predicted ratings

rmse_yearsbetween_effect <- rmse(validation$rating, yearsbetween_pred_ratings)  # calculate rmse for model
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + Years between Effect", rmse = rmse_yearsbetween_effect)) # add calculated rmse to rmse table


##### Model 7 - Movie Regularization ##### 

lambdas <- seq(0, 10, 0.25) # define a set of lambdas to test

reg_movie_rmses <- sapply(lambdas, function(l){ # calculate rmses for all defined lambdas by creating a function that...
  e_m <- edx %>% # ...takes data and...
    group_by(movieId) %>% # ...groups it by movie and...
    summarize(e_m = sum(rating - average) / (n() + l))  # calculates e_m then...
  predicted_ratings <- validation %>% # ...takes test dataset and...
    left_join(e_m, by = "movieId") %>% # ...joins it to train dataset by movie and...
    mutate(pred = average + e_m) %>% # ...calculates a predicted rating and...
    pull(pred)  # ...pulls predicted ratings and...
  return(rmse(validation$rating, predicted_ratings))  # ...returns rmses for each lambda
})

rmse_reg_movie_effect <- min(reg_movie_rmses) # return minimum rmse
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + Movie Effect + Regularization", rmse = rmse_reg_movie_effect)) # add calculated rmse to rmse table


##### Model 8 - Combine the Best Effects #####

model_rmses  # display calculated RMSES

movie_effect <- edx %>% # take data and...
  group_by(movieId) %>% # ...group it by movie and...
  summarize(e_m = mean(rating - average)) # calculate e_m for each movie by taking average of difference between each rating and average rating

movie_user_effect <- edx %>% # take data and...
  left_join(movie_effect, by = "movieId") %>% # join it to movie_effect dataset by movie and...
  group_by(userId) %>% # ...group by user and...
  summarize(e_u = mean(rating - average - e_m)) # ... calculate e_u for each user by taking average of difference between each rating and average rating and calculated e_m

movie_user_pred_ratings <- validation %>% # take test dataset and...
  left_join(movie_effect, by = "movieId") %>%  # join it to movie_effect dataset by movie and...
  left_join(movie_user_effect, by = "userId") %>% # join it to movie_effect dataset by movie and...
  mutate(predicted_rating = average + e_m + e_u) %>% # ...calculate predicted ratings and...
  pull(predicted_rating)  # ...pull predicted ratings

best_model_rmse <- rmse(validation$rating, movie_user_pred_ratings)  # calculate rmse for model
model_rmses <- bind_rows(model_rmses,
                         tibble(model = "Average + Movie + User Effects", rmse = best_model_rmse)) # add calculated rmse to rmse table
model_rmses[8,]



###########################################################################
#                                                                         #
#                    PREPARE THE DATA SET AND LIBRARIES                   #
#                                                                         #
###########################################################################


# Load required libraries
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(caret)
library(data.table)
library(gridExtra)
library(kableExtra)
library(knitr)
library(lubridate)
library(scales)
library(tidyverse)

options(digits=7)


#-------------------------------------------------------------------------#
# LOAD THE DATA SET                                                       #
#-------------------------------------------------------------------------#

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#---------->
# Add columns and review the data set
#---------->

# Adding rating year, scraping release year, and calculating differences between the two
edx <- edx %>% mutate(rating_y = year(as_datetime(timestamp)),                          
                      released = str_match(title, "\\(([0-9][0-9][0-9][0-9])\\)")[,2],
                      lapsed_y = as.integer(rating_y) - as.integer(released))

edx %>% select(-timestamp) %>% head(3) %>% kbl()   # quick visual sanity check
t(summary(edx)[c(1,3,4,6),]) %>% kbl()
                                       
#---------->
# Separate the training data set into a part for training and one for testing
#---------->

set.seed(47, sample.kind="Rounding")       # set seed to assure comparable outcomes between runs
train_index <- createDataPartition(edx$rating, times=1, p=0.1, list=FALSE)
train_set <- edx[-train_index,]
test_set <- edx[train_index,]

#---------->
# Define a function for model evaluation
#---------->

RMSE = function(observed, predicted){
  sqrt(mean((observed - predicted)^2))
}


###########################################################################
#                                                                         #
#                            EXPLORE THE DATA SET                         #
#                                                                         #
###########################################################################


#-------------------------------------------------------------------------#
# VISUALIZATION                                                           #
#-------------------------------------------------------------------------#


#---------->
# Only full-star and half-star ratings are allowed
#---------->

edx %>%
  ggplot(aes(rating)) + 
  geom_histogram(binwidth=0.5, fill=rep(c("#fc4628", "#3b3f40"), 5), color="white") + 
  ggtitle("The prevalence of ratings") +
  theme_minimal() +
  scale_x_discrete(limits=seq(0.5,5,0.5)) +
  scale_y_continuous(labels = label_comma())


#---------->
# Only full-star and half-star ratings are allowed
#---------->

# There are much more full-star than half-star ratings
edx %>% mutate(half_star=rating%%1!=0) %>% 
  group_by(half_star) %>%
  summarize(n=n())


xlabels <- sort(unique(edx$released))
xlabels[seq(2, length(xlabels), 5)] <- ""
xlabels[seq(3, length(xlabels), 5)] <- ""
xlabels[seq(4, length(xlabels), 5)] <- ""
xlabels[seq(5, length(xlabels), 5)] <- ""
xlabels[seq(6, length(xlabels), 10)] <- ""


#---------->
# Movies released more recently are more likely to be reviewed
#---------->

edx %>%
  group_by(released) %>%
  summarize(n=n_distinct(movieId)) %>%
  ggplot(aes(released, n)) +
  geom_col(fill="#3b3f40") +
  labs(x="year released", y="unique movies") +
  theme_minimal() +
  ggtitle("") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_discrete(labels = xlabels, breaks = as.integer(xlabels)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))


#---------->
# Older movies are on average more positively acclaimed but the this effect reverts prior to 1940
#---------->

edx %>%
  group_by(released) %>%
  summarize(ratings=mean(rating)) %>%
  ggplot(aes(as.Date(released, format="%Y"), ratings)) + 
  geom_point() +
  geom_smooth(method="loess", span=0.75, color="#53d1ee", alpha=0.20) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
  labs(x="year released")


#---------->
# Older movies are less likely to be reviewed with some movies rated before premiere
#---------->

lags <- edx %>%
  group_by(lapsed_y) %>%
  summarize(n=n_distinct(movieId), ratings=mean(rating)) %>%
  setNames(c("lapsed_y","n","ratings"))

colors <- c(rep("#fc4628",2), rep("#3b3f40", nrow(lags)-2))
lags_cor <- lags %>% filter(lapsed_y >= 0)
time_cor <- cor(lags_cor$lapsed_y, lags_cor$ratings)

lags %>%
  ggplot(aes(lapsed_y, n)) +
  geom_col(fill=colors) +
  scale_y_continuous(trans='log2') +
  labs(x="years lapsed", y="unique movies") +
  theme_minimal()

#---------->
# Average ratings increase with time passed from premiere up a point after which the trend reverts
#---------->

lags %>% ggplot(aes(lapsed_y, ratings)) +
  geom_point(color=c("#fc4628", "#fc4628", rep("#3b3f40", 94)), size=c(3, 3, rep(1, 94))) +
  geom_smooth(method="loess", span=0.75, color="#53d1ee", alpha=0.20) +
  labs(x="years lapsed") +
  theme_minimal()


#---------->
# Generally, the more widely known the movie is, the higher ratings it gets. Movies with many reviews receive better 
# ratings on average, which can mean that people tend to watch already popular flicks. The correlation though, is not high.
#---------->

edx %>% group_by(movieId) %>%
  summarize(n=n(), rating=mean(rating)) %>%
  cor() %>% .[2:3,2:3]



###########################################################################
#                                                                         #
#                          TEST AND COMPARE MODELS                        #
#                                                                         #
###########################################################################


#---------->
# The expected value (an average of all ratings) is the starting point for analysis
#---------->


mu <- mean(train_set$rating)

expected_value_rmse <- RMSE(test_set$rating, mu)             # Evaluate the model


#---------->
# There is one problem though. We only allow for half-star or full-start ratings. The final real-life application will
# require the outcome to be rounded to the nearest half.
#---------->

round_to_half <- function(x){round(x/0.5)*0.5}

#---------->
# a) Include the movie bias (distance from the overall mean for each movie average rating).
#---------->

movie_mu <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))                   # get distances from the overall average

linear_predictions <- test_set %>%
  left_join(movie_mu, by="movieId") %>%                # Join movie distances
  mutate(b_m = ifelse(is.na(b_m), 0, b_m)) %>%         # Make sure to fill NAs for movies not in the training set
  mutate(predict_by_movie=mu + b_m)                    # Predict by adding overall mean and movie bias

movie_bias_rmse <- RMSE(test_set$rating, linear_predictions$predict_by_movie)


#---------->
# b) Include the user bias (distance from the mean for each user's ratings average).
#---------->


user_mu <- train_set %>%
  left_join(movie_mu, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))

linear_predictions <- linear_predictions %>%
  left_join(user_mu, by="userId") %>%
  mutate(b_u = ifelse(is.na(b_u), 0, b_u)) %>%  
  mutate(predict_by_movie_and_user = mu + b_m + b_u,
         predict_by_user  = mu + b_u)

user_bias_rmse <- RMSE(test_set$rating, linear_predictions$predict_by_user)
user_and_movie_rmse <- RMSE(test_set$rating, linear_predictions$predict_by_movie_and_user)


#---------->
# c) Include the genre bias. To that end, check for the average per movie genre. First, split the label into columns (1), then transform it
# into a tidy format (2), and finally make a table with all averages per genre (e.g. instead of checking 5 categories for a 
# string "Comedy|Sci-Fi|Western||", treat each one separately).
# This function may take a while to execute.
#---------->


# create a table of genre biases 
genre_mus <- train_set %>%                                
  select(genres, rating) %>%                                                       #(1 - split into respective biases)
  separate(col=genres, sep="[//|]", into=c("1","2","3","4","5","6","7","8")) %>%   #(2 - make it tidy)
  pivot_longer(1:8, names_to="col_num", values_to="genre") %>%                     
  filter(!is.na(genre)) %>%
  group_by(genre) %>%                                                              # distance for each genre from the overall mean
  summarize(g_mu = mean(rating - mu))                     

check_genres_mu <- function(x){
  # This function takes in a row of data and checks for each genre in the genres column 
  # (e.g. Drama|Comedy|Thriller) to sum all the biases for those genres
  b <- sapply(genre_mus$genre, function(g){                        # for each genre
    bias <- as.numeric(ifelse(str_detect(x$genres, g),             # if present in string
                              genre_mus %>% filter(genre==g) %>%   # take bias
                                .$g_mu, 0)
    )
    bias                                                           # return all biases
  })
  rowSums(b)                                  # take the sum of all genres' biases for given movie                               
}

genre_mu <- check_genres_mu(test_set)         # apply the above function to the test set        


linear_predictions <- linear_predictions %>%
  mutate(b_g = genre_mu) %>%
  mutate(b_g = ifelse(is.na(b_g), 0, b_g)) %>%
  mutate(predict_by_genres_user_movie = mu + b_u + b_m + b_g)

individual_genres_rmse <- RMSE(test_set$rating, linear_predictions$predict_by_genres_user_movie)

#---------->
# d) The above approach is computation-heavy, so try a similar but simpler approach without splitting individual genres
# from the combination of all genres (e.g. instead of checking 5 categories for a string "Comedy|Sci-Fi|Western||", 
# treat it as a single unique combination with its own respective bias)
#---------->


genres_mu <- train_set %>%
  left_join(movie_mu, by="movieId") %>%   
  left_join(user_mu, by="userId") %>%  
  group_by(genres) %>%
  summarize(b_gs = mean(rating - mu - b_m - b_u))

linear_predictions <- linear_predictions %>%
  left_join(genres_mu, by="genres") %>%
  mutate(b_gs = ifelse(is.na(b_gs), 0, b_gs)) %>%
  mutate(predict_by_genres = mu + b_gs,
         predict_by_user_movie_genres  = mu + b_m + b_u + b_gs)

genres_bias_rmse <- RMSE(test_set$rating, 
                         linear_predictions$predict_by_genres)
genres_user_movie_rmse <- RMSE(test_set$rating, 
                               linear_predictions$predict_by_user_movie_genres)

#---------->
# The result for genres combinations is better then for individual genres.
#---------->


#-------------------------------------------------------------------------#
# REGULARIZATION                                                          #
#-------------------------------------------------------------------------#


#---------->
# Some movies have many more ratings than others. This distorts biases because movies with fewer ratings 
# tend to get more extreme scores. Likewise, users with fewer movies graded tend to give more extreme scores. I will,
# therefore regularize these biases with additional parameter to penalize instances with low number of observations.
# To that end, I will test several lambda values to look for the lowest RMSE. I will start with the movie bias.
#---------->

lambda <- seq(2,8, 0.25)   

tune_lambda <- sapply(lambda, function(l){
  # This function will apply given lambda to movie bias and check RMSE for predictions
  movie_mu <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n() + l))       # test bias with a given lambda
  
  user_mu <- train_set %>%
    left_join(movie_mu, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_m)/(n() + l))
  
  genres_mu <- train_set %>%                                 
    left_join(movie_mu, by="movieId") %>%
    left_join(user_mu, by="userId") %>%
    group_by(genres) %>%
    summarize(b_gs = sum(rating - mu - b_m - b_u)/(n() + l))      
  
  predictions <- test_set %>%
    left_join(movie_mu, by="movieId") %>%
    left_join(user_mu, by="userId") %>%
    left_join(genres_mu, by="genres") %>%
    replace(is.na(.), 0) %>%
    mutate(predict = mu + b_u + b_m + b_gs)
  
  RMSE(test_set$rating, predictions$predict)
})

data.frame(l=lambda, rmse = tune_lambda) %>%           # plot the results
  ggplot(aes(l, rmse)) + 
  geom_point(color="#53d1ee", size=3) + 
  geom_text(aes(label=l), vjust = -2, size = 4)


regularized_genres_user_movie_rmse <- min(tune_lambda)

#-------------------------------------------------------------------------#
# PICK THE FINAL MODEL                                                    #
#-------------------------------------------------------------------------#

results <- tibble(method=c("Expected value", "Movie", "User", "User and Movie", 
                           "Genres", "Genre & User & Movie", "Individual Genres & User & Movie",
                           "Regularized Genre & User & Movie"),
                  RMSE=c(expected_value_rmse, movie_bias_rmse, user_bias_rmse, user_and_movie_rmse, 
                         genres_bias_rmse, genres_user_movie_rmse, individual_genres_rmse,
                         regularized_genres_user_movie_rmse))  


options(pillar.sigfig = 7)

#---------->
# The best one is the regularized model taking into account user, movie, and genres biases.
#---------->

results %>% arrange(RMSE) %>% kbl()


###########################################################################
#                                                                         #
#                     PICK AND EVALUATE THE FINAL MODEL                   #
#                                                                         #
###########################################################################

# Benchmarks:
# 10 points: 0.86550 <= RMSE <= 0.89999
# 15 points: 0.86500 <= RMSE <= 0.86549
# 20 points: 0.86490 <= RMSE <= 0.86499
# 25 points: RMSE < 0.86490
#                   0.8645253

#---------->
# Validate the final model against the validation set.
#---------->

FINAL_MODEL <- function(train_set, test_set, l=4.5){
  # This function will apply given lambda to biases and check RMSE for predictions
  movie_mu <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n() + l)) 
  
  user_mu <- train_set %>%
    left_join(movie_mu, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_m)/(n() + l))
  
  genres_mu <- train_set %>%                                 
    left_join(movie_mu, by="movieId") %>%
    left_join(user_mu, by="userId") %>%
    group_by(genres) %>%
    summarize(b_gs = sum(rating - mu - b_m - b_u)/(n() + l))      
  
  predictions <- test_set %>%
    left_join(movie_mu, by="movieId") %>%
    left_join(user_mu, by="userId") %>%
    left_join(genres_mu, by="genres") %>%
    replace(is.na(.), 0) %>%
    mutate(predict = mu + b_u + b_m + b_gs) %>%
    pull(predict)
  
  RMSE(test_set$rating, predictions)
}


final_rmse <- FINAL_MODEL(train_set, validation)

#---------->
# The 25 points benchmark has been exceeded.
#---------->

final_rmse < 0.86490






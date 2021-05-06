######### Title: Movie Recommendation - HarvardX CapStone Project 1
### Codes supplied by the HarvardX program to Create edx set (split into train.set and test.set), validation set (final hold-out test set)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggthemes)
library(scales)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)



ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           #title = as.character(title),
                                           #genres = as.character(genres))
# if using R 4.0 or later:
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

#Here, the edx which is 90% of the MovieLens Data has 9000047 observations and 6 variables.
#the edx set will further be partitioned into train and test sets
#the validation set wich is 10% of the movielens data has 999999 observations and 6 variables

############## <<<<<<<<< SPLIT EDX set to TRAIN and TEST SET >>>>>>>>> #########################
#We will be using 80% of the edx set as the train.set and 20% as test set
#This is to ensure there is enough data for both training and testing

set.seed(1, sample.kind="Rounding")  
testindex <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train.set <- edx[-testindex,]
temp1 <- edx[testindex,]

#The partitioned train and test sets from the edx dataset contains 7200037 and 1800010 observations respectively.
#they both have 6 variables

names(edx)

# ensure that userId and movieId in test.set are also in train.set
test.set <- temp1 %>% 
  semi_join(train.set, by = "movieId") %>%
  semi_join(train.set, by = "userId")

# Add rows removed from test.set set back into train set
removed_obs <- anti_join(temp1, test.set)
train.set <- rbind(train.set, removed_obs)

rm(temp1, testindex, removed_obs)

#######EXPLORATORY DATA ANALYSIS ###########
#To describe and understand the data so as to build a better model, 
#it is important to understand the characeristics of all the variables by exploring it.

names(train.set)

#There 6 variables in the train.set data, they are:
#"userId"    "movieId"   "rating"    "timestamp" "title"     "genres"

head(train.set)

#Structure of the train.set data

str(train.set)

#There are 7200074 observations in the train.set
#From the result,movieId is numeric whereras movieId is integer.
#Convert movieId from numeric to integer.

train.set$movieId <- as.integer(train.set$movieId)

#To check the Dimension of the train.set data.

dim(train.set)

#Summary

summary(train.set)

#The summary of rating:
#         rating
#     Min.      :0.500
#     1st Qu.   :3.000
#     Median    :4.000
#     Mean      :3.512
#     3rd Qu.   :4.000
#     Max.      :5.000

#Numbers of ratings greater than 3
sum(train.set$rating > 3)
4237538

#Numbers movies available for rating
length(unique(train.set$movieId))
10669

#Checking Genres Variable:
#Numbers of Genres available without repetition
length(unique(train.set$genres))
797

#Number of people (userid) without repetition
length(unique(train.set$userId))
69878


#Exploring the data by Genres

#From the above estimate of number of genres, there are 797 genres categories.
#To check the frequency of each genre in the edx dataset, the code below is executed.

train.set %>% group_by(genres) %>%
  summarise(Freq=n()) %>%
  head()

#######>>>> Exploring the edx data through the date variable

str(edx$timestamp)

#For the range of dates, that is, starting date when the data was collected to the most recent date.

## Exploring Throught The Date variable
#From the summary output above, the minimum from the time stamp summary (which is also the date) was 1995-01-09 while the maximum (that is, the end date) was on 2009-01-05. The interval, that is, how long the rating exercise lasted can be arrived at as:

interval_in_days = difftime(max(train.set$timestamp), min(train.set$timestamp), units = "days")
interval_in_years = as.double(interval_in_days)/365 # absolute years
interval_in_years


## Movie Rating Distribution Across years


edx %>% mutate(rating_year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=rating_year)) +
  geom_histogram(color = "red") +
  ggtitle("Rating Distribution Across Years") +
  xlab("Rating Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) +
  theme_economist()



#From the graph above, more people participated in the rating exercise in year 2000. The year 1998 had the least participation in the movies rating exercise.

## Exploring the ratings from the EDX Dataset

#Without using any model, we can use frequencies to explore the top 10 of the movies with highest number of rating.


numRatings <- edx %>% group_by(movieId) %>% 
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings)
numRatings





## Exploring Dates by Ratings
#The frequencies of movie ratings per date is given with the following codes:
  

edx %>% mutate(date_rating = date(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(date_rating, title) %>%
  summarise(freq = n()) %>%
  arrange(-freq) %>%
  head(5)



# Rank the 10 levels of different ratings

#The possible ratings are from 0.5 to 5.0 with an interval of 0.5. In this section, we are displaying number of times each rating was made used of.


rankRatings <- edx %>% group_by(rating) %>% 
  summarize(freq = n())
rankRatings

#The rating 4.0 was most given to movies followed by rating 3.0 with frequencies of 2,588,430 and 2,121,422 respectively. The level 4 rating was assigned the most for the movies, followed by 3, 5, 3.5 and lastly 2 was the least of the top 5

#The output above can be presented in a graphical format:
 
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels= trans_format("log10", math_format(10^.x)))+
  ggtitle("Distribution of Rating Levels", subtitle = "Most movies were Rated High.")+
  xlab("Rating")+
  ylab("Count")+
theme_economist()


## Exploring the movies variable

#It has been earlier established that there are 10677 movies in the EDX dataset. The ratings range from 0.5 to 5 with an interval of 0.5.


## The number of movies is given as:

#The number of unique movies, without repeatition is given as:

length(unique(edx$movieId))




## Distribution of movies

#To check if the movies ratings follow a normal distribution using the histogram: 
  
 
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bin = 30, color = "cyan") +
  scale_x_log10() + 
  ggtitle("Movies Distribution",
          subtitle = "The distribution is approximately normal") +
  xlab("Frequency of Ratings")+
  ylab("Frequency of Movies")+
  theme_economist()




## Exploring the 'userid' Variable in the EDX set

#The users variable uniquely identifies those rating the movies.


length(unique(edx$userId))


#There are 69878 distinct users in the edx dataset. To check the distribution of the userid variable, observing the first six for the display of userid and how many times they appear in the data.


edx %>% group_by(userId) %>%
  summarise(freq=n()) %>%
  arrange(freq) %>%
  head()

## Graphical Inspection of users that have rated movies


edx %>% group_by(userId) %>%
  summarise(frq=n()) %>%
  ggplot(aes(frq)) +
  geom_histogram(bin = 30, color = "cyan") +
  scale_x_log10() + 
  ggtitle("Distribution of Users in the Data")



## Data Preparation and Methodology

#The objective of this analysis is to predict user rating with the feature variables. This work, due to lack of sufficient computer capacity, efficiency and speed, I used the userid and movieid to predict the rating of movies. Therefore I subset only the needed variables and let go off the timestamp and genre.


train.set <- train.set %>% select(userId, movieId, rating, title, genres)
test.set <- test.set %>% select(userId, movieId, rating, title, genres)
validation <- validation%>% select(userId, movieId, rating, title, genres)


#The genre variable is a character type, this will not work with our model fitting until converted to factor.
#To convert character to factor, the codes below were used:
  

train.set$genres<- as.factor(train.set$genres)
test.set$genres<- as.factor(test.set$genres)
validation$genres<- as.factor(validation$genres)


#Now, to confirm that the genres variables in the three datasets have been changed from character to factor:
  

str(train.set$genres)
str(test.set$genres)
str(validation$genres)




#The RMSE is systematically derived through the Mean Absolute Error (MAE) and Mean Squared Error (MSE) below:
  
## Define Mean Absolute Error (MAE)

MAE <- function(true_ratings, predicted_ratings){
    mean(abs(true_ratings - predicted_ratings))
}

## Define Mean Squared Error (MSE)
MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings - predicted_ratings)^2)
}


#### Define Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Samller errors yield smaller MAE, MSE and RMSE and larger error yields otherwise proportionately

#Model1 - Linear Model

#Runing the *lm* function was not successful as the computer used does not have the computing capacity and storage to perform such large operation. 

#Therefore, to get around this and fix the Linear prediction model, each of the model parameters will be built individually: the *random prediction*, the *user bias*, the *movie bias* and finally all put together to form the linear model.

#Random Prediction

set.seed(1952, sample.kind = "Rounding")
# Create the probability of each rating
p <- function(x, y) mean(y == x)
rating <- seq(0.5,5,0.5)

# Estimate the probability of each rating with Monte Carlo simulation
B <- 10^3
M <- replicate(B, {
  s <- sample(train.set$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))
# Predict random ratings
y_hat_random <- sample(rating, size = nrow(test.set),
                       replace = TRUE, prob = prob)
# Create a table with the error results
result <- tibble(Method = "Project Goal", RMSE = 0.8649, MSE = NA, MAE = NA)
result <- bind_rows(result,
                    tibble(Method = "Random prediction",
                           RMSE = RMSE(test.set$rating, y_hat_random),
                           MSE = MSE(test.set$rating, y_hat_random),
                           MAE = MAE(test.set$rating, y_hat_random)))
# Show the RMSE improvement
result %>% knitr::kable(caption = "RMSE Display of Models")


#While the project goal is an RMSE of 0.864900, the random prediction has shown not to be sufficient for this data to predict a trustworthy output. The random prediction has an RMSE of 1.5007061, this is a problem of over fitting.

#The mean rating will also be used to model this data, if it would give a bettwe RMSE. The mean rating, that is, the slope of the linear model


# The initial prediction is the mean of the ratings (mu).
# y_hat = mu
# Mean of observed values
mu <- mean(train.set$rating)
# Update the error table
result <- bind_rows(result,
                    tibble(Method = "Mean",
                           RMSE = RMSE(test.set$rating, mu),
                           MSE = MSE(test.set$rating, mu),
                           MAE = MAE(test.set$rating, mu)))
# Show the RMSE improvement
result %>% knitr::kable(caption = "RMSE Display of Models")


#Although using the mean prediction model gives a better RMSE (1.059904) value than the random prediction (1.502071), it still has the overfitting attribute which is not acceptable as a good prediction accuracy. The mean prediction can be improved towards forming the linear model. This mean prediction has not taken into consideration the effect of the movieid nor the userid. Accounting for both parameters in the linear model should result in a better RSME estimate.


## Include movie effect (b_i)

#This partial model accounts for only the movie effect:

  

# bi is the movie effect (bias) for movie i.
# y_hat = mu + bi
# Movie effects (bi)

bi <- train.set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
head(bi)



# Plot the distribution of movie effects

bi %>% ggplot(aes(x = b_i)) +
  geom_histogram(bins=10, col = I("cyan")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie effect") +
  ylab("Count") +
  scale_y_continuous(labels = comma) +
  theme_economist()
# Predict the rating with mean + bi
y_hat_bi <- mu + test.set %>%
  left_join(bi, by = "movieId") %>%
  .$b_i
# Calculate the RMSE
result <- bind_rows(result,
                    tibble(Method = "Mean + bi",
                           RMSE = RMSE(test.set$rating, y_hat_bi),
                           MSE = MSE(test.set$rating, y_hat_bi),
                           MAE = MAE(test.set$rating, y_hat_bi)))

# Show the RMSE improvement
result %>% knitr::kable(caption = "RMSE Display of Models")



#Adding the movie effect further reduces or improves the RMSE towards the project RMSE goal. Accounting for the movie effect, the RMSE of the linear model becomes 0.9437429 which is still greater than the project goal RMSE of 0.864900.

## Linear Model (movie & user bias)

#To further improve the linear model, the user effect (bu) is accounted for.


bu <- train.set %>%
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
# Prediction
y_hat_bi_bu <- test.set %>%
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Plot the distribution of user effects
train.set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(color = "black") +
  ggtitle("User Effect Distribution") +
  xlab("User Bias") +
  ylab("Count") +
  scale_y_continuous(labels = comma) +
  theme_economist()

# Update the results table
result <- bind_rows(result,
                    tibble(Method = "Mean + bi + bu",
                           RMSE = RMSE(test.set$rating, y_hat_bi_bu),
                           MSE = MSE(test.set$rating, y_hat_bi_bu),
                           MAE = MAE(test.set$rating, y_hat_bi_bu)))
# Show the RMSE improvement
result %>% knitr::kable()

#The addition of both user effect and movie effect further improved the RMSE (0.8659319) to a great extent when compared with the project RMSE goal of (0.8649000). The linear model is hence completed with a full model fitting approximately close to the project goal.


#It can be observed that the addition of movie effect and user further improved the RMSE towards the project RMSE goal. Accounting for the genre effect, the RMSE of the linear model should hopefully become more improved.





# Model 3 - Regularization
#When the movie and user effects were regularized, the RMSE estimate is indeed further improved to 0.86500 which is closer to the Project RMSE goal, 0.8649000.


regularization <- function(lambda, trainset, testset){
  
  # Mean
  mu <- mean(trainset$rating)
  
  # Movie effect (bi)
  b_i <- trainset %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  # User effect (bu)  
  b_u <- trainset %>% 
    left_join(b_i, by="movieId") %>%
    filter(!is.na(b_i)) %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  # Prediction: mu + bi + bu  
  predicted_ratings <- testset %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    filter(!is.na(b_i), !is.na(b_u)) %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, testset$rating))
}

# Define a set of lambdas to tune
lambdas <- seq(0, 10, 0.25)

# Update RMSES table
rmses <- sapply(lambdas, 
                regularization, 
                trainset = train.set, 
                testset = test.set)

# Plot the lambda x RMSE
tibble(Lambda = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Regularization", 
          subtitle = "Pick the penalization that gives the lowest RMSE.") +
  theme_economist()



#Now, the lambda that gives the smallest RMSE is applied to the linear model to 'regularize' it:
  
  
# We pick the lambda that returns the lowest RMSE.
lambda <- lambdas[which.min(rmses)]

# Then, we calculate the predicted rating using the best parameters 
# achieved from regularization.  
mu <- mean(train.set$rating)

# Movie effect (bi)
b_i <- train.set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User effect (bu)
b_u <- train.set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Prediction
y_hat_reg <- test.set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Update the result table
result <- bind_rows(result, 
                    tibble(Method = "Regularized bi and bu", 
                           RMSE = RMSE(test.set$rating, y_hat_reg),
                           MSE  = MSE(test.set$rating, y_hat_reg),
                           MAE  = MAE(test.set$rating, y_hat_reg)))

# Regularization made a small improvement in RMSE.  
result



#The Regularization of the linear model gave approximately the same RMSE value than the project RMSE.


## Model 4 

##Matrix Factorization with recosystem
#The Matrix Factorization process is conducted as below:
  
  
  
if(!require(recosystem))
  install.packagesif(!require(recosystem))
install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(1234, sample.kind = "Rounding") # This is a randomized algorithm
# Convert the train and test sets into recosystem input format
train_data <- with(train.set, data_memory(user_index = userId,
                                          item_index = movieId,
                                          rating = rating))
test_data <- with(test.set, data_memory(user_index = userId,
                                        item_index = movieId,
                                        rating = rating))

# Convert validation sets to recosystem input format

validation_reco <- with(validation, data_memory(user_index = userId,
                                                item_index = movieId,
                                                rating = rating))



# Create the model object
r <- recosystem::Reco()

# identifying the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30),
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1),
                                       costq_l2 = c(0.01, 0.1),
                                       nthread = 4, niter = 10))
# Train the algorithm
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

# using the model for prediction
y_hat_reco <- r$predict(test_data, out_memory())
head(y_hat_reco, 10)

# validating the model predictions
y_hat_final_reco <- r$predict(test_data, out_memory())

#compute the results table from start to finish
result <- bind_rows(result,
                    tibble(Method = "Matrix Factorization using recosystem",
                           RMSE = RMSE(test.set$rating, y_hat_reco),
                           MSE = MSE(test.set$rating, y_hat_reco),
                           MAE = MAE(test.set$rating, y_hat_reco)))
result


## Validation of Selected model
#Applying the validation set to the Regularization and the matrix factorization model:
  
  ### Validation of the linear model Regularization
  
  
#running the model training again on the edx set

mu_edx <- mean(edx$rating)

# Edx Movie bias (bi)
b_i_edx <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

# Edx User bias (bu)
b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

# Prediction using validation
y_hat_edx <- validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  pull(pred)

# Update the results table
result <- bind_rows(result, 
                    tibble(Method = "Validating Regularized Linear Model", 
                           RMSE = RMSE(validation$rating, y_hat_edx),
                           MSE  = MSE(validation$rating, y_hat_edx),
                           MAE  = MAE(validation$rating, y_hat_edx)))

# RMSE of the validation is:
result 



#The RMSE calculated using the `validation` set on the edx set (`r RMSE(validation$rating, y_hat_edx)`) is found to be approximately the target of 0.8649, this implies that the model is consistent.


### Validating the Matrix Factorization Model
#The entire `edx` is transformed to a reco dataset and `validation` set is tested on it to see if the RMSE is consistently below the project RMSE goal.

 
set.seed(1234, sample.kind = "Rounding")

# Transforming the 'edx' and 'validation' sets to recosystem datasets
edx.reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation.reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Creating the reco model object
r <-  recosystem::Reco()

# Parameter Tuning
opts <-  r$tune(edx.reco, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Model validation Training
r$train(edx.reco, opts = c(opts$min, nthread = 4, niter = 20))

# Making the Prediction with validation set
y_hat_reco_valid <-  r$predict(validation.reco, out_memory())

# Update the result table
result <- bind_rows(result, 
                    tibble(Method = "Final Matrix Factorization - Validation", 
                           RMSE = RMSE(validation$rating, y_hat_reco_valid),
                           MSE  = MSE(validation$rating, y_hat_reco_valid),
                           MAE  = MAE(validation$rating, y_hat_reco_valid)))
result


#The validated RMSE from matrix factorization is 0.783 which is better than the initial models' RMSE and project RMSE goal because it tends towards zero.

#The Matrx Factorization method is hence the most appropriate model for the Movielens recommendation system.















  





  














































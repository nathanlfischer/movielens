##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

#load libraries
library(rafalib)
library(lubridate)
library(knitr)

################################### Explore Data Section ################################################

#EDA - basic information
dim(edx)
str(edx)
head(edx)
summary(edx)

#EDA - title column includes release year - make two separate columns - add date, weekday, and week column
edx <- edx %>% 
  extract(col=title,into=c("title","year"),regex="^(.*)\\((\\d{4})\\)$") %>%
  mutate(year=as.integer(year),
         date = as_datetime(timestamp),
         weekday = wday(date,label=TRUE),
         week = round_date(date, unit = "week"))

#EDA - number of distinct users and movies
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#EDA - plot 200 users, 200 movies, dot for vote
users <- sample(unique(edx$userId), 200)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 200)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:200, 1:200,. , xlab="Movies", ylab="Users")
abline(h=0:200+0.5, v=0:200+0.5, col = "grey")

#EDA - distribution of movie votes and user votes
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movie Ratings")

edx %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("User Ratings")

#EDA - counts for each rating/score
edx %>%
  ggplot(aes(rating)) +
  geom_bar()

#EDA -  movies with most and least ratings
edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) 
  
edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  arrange(count)

#EDA - movies with best and worst rating - at least n votes
edx %>%
  group_by(title) %>%
  summarize(n=n(), 
            average=mean(rating)) %>%
  filter(n>1000) %>%
  arrange(desc(average)) 
edx %>%
  group_by(title) %>%
  summarize(n=n(), 
            average=mean(rating)) %>%
  filter(n>1000) %>%
  arrange(average)


#EDA - number of reviews and average rating for each genre
##multi-genre
### number of grouped genres
edx %>% 
  summarize(n_genres = n_distinct(genres))
### number of reviews
edx %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  filter(n>1000) %>%
  arrange(desc(n)) %>%
  top_n(25,n)
edx %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  filter(n>1000) %>%
  arrange(n) %>%
  top_n(25,n)
### average rating
edx %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg_rating)) %>%
  ggplot(aes(x = genres, y = avg_rating, ymin = avg_rating - 2*se, ymax = avg_rating + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##single genre
### number of reviews
edx %>%
  separate_rows(genres, sep="\\|") %>%
  group_by(genres) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
### average rating
edx %>%
  separate_rows(genres, sep="\\|") %>%
  group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg_rating)) %>%
  ggplot(aes(x = genres, y = avg_rating, ymin = avg_rating - 2*se, ymax = avg_rating + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#EDA - number of movies released per year
edx %>%
  select(movieId,year) %>%
  distinct() %>%
  group_by(year) %>%
  summarize(count =n()) %>%
  filter(year>=1920) %>%
  arrange(year) %>%
  ggplot(aes(year,count)) +
  geom_point() +
  geom_smooth(method='loess', span=0.12)

#EDA - median # ratings by year of movie release
edx %>%
  group_by(movieId,year) %>%
  summarize(n_ratings = n()) %>%
  group_by(year) %>%
  summarize(median_n = median(n_ratings)) %>%
  ggplot(aes(y=median_n,x=year)) +
  geom_point() +
  geom_text(aes(label=year), nudge_y=0.1)

#EDA - avg rating vs week - some evidence of time effect
edx %>%
  group_by(week) %>%
  summarize(average = mean(rating)) %>%
  ggplot(aes(y=average, x=week)) +
  geom_point() +
  geom_smooth()

#EDA - Top 5 rated movie avg rating over time
top <- edx%>%
  group_by(movieId) %>%
  summarize(n=n(),average=mean(rating)) %>%
  filter(n>1000) %>%
  arrange(desc(average)) %>%
  top_n(5,average) %>%
  pull(movieId)
edx %>%  
  filter(movieId %in% top) %>%
  group_by(title,week) %>%
  summarize(average=mean(rating)) %>%
  ggplot(aes(y=average, x=week, color=title)) +
  geom_point() +
  geom_smooth()

#EDA - avg rating vs month - some evidence of time effect
edx %>%
  mutate(month = round_date(date, unit = "month")) %>%
  group_by(month) %>%
  summarize(average = mean(rating)) %>%
  ggplot(aes(y=average, x=month)) +
  geom_point() +
  geom_smooth()

#EDA - day of the week effect
##number of ratings vs day of week
edx %>%
  group_by(weekday) %>%
  summarize(n=n()) %>%
  ggplot(aes(day,n)) +
  geom_point()
edx %>%
  group_by(weekday) %>%
  ggplot(aes(weekday)) +
  geom_bar()
##avg rating vs day of week
edx %>%
  group_by(weekday) %>%
  summarize(avg_rating=mean(rating)) %>%
  ggplot(aes(weekday,avg_rating)) +
  geom_point()


#EDA - post 1980, 10 movies with most rating per year with avg rating
edx %>%
  filter(year>=1980) %>% 
  group_by(title) %>%
  summarize(rating_per_year = n()/(2018-first(year)),
            avg_rating = mean(rating)) %>%
  arrange(desc(rating_per_year)) %>%
  top_n(10,rating_per_year)

#EDA - avg rating vs # ratings per year
edx %>%
  filter(year>=1993) %>% 
  group_by(title) %>%
  summarize(rating_per_year = n()/(2018-first(year)),
            avg_rating = mean(rating)) %>%
  mutate(rating_per_year = round(rating_per_year, digits=0)) %>%
  group_by(rating_per_year) %>%
  summarize(average = mean(avg_rating)) %>%
  ggplot(aes(y=average,x=rating_per_year)) +
  geom_point()

#EDA - rating frequency - no of ratings per user per week/month
edx %>%
  group_by(userId,week) %>%
  summarize(freq=n(),
            average=mean(rating)) %>%
  ggplot(aes(y=average, x=freq)) +
  geom_point() +
  geom_smooth()

edx %>%
  mutate(month = round_date(date, unit = "month")) %>%
  group_by(userId,month) %>%
  summarize(freq=n(),
            average=mean(rating)) %>%
  ggplot(aes(y=average, x=freq)) +
  geom_point() +
  geom_smooth()


#EDA - Hierarchical clustering
top <- edx %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(100, n) %>% #only 100 most popular titles
  pull(movieId)
x <- edx %>% 
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>% #more than 25 votes
  ungroup() %>% 
  select(title, userId, rating) %>%
  spread(userId, rating)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

#clusters of movie titles by rating
d <- dist(x)
h <- hclust(d)
plot(h, cex = 0.65, main = "", xlab = "")
groups <- cutree(h, k = 10) #create 10 groups
map(seq(1:10),~{names(groups)[groups==.x]})

#EDA - k-means clustering
x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 10, nstart=25)
groups <- k$cluster
split(names(groups), groups)

############################ Data Prep for Modeling ###################################

#partition of data for modeling
library(caret)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
rm(test_index, temp, removed)

#create RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


################################# Modeling Section #######################################
##Simple - average of all ratings
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% kable(digits=5)

##movie effects
mu <- mean(train_set$rating) 
b_i <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
qplot(b_i, data = b_i, bins = 10, color = I("black")) #histogram of b_i

predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% kable(digits=5)

##user effects
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% kable(digits=5)


#Model - Genre (grouped) effects
b_g <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre (grouped) Effect Model",  
                                 RMSE = model_3_rmse ))
rmse_results %>% kable(digits=5)

#Model - Genre (ungrouped) effects
b_g2 <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  separate_rows(genres, sep="\\|") %>%
  group_by(genres) %>%
  summarize(b_g2 = mean(rating - mu - b_i - b_u))
predicted_ratings <- test_set %>% 
  separate_rows(genres, sep="\\|") %>%
  left_join(b_g2, by="genres") %>%
  group_by(movieId,userId) %>%
  summarize(b_g_sum = sum(b_g2)) %>%
  #opposite of separate_rows and sum b_gs
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%  
  mutate(pred = mu + b_i + b_u + b_g_sum) %>%
  pull(pred)
model_4_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre (ungrouped) Effect Model",  
                                 RMSE = model_4_rmse ))
rmse_results %>% kable(digits=5)


#Model - release year effects
b_y <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u - b_g))
predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

model_5_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre (grouped) + Year Effect Model",  
                                 RMSE = model_5_rmse ))
rmse_results %>% kable(digits=5)

#Model - day of week effects
b_wd <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  group_by(weekday) %>%
  summarize(b_wd = mean(rating - mu - b_i - b_u - b_g - b_y))
predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  left_join(b_wd, by="weekday") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_wd) %>%
  pull(pred)
model_6_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre (grouped) + Year + Weekday Effect Model",  
                                 RMSE = model_6_rmse ))
rmse_results %>% kable(digits=5)

#Model - time effect (week) - loess of week #
#plot residuals as fn of week
resid <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  left_join(b_wd, by="weekday") %>%
  mutate(resid = rating - mu - b_i - b_u - b_g - b_y - b_wd) %>%
  select(week,resid) %>%
  group_by(week) %>%
  summarize(average = mean(resid))
resid %>%
  ggplot(aes(y=average, x=week)) +
  geom_point() +
  geom_smooth()
#train loess model on residual data - use test data to determine optimal span parameter  
spans = seq(0.05, 0.9, len = 10)
rmses <- sapply(spans, function(s){
  train_loess_t <- resid %>% 
    mutate(week = as.numeric(week)) %>%
    loess(average ~ week, data = ., span = s, degree = 2)
  predicted_ratings <- test_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    left_join(b_wd, by="weekday") %>%
    mutate(week = as.numeric(week),
           b_t = predict(train_loess_t, week),
           pred = mu + b_i + b_u + b_g + + b_y + b_wd + b_t) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(spans, rmses)  

span <- spans[which.min(rmses)]
span

#train model using optimal span parameter
train_loess_t <- resid %>% 
  mutate(week = as.numeric(week)) %>%
  loess(average ~ week, data = ., span = span, degree = 2)
predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  left_join(b_wd, by="weekday") %>%
  mutate(week = as.numeric(week),
         b_t = predict(train_loess_t, week),
         pred = mu + b_i + b_u + b_g + b_y + b_wd + b_t) %>%
  pull(pred)
model_7_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre (grouped) + Year + Weekday + Time Effect Model",  
                                 RMSE = model_7_rmse ))
rmse_results %>% kable(digits=5)



#Model - Add regularization to penalize small sample sizes - cross-validation to pick lambda parameter
lambda <- vector("double",4)
lambdas <- seq(0, 10, 0.25)

#determine lambda for movie effects
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda[1] <- lambdas[which.min(rmses)]
lambda[1]

#recalculate movie effect with optimal lambda
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda[1]))

#determine lambda for user effects
rmses <- sapply(lambdas, function(l){
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda[2] <- lambdas[which.min(rmses)]
lambda[2]

#recalculate user effect with optimal lambda
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda[2]))

#determine lambda for genre effects
rmses <- sapply(lambdas, function(l){
  b_g <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda[3] <- lambdas[which.min(rmses)]
lambda[3]

#recalculate genre effect with optimal lambda
b_g <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda[3]))

#determine lambda for year effects
lambdas <- seq(160, 200, 1)
rmses <- sapply(lambdas, function(l){
  b_y <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda[4] <- lambdas[which.min(rmses)]
lambda[4]

#recalculate year effect with optimal lambda
b_y <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda[4]))

#determine lambda for week day effects
lambdas <- seq(0, 10000000, 500000)
rmses <- sapply(lambdas, function(l){
  b_wd <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    group_by(weekday) %>%
    summarize(b_wd = sum(rating - mu - b_i - b_u - b_g - b_y)/(n()+l))
  predicted_ratings <- test_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    left_join(b_wd, by="weekday") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y + b_wd) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)
#RMSE minimizes as lambda goes to infinity, effect goes to zero: remove from model

#retrain loess model of time effect with updated movie, user, genre, and year effects
resid <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  mutate(resid = rating - mu - b_i - b_u - b_g - b_y) %>%
  select(week,resid) %>%
  group_by(week) %>%
  summarize(average = mean(resid))

train_loess_t <- resid %>% 
    mutate(week = as.numeric(week)) %>%
    loess(average ~ week, data = ., span = span, degree = 2)
predicted_ratings <- test_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  mutate(week = as.numeric(week),
         b_t = predict(train_loess_t, week),
         pred = mu + b_i + b_u + b_g + b_y +  b_t) %>%
  pull(pred)
model_8_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Movie + User + Genre + Year + Time Effect Model",  
                                 RMSE = model_8_rmse ))
rmse_results %>% kable(digits=5)

#min/max adjustment - the prediction scale is 0 to 5 - any scores over 5 change to 5 and any below 0, change to 0
predicted_ratings[predicted_ratings > 5] <- 5
predicted_ratings[predicted_ratings < 0] <- 0
model_9_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Movie + User + Genre + Year + Time Effect Model with Min/Max Adjustment",  
                                 RMSE = model_9_rmse ))
rmse_results %>% kable(digits=5)

############################################################################################################################
#Testing Final Model on Validation data
############################################################################################################################
#####Train final model on whole edx dataset

#create the same variables for validation data as edx dataset - except weekday not needed
validation <- validation %>% 
  extract(col=title,into=c("title","year"),regex="^(.*)\\((\\d{4})\\)$") %>%
  mutate(year=as.integer(year),
         date = as_datetime(timestamp),
         week = round_date(date, unit = "week"))

#####Train final model on whole edx dataset
mu <- mean(edx$rating)

b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda[1]))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda[2]))

b_g <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda[3]))

b_y <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda[4]))

resid <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  mutate(resid = rating - mu - b_i - b_u - b_g - b_y) %>%
  select(week,resid) %>%
  group_by(week) %>%
  summarize(average = mean(resid))

train_loess_t <- resid %>% 
  mutate(week = as.numeric(week)) %>%
  loess(average ~ week, data = ., span = span, degree = 2)

predicted_ratings <- validation %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  mutate(week = as.numeric(week),
         b_t = predict(train_loess_t, week),
         pred = mu + b_i + b_u + b_g + b_y +  b_t) %>%
  pull(pred)
predicted_ratings[predicted_ratings > 5] <- 5
predicted_ratings[predicted_ratings < 0] <- 0

model_10_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Validation Data",  
                                 RMSE = model_10_rmse ))
rmse_results %>% kable(digits=5)


#############################################################################################################################
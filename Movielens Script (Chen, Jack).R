#loading necessary libraries
necessary_libraries <- c("tidyverse", "lubridate")
for (x in necessary_libraries){
  if(!require(x, character.only = TRUE)){
    install.packages(necessary_libraries)
    require(x, character.only = TRUE)
  }
}
rm(necessary_libraries, x)

#loading in data set
edx <- read_csv("dat/edx.csv")
validation <- read_csv("dat/validation.csv")

# Function for calculating RMSE
RMSE <- function(predicted_ratings,true_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


####Baseline and main effects####
##Creating a function to run k fold cross validation
crossvalidation <- function(x){ #x indicates the number of folds
  
  folds <- cut(seq(1, nrow(edx)), breaks = x, labels = FALSE)
  #create and empty numeric vector to store the produced RMSEs
  list_of_RMSE <- numeric()
  #loop to go through the ten different folds for 10 fold cross validation
  for(i in seq(1, x)){
    
    ##Creating test and train data
    print(c("Creating Train and Test Data: ", i))
    set.seed(135)
    testindex <- sample(folds == i, length(folds))
    trainData <- edx[-testindex,] %>%
      select(userId, movieId, genres, rating)
    testData <- edx[testindex,] %>% 
      semi_join(trainData, by = "movieId") %>%
      semi_join(trainData, by = "userId") %>% 
      select(userId, movieId, genres, rating)
    
    print("Calculating the mean and effects")
    #setting up predictor values
    mu <- mean(trainData$rating)
    
    movie_effect <- trainData %>%
      group_by(movieId) %>%
      summarize(b_i = mean(rating - mu))
    
    user_effect <- trainData %>%
      left_join(movie_effect, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = mean(rating - mu - b_i))
    
    genres_effect <- trainData %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(user_effect, by = "userId") %>%
      group_by(genres) %>%
      summarize(b_g = mean(rating - mu - b_i - b_u))
    
    ## Generating y_hat
    print("Creating predictions")
    pred <- testData %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(user_effect, by = "userId") %>%
      left_join(genres_effect, by = "genres") %>%
      mutate(pred = mu + b_i + b_u + b_g) %>%
      .$pred
    
    #adding the produced RMSE to a variable
    list_of_RMSE <- append(list_of_RMSE, RMSE(pred, testData$rating))
    print(c("Completed Loop #", i))
  }
  
  return(mean(list_of_RMSE))
}

crossvalidation(10)
#Looking at the cross validation results, we have 0.8563594



####Regularization####
lambda <- seq(0.3, 0.5, .025) #played around different ranges to get this range

#checking lambda for regularization for user effects
list_of_RMSE <- sapply(lambda, function(l){
  
  print(paste("Using Lambda Value", l))
  folds <- cut(seq(1, nrow(edx)), breaks = 10, labels = FALSE)
  
  #create and empty numeric vector to store the produced RMSEs
  list_of_RMSE <- numeric()
  
  #loop to go through the ten different folds for 10 fold cross validation
  for(i in 1:10){
    print(paste("Creating Train and Test Data: ", i))
    set.seed(135)
    testindex <- sample(folds == i, length(folds))
    trainData <- edx[-testindex,] %>%
      select(userId, movieId, genres, rating)
    testData <- edx[testindex,] %>% 
      semi_join(trainData, by = "movieId") %>%
      semi_join(trainData, by = "userId") %>% 
      select(userId, movieId, genres, rating)
    
    print("Calculating baseline and main effects")
    #setting up predictor values
    mu <- mean(trainData$rating)
    
    movie_effect <- trainData %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + l))
    
    user_effect <- trainData %>%
      left_join(movie_effect, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n() + l))
    
    genres_effect <- trainData %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(user_effect, by = "userId") %>%
      group_by(genres) %>%
      summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + l))
    
    ## Generating y_hat
    print("Creating predictions")
    pred <- testData %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(user_effect, by = "userId") %>%
      left_join(genres_effect, by = "genres") %>%
      mutate(pred = mu + b_i + b_u + b_g) %>%
      .$pred
    
    #adding the produced RMSE to a variable
    list_of_RMSE <- append(list_of_RMSE, RMSE(pred, testData$rating))
    print(paste("Completed Loop #", i))
  }
  print(paste("Completed Lambda Value: ", l))
  return(mean(list_of_RMSE))
}) 
plot(lambda, list_of_RMSE)
data.frame(lambda, list_of_RMSE)

## looks like 0.375 gives us the best value
##Achieved an RMSE of roughly 0.8563547


####Creating the Linear model with years release and timestamp order as predictors####
#Instead of working with the original data, I would like to continue working from where I left off
#Therefore, baseline, user effects, movie effects, and genre effects were removed leaving the residuals (resid)
mu <- mean(edx$rating)

movie_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + 0.375))

user_effect <- edx %>% 
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i)/(n() + 0.375))

genres_effect <- edx %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n() + 0.375))

edx <- edx %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  left_join(genres_effect, by = "genres") %>%
  mutate(resid = rating - mu - b_i - b_u - b_g) %>%
  select(userId, movieId, genres, timestamp, title, rating, resid)

#Data need to be cleaned before proceeding
#Extracting the release year from the title and adding it as an extra column
edx <- edx %>%
  mutate(release_year = as.numeric(str_match(title, "\\((\\d{4})\\)$")[,2]))

#Finding the mean and standard deviation of each timestamp
#This is for calculating timestamp order, as well as some other exploratory analyses
timestamp_sd_mean <- edx %>% group_by(userId) %>% 
  summarize(timestamp_mean = mean(timestamp), timestamp_sd = sd(timestamp))

#In additional to timestamp order, I also created a column for standardized timestamps to compare effectiveness
edx <- edx %>% left_join(timestamp_sd_mean, by = "userId") %>%
  mutate(timestamp_z = ifelse(timestamp_sd == 0, timestamp - timestamp_mean, (timestamp - timestamp_mean)/timestamp_sd)) %>%
  group_by(userId) %>%
  mutate(timestamp_centered = timestamp - timestamp_mean, timestamp_min = min(timestamp_centered)) %>% 
  ungroup() %>% mutate(timestamp_order = timestamp_centered - timestamp_min) %>% 
  select(-timestamp_min, -timestamp_sd, -timestamp_centered)

#Indices to select a portion of the data 
#because it is too demanding on the pc to graph out the entire dataset
index <- sample(1:length(edx$resid), 5000, replace = FALSE)

##Taking a look at how release year, timestamp, standardized timestamp (timestamp_z) and timestamp order
##compare to resid (the residuals after removing baseline and all main effects)

###Comparing various variables with resid

#Release year and resid
plot(edx$release_year[index], edx$resid[index])
cor(edx$release_year, edx$resid) # -0.003518538

#timestamp and resid
plot(edx$timestamp[index], edx$resid[index])
cor(edx$timestamp, edx$resid) #-0.00608956

#standardized timestamp (timestamp_z) and resid
plot(edx$timestamp_z[index], edx$resid[index])
cor(edx$timestamp_z, edx$resid) # 0.005843557

#timestamp order and resid
plot(edx$timestamp_order[index], edx$resid[index])
cor(edx$timestamp_order, edx$resid)#-0.006250654

#looks like the correlations is low but it could be significant enough to help with our model
#timestamp order was selected over timestamp z because timestamp order is more strongly
#correlated with resid


#Running a 10 fold cross validation to see if final RMSE improves


#creating the k folds cross validation function
lm_crossfoldvalidation <- function(x){ #x indicates the number of folds
  folds <- cut(seq(1, nrow(edx)), breaks = x, labels = FALSE)
  RMSEs <- numeric() #Empty variable for storing RMSE
  for(i in 1:x){
  
    #It is a time consuming loop. The prints throughout is to provide information on where the loop is
    print(paste("Starting loop number ", i))
    set.seed(500) #setting seed for reproducibility
    
    
    print(paste("Creating training and testing for loop ", i))
    testing_index <- sample(folds, nrow(edx), replace = FALSE) == i
    train <-edx[-testing_index,]
    test <- edx[testing_index,] %>% 
      semi_join(train, by = "userId") %>% 
      semi_join(train, by = "movieId")
    
  
    print("Running lm function to create model")
    model <- lm(resid ~ timestamp_order + release_year, data = train)
  
    print("Model Created, creating model summary")
    model_summary <- summary(model)
  
    lm_pred <- apply(test %>% select(timestamp_order, release_year), 1, function(x){
      model_summary$coefficients[1,1] + sum(x*c(model_summary$coefficients[2,1],model_summary$coefficients[3,1]))
    })
  
    RMSEs <- append(RMSEs, RMSE(lm_pred, test$resid))
    rm(testing_index, train, test, model, model_summary, lm_pred)
    print(paste("Loop number ", i, " completed"))
  }

  mean(RMSEs) 
}
lm_crossfoldvalidation(10) 
#With a linear model, it looks like we have an estimate RMSE of 0.8563335

### Creating the linear model from our edx data###
model <- lm(resid ~ timestamp_order + release_year, data = edx)
model_summary <- summary(model)

### Applying the model we have so far to the validation data to assess final RMSE
#Setting up the validation set so the predicted values can be generated
#Extracting release year
validation <- validation %>%
  mutate(release_year = as.numeric(str_match(title, "\\((\\d{4})\\)$")[,2]))

#Caculating timestamp order
validation <- validation %>% left_join(timestamp_sd_mean, by = "userId") %>%
  group_by(userId) %>%
  mutate(timestamp_centered = timestamp - timestamp_mean, timestamp_min = min(timestamp_centered)) %>% 
  ungroup() %>% mutate(timestamp_order = timestamp_centered - timestamp_min) %>% 
  select(-timestamp_min, -timestamp_sd, -timestamp_centered)


#Generating the predicted values
pred <- validation %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  left_join(genres_effect, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g + model_summary$coefficients[1,1] + 
           timestamp_order * model_summary$coefficients[2,1] + 
           release_year * model_summary$coefficients[3,1]) %>%
  .$pred

#Calculating RMSE for validation set
RMSE(pred, validation$rating)

#Looks like the RMSE on our validation set is 0.8648306
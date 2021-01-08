---
title: 'MovieLens - HarvardX: PH125.9x Data Science'
author: "Horea - Adrian Cioca"
date: "04/01/2021"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---

---
####################################################
# MovieLens Project - A Rating Prediction For Movies
####################################################

################
# 1. Introduction
################

  ##The purpose of the MovieLens project is to create a recommendation system 
  ##which will predict the user rating in order to be able to build a custom taste
  ##profile.
  ##We will start generating the data sets using the code provided by edx.
  ##The edx data set will be used for training our algorithm and the validation 
  ##data set to predict movie ratings.
  ##RMSE(Root Mean Square Error) will be the indicator used to measure the error 
  ##of the model in predicting the rating data.
  ##We used linear regression to predict the value of an outcome variable
  ##(rating) in base of one or more inputs predictors.
  ##To understand the behavior of the variables used in the linear model we used 
  ##- Scatter plots to show the liner relationship between the predictor 
  ##and response
  ##- Box plots to show any outlier observation in the variable
  ##- Density plots to show the distribution of the predictor variable like age of the movie, year of production, user id etc. 
###########################
# 2. Data Setup  
###########################

#####################################
## 2.1 Create edx and validation set


### Note: this process could take a couple of minutes
### If you don't have installed already the packages from if statements, 
### please uncomment them.

## 2.1.1 Data Download

## 2.1.1.0 Load Packages


## Check the package if is installed and install it if is not present

pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE,repos = "http://cran.us.r-project.org")
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }

pkgTest("tidyr")
library(tidyr)
pkgTest("caret")
library(caret)
pkgTest("data.table")
library(data.table)
pkgTest("dplyr")
library(dplyr)
pkgTest("broom")
library(broom)
pkgTest("lubridate")
library(lubridate) 
pkgTest("sqldf")
library(sqldf)
pkgTest("e1071")
library(e1071)
pkgTest("stringr")
library(stringr)
pkgTest("stringi")
library(stringi)


## 2.1.1.1 Download MovieLens data
###r, DownloadData, results='hide'
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

## 2.1.2 Create the Data Set
###{r, CreateTheDataSet}
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                            title = as.character(title),
                                            genres = as.character(genres))
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                            title = as.character(title),
#                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

## Validation set will be 10% of MovieLens data
##{r, ValidationSet}
set.seed(1)
#set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
# create the train set
edx <- movielens[-test_index,]
# create the test set
temp <- movielens[test_index,]

## Make sure userId and movieId in validation set are also in edx set
##{r, IncludeUserIDMovieIDinEDX}
validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

## Add rows removed from validation set back into edx set
##{r, AddRemovedRowsinEDX}
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

## Validation set will be used just to test the final algorithm

# 3. Analysis

## 3.1 General data analysis :
##{r edx}
## Show the structure of edx
str(edx)

## We have to check the integrity of the data if we have NAs  we have to remove them
## So we do check each column 
##{r DataIntegrity}
indx <- apply(edx, 2, function(x) any(is.na(x)))
indx

## We can see that our data is correct without NAs 
## The head of the data:
##{r head}
head(edx)

## 3.1.1 The movie with the highest number of ratings
##{r, MovieWithTheHighestRating}
## Return the movies with highest number of ratings
MovieIDRatings <-sqldf("select count(rating) as NumbersOfRatings, movieId, title from edx group by movieId, title ")
head(MovieIDRatings)

## So Toy Story has the higest number of ratings
## 3.1.2 A barplot of the ratings per movieId
##{r, Ratings }
## Barplot of rtatings per movieId
barplot(MovieIDRatings$NumbersOfRatings)
##
## 3.1.3 The average ratings per movieId
##{r, AverageRatingsPerMovieId}
## Calculate the average ratings per movieId
 avgMovieID <- mean(MovieIDRatings$NumbersOfRatings)
 avgMovieID

## 3.1.4 Genres of Movies

## We want to count the ratings in functions of genres to be able to give a better prediction
## of the ratings.
## So first we retrive the distinct genres from the database
##{r genres}
## Extract the distinct genres from edx database
genres <- edx$genres %>% str_split(., pattern = "\\|")
genres <- genres %>% unlist() %>% unique()
genres


## Then the movie ratings  per genres
## We use sqldf library 
##{r ratings} 
## Calculate the ratings per genres
Ratings_per_genres <- sqldf("select count(*) as rating, 'Comedy'     as genres  from edx where genres like '%Comedy%'  union 
                             select count(*) as rating, 'Romance'     as genres  from edx where genres like '%Romance%' union 
                             select count(*) as rating, 'Action'      as genres  from edx where genres like '%Action%' union
                             select count(*) as rating, 'Crime'       as genres  from edx where genres like '%Crime%' union
                             select count(*) as rating, 'Thriler'     as genres  from edx where genres like '%Thriler%' union
                             select count(*) as rating, 'Drama'       as genres  from edx where genres like '%Drama%' union
                             select count(*) as rating, 'SCi-Fi'      as genres  from edx where genres like '%Sci-Fi%' union
                             select count(*) as rating, 'Adventure'   as genres  from edx where genres like '%Adeventure%' union
                             select count(*) as rating, 'Children'    as genres  from edx where genres like '%Children%' union
                             select count(*) as rating, 'Fantasy'     as genres  from edx where genres like '%Fantasy%' union
                             select count(*) as rating, 'War'         as genres  from edx where genres like '%War%' union
                             select count(*) as rating, 'Animation'   as genres  from edx where genres like '%Animation%' union
                             select count(*) as rating, 'Musical'     as genres  from edx where genres like '%Musical%' union
                             select count(*) as rating, 'Western'     as genres  from edx where genres like '%Western%' union
                             select count(*) as rating, 'Mystery'     as genres  from edx where genres like '%Mystery%' union
                             select count(*) as rating, 'Film-Noir'   as genres  from edx where genres like '%Film-Noir%' union
                             select count(*) as rating, 'Horror'      as genres  from edx where genres like '%Horror%' union
                             select count(*) as rating, 'Documentary' as genres  from edx where genres like '%Romance%' union
                             select count(*) as rating, 'IMAX'        as genres  from edx where genres like '%IMAX%' union
                             select count(*) as rating, 'NoGenres'    as genres  from edx where genres='' or genres is null")

                               
# Save the data in descending order
Ratings_per_genres <- sqldf("select *  from Ratings_per_genres order by rating desc" )
Ratings_per_genres



## 3.1.5 A barplot of genres in function of ratinngs:
##{r genresVsratings}
## Barplot of genres in function of ratinngs 
barplot(Ratings_per_genres$rating/1000000, names.arg = Ratings_per_genres$genres, 
 xlab = "ratings", horiz=TRUE, las=1, col=c(1:10), 
 desc(Ratings_per_genres$rating/1000000))

## 3.1.6 A histogram of ratings with the density:
##{r Ratings_per_genres}
## Histogram of ratings
hist(Ratings_per_genres$rating/1000000, 
     main="Histogram for Ratings per genres", 
     xlab="Ratings", 
     border="blue", 
     col="green", 
     xlim=c(0,4), 
     las=1, 
     breaks=50, 
     prob = TRUE)

lines(density(Ratings_per_genres$rating/1000000))




## 3.1.7 A pie of ratings counts per genres is bellow.
##{r   echo=FALSE}
## Pie of ratings counts per genres
pie(Ratings_per_genres$rating, labels = Ratings_per_genres$genres)


## We can see from the pie chart the first three most rated genres : Drama, Comedy and Action
## We need also to count the distinct genres, movies and users:
##{r  echo = FALSE}
## Distinct genres, movies and users
edx %>% summarize(distinct_genres = n_distinct(genres), distinct_movies = n_distinct(movieId), distinct_users = n_distinct(userId))

## 3.1.8 The mean for all genres

##The mean for all genres and the mean for each genres will give us an overview of the movies ## in terms of the ratings:
##{r  echo = FALSE}
# Mean for all genres
mean_all <- mean(edx$rating)
mean_all

## 3.1.9 The mean per gender with histogram 
##{r echo = FALSE}

## Mean per gender with histogram
means    <-           sqldf("select avg(rating) as mean, 'Comedy'      as genres  from edx where genres like '%Comedy%'  union 
                             select avg(rating) as mean, 'Romance'     as genres  from edx where genres like '%Romance%' union 
                             select avg(rating) as mean, 'Action'      as genres  from edx where genres like '%Action%' union
                             select avg(rating) as mean, 'Crime'       as genres  from edx where genres like '%Crime%' union
                             select avg(rating) as mean, 'Thriler'     as genres  from edx where genres like '%Thriler%' union
                             select avg(rating) as mean, 'Drama'       as genres  from edx where genres like '%Drama%' union
                             select avg(rating) as mean, 'SCi-Fi'      as genres  from edx where genres like '%Sci-Fi%' union
                             select avg(rating) as mean, 'Adventure'   as genres  from edx where genres like '%Adeventure%' union
                             select avg(rating) as mean, 'Children'    as genres  from edx where genres like '%Children%' union
                             select avg(rating) as mean, 'Fantasy'     as genres  from edx where genres like '%Fantasy%' union
                             select avg(rating) as mean, 'War'         as genres  from edx where genres like '%War%' union
                             select avg(rating) as mean, 'Animation'   as genres  from edx where genres like '%Animation%' union
                             select avg(rating) as mean, 'Musical'     as genres  from edx where genres like '%Musical%' union
                             select avg(rating) as mean, 'Western'     as genres  from edx where genres like '%Western%' union
                             select avg(rating) as mean, 'Mystery'     as genres  from edx where genres like '%Mystery%' union
                             select avg(rating) as mean, 'Film-Noir'   as genres  from edx where genres like '%Film-Noir%' union
                             select avg(rating) as mean, 'Horror'      as genres  from edx where genres like '%Horror%' union
                             select avg(rating) as mean, 'Documentary' as genres  from edx where genres like '%Romance%' union
                             select avg(rating) as mean, 'IMAX'        as genres  from edx where genres like '%IMAX%' union
                             select avg(rating) as mean, 'NoGenres'    as genres  from edx where genres='' or genres is null")
means
hist(means$mean, 
     main="Histogram for means per genres", 
     xlab="means", 
     border="blue", 
     col="green", 
     xlim=c(0,4), 
     labels = TRUE,
     #xlabel = genres,
     las=1, 
     breaks=50, 
     prob = TRUE)
#                    y = c(mean_all, mean_comedy, mean_romance, mean_action, mean_crime, mean_thriler, mean_drama, mean_SCi_Fi, mean_adventure,  #                           mean_children, mean_fantasy,mean_war, mean_animation, mean_musical, mean_western, mean_mystery, mean_film_noir,      #                            mean_horror, mean_documentary, mean_imax, mean_nogenres))
histogram(means$mean,means$genres)


## 3.1.10 Ratings function of date

## Also we want to know the number of ratings in function of the date
## So we add a column date in the format yyyy-mm-dd which is a conversion of the timestamp  column
## The new structure will be as follow: 
##{r,  echo=FALSE}
## Add a column date in the format yyyy-mm-dd
edx <- edx %>% mutate(date = as.Date(as.POSIXct(edx$timestamp, origin="1970-01-01"))) %>% select(userId, movieId, rating, timestamp, date, title, genres)
#Now we can remove the timestamp column
edx <- edx %>% select(-timestamp)
#The new structure:
str(edx)


## Now we can extract the released year from the title and saved as releasedYear :
##{r, echo=FALSE}
## Extract the released year
edx <- edx %>% mutate(releasedYear = substr(title, nchar(title) - 4, nchar(title) - 1))
#Will do the same thing for validation database
validation <- validation %>% mutate(releasedYear = substr(title, nchar(title) - 4, nchar(title) - 1))
## New structure of edx
str(edx)

## Also the age of the movies will be  calculated in base of the ReleasedYear:
##{r, echo=FALSE}
## Age of movie on edx database
edx <- edx %>% mutate(age = as.numeric( substr(Sys.Date(),1,4)) - as.numeric(releasedYear)) 

#Will do the same for validation database
validation <- validation  %>% mutate(age = as.numeric( substr(Sys.Date(),1,4)) - as.numeric(releasedYear)) 
#Now edx will look like this:
str(edx)

## It's important to have in our analyses the rating year
## This will be obtain as folow :   
##{r, echo=FALSE}
## Add rating year
edx <- edx %>%  mutate(., rating_year = year(as_datetime(date)))


## 3.1.11 The minimum , maximum and the mean of the ages of the movies in edx:
##{r, echo=FALSE}
## Clculate minimum , maximum and the mean of the ages 
minAge  <- min(edx$age)
print(minAge)
maxAge  <- max(edx$age)
print(maxAge)
meanAge <- mean(edx$age)
print(meanAge)
 
print("For Validation database:")
minAge_validation  <- min(validation$age)
print(minAge_validation)
maxAge_validation  <- max(validation$age)
print(maxAge_validation)
meanAge_validation <- mean(validation$age)
print(meanAge_validation)
 

## 3.1.12 Histogram of ages

## A histogram of ages show us that the highest number of movies from netflix have an age around 24 years:
##{r, echo=FALSE}

## A histogram of ages 
ggplot(edx, aes(x = edx$age),xlim(100)) + stat_bin(binwidth = 2)
  geom_histogram()

## 3.1.13 A plot of ratings and age on validation set
##{r, echo=FALSE}
## Linear model of ratings function of age  
fit <- validation %>% lm(rating ~ age, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

## A plot of ratings and age 
validation %>% mutate(rating_hat = predict(fit, newdata = .)) %>% 
  ggplot(aes(rating_hat, age, label = movieId)) +
  geom_point() + 
  geom_abline()
  

## We will create also for test purpouse a data base with the mouvies age under 
##  30 years  edx_30_minus
##{r, echo=FALSE}
## Create edx_30_minus database
edx_30_minus <-edx %>% filter(age < 30) 
str(edx_30_minus)

# 4. Linear Models Analysis

## Like methods will be use :
## We will use edx database to train the model and edx_30_minus and validation databases to test the model

## 4.1  Mean effect model
##{r, echo=FALSE}
## Calculate the mean of the ratings
## on edx database
mu_hat <- mean(edx$rating)
mu_hat

## on edx_30_minus database
mu_hat_30 <- mean(edx_30_minus$rating)
mu_hat_30

## on validation database
mu_hat_v <- mean(validation$rating)
mu_hat_v

## 4.2 Movie effect model
 
##{r, echo=FALSE}
## Create movie average data and plot the b_i coefficient 
movie_avgs <- edx %>% group_by(movieId)
movie_avgs <- movie_avgs %>% summarize(b_i = mean(rating - mu_hat))
qplot(b_i, data = movie_avgs, bins = 100, color = I("blue"))

b_i <-mean(edx$rating - mu_hat)




## Predicted ratings on validation database
##{r, MovieModel, echo=FALSE}
## Calculate Predicted ratings for movie model on validation database
predicted_ratings_m <- mu_hat + validation %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)
## Show the head of data
head(predicted_ratings_m)   


## Predicted ratings on edx_30_minus
##{r, MovieModeledx_30_minus, echo=FALSE}
## Calculate Predicted ratings for movie model on edx_30_minus database
predicted_ratings_m_30 <- mu_hat + edx_30_minus %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)
## Show the head of data
  head(predicted_ratings_m_30)  

## Movie Analyse Model on validation database
##{r, MovieAnalyseModel}
# Scatter plot
scatter.smooth(x=predicted_ratings_m, y=validation$rating, main="validation$rating ~ predicted_ratings_m") 

# Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_m, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_m)$out))  # box plot for 'predicted ratings'
boxplot(validation$rating, main="Rating", sub=paste("Outlier rows: ", boxplot.stats(validation$rating)$out))  # box plot for 'validation ratings'

# Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_m), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_m), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_m), col="red")
plot(density(validation$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(validation$rating), 2)))  # density plot for 'Ratings'
polygon(density(validation$rating), col="red")

# Correlation coefficient
print(cor(predicted_ratings_m, validation$rating)) 

## Density plot in general is showing how close we are to normality.
## Correlation show the linear dependence between two variables. It is between -1 and 1
## If is positive closed to 1 it shows a strong dependence between the two variables.
## If is closed to 0 it shows a week dependence.
## In this case is bigger than 0 but not so closed to 1 is under 0.5.
## So we can say that is still a week dependency between these two variables

## Movie Analyse Model on edx_30_minus database
##{r, MovieAnalyseModeledx_30_minus}
# Scatter plot
scatter.smooth(x=predicted_ratings_m_30, y=edx_30_minus$rating, main="edx_30_minus$rating ~ predicted_ratings_m") 

# Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_m_30, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_m_30)$out))  # box plot for 'predicted ratings'
boxplot(edx_30_minus$rating, main="Raiting", sub=paste("Outlier rows: ", boxplot.stats(edx_30_minus$rating)$out))  # box plot for 'validation ratings'

# Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_m_30), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_m_30), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_m_30), col="red")
plot(density(edx_30_minus$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(edx_30_minus$rating), 2)))  # density plot for 'Ratings'
polygon(density(edx_30_minus$rating), col="red")

# Correlation coefficient
print(cor(predicted_ratings_m_30, edx_30_minus$rating)) 
```
## Build the linear model for Movies

## On validation database
##{r, echo=FALSE}
 ## build linear regression model for full model
linearModelMovie <- lm(predicted_ratings_m ~ validation$rating, data=validation)
print(linearModelMovie)
  

## Now we can write the linear model formula for Movie Model 
## On validation database:
## predicted_ratings_m <- 2.7761 + 0.2096 * validation$rating

## On edx_30_minus
##{r, echo=FALSE}
 ## build linear regression model for full model
linearModelMovie <- lm(predicted_ratings_m_30 ~ edx_30_minus$rating, data=edx_30_minus)
print(linearModelMovie)
  

## So the linear model formula for Movie Model on edx_30_minus
## predicted_ratings_m <- 2.7558 + 0.1998 * validation$rating




## 4.3  User effect model

{r, echo=FALSE}
## User average
user_avgs <- edx %>% group_by(userId)# 
user_avgs <- user_avgs %>% summarize(b_u = mean(rating - mu_hat - b_i))

## Plot b_u coefficient
qplot(b_u, data = user_avgs, bins = 100, color = I("red"))
b_u <-mean(edx$rating - mu_hat - b_i)






## Predicted Ratings on validation database 
##{r, UserModel, echo=FALSE}

## Predicted Ratings on validation database 
predicted_ratings_u <- mu_hat + validation %>% left_join(user_avgs, by='userId') %>% pull(b_u)
## Show the head of the user predicted ratings
head(predicted_ratings_u) 


##{r, UserDensity}
#Scatter plot
scatter.smooth(x=predicted_ratings_u, y=validation$rating, main="validation$rating ~ predicted_ratings") 


#Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_u, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_u)$out))  # box plot for 'predicted ratings'
boxplot(validation$rating, main="Raiting", sub=paste("Outlier rows: ", boxplot.stats(validation$rating)$out))  # box plot for 'validation ratings'


#Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_u), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_u), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_u), col="red")
plot(density(validation$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(validation$rating), 2)))  # density plot for 'Ratings'
polygon(density(validation$rating), col="red")

# Correlation coefficient
print(cor(predicted_ratings_u, validation$rating)) 

## Predicted Ratings on edx_30_minus database
##{r, UserModel_edx30, echo=FALSE}
## Predicted Ratings on edx_30_minus database
   predicted_ratings_u_30 <- mu_hat + edx_30_minus %>% left_join(user_avgs, by='userId') %>% pull(b_u)
## Show the head of the user predicted ratings
head(predicted_ratings_u_30)  


##{r, UserDensity_edx30}
#Scatter plot
scatter.smooth(x=predicted_ratings_u_30, y=edx_30_minus$rating, main="edx_30_minus$rating ~ predicted_ratings") 


#Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_u_30, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_u_30)$out))  # box plot for 'predicted ratings'
boxplot(edx_30_minus$rating, main="Rating", sub=paste("Outlier rows: ", boxplot.stats(edx_30_minus$rating)$out))  # box plot for 'validation ratings'


#Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_u_30), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_u_30), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_u_30), col="red")
plot(density(edx_30_minus$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(edx_30_minus$rating), 2)))  # density plot for 'Ratings'
polygon(density(edx_30_minus$rating), col="red")

# Correlation coefficient
print(cor(predicted_ratings_u_30, edx_30_minus$rating)) 

## User Model 
## On validation database
##{r, echo=FALSE}
 ## build linear regression model on validation database
linearModelMovie <- lm(predicted_ratings_u ~ validation$rating, data=validation)
print(linearModelMovie)
  

## Linear model formula for User Model on validation database
## predicted_ratings_m <- 2.9613 + 0.1568 * validation$rating

## On edx_30_minus database
##{r, echo=FALSE}
## build linear regression model on edx_30_minus database
linearModelMovie <- lm(predicted_ratings_u_30 ~ edx_30_minus$rating, data=edx_30_minus)
print(linearModelMovie)
  


## Linear model formula for User Model on edx_30_minus database
## predicted_ratings_m <- 2.9613 + 0.1572 * edx_30_minun$rating

## 4.4  Age effect model
##{r, echo=FALSE}
## Age average
age_avgs <- edx %>% group_by(age)# 
age_avgs <- age_avgs %>% summarize(c_u = mean(rating))
## Plot c_u coeficient
qplot(c_u, data = age_avgs, bins = 100, color = I("green"))

## Predicted ratings on validation database
##{r, AgeModel, echo=FALSE}
   ## Predicted ratings on validation database
   predicted_ratings_a <- validation %>% left_join(age_avgs, by='age') %>% pull(c_u)
   


## Predicted ratings on edx_30_minus database
##{r, AgeModel_edx30, echo=FALSE}
   ## Predicted ratings on edx_30_minus database
   predicted_ratings_a_30 <- edx_30_minus %>% left_join(age_avgs, by='age') %>% pull(c_u)
   


## Age model analyses on validation database
##{r, AgeDensity}
# Scatter plot
par(mar=c(1,1,1,1))
graphics.off()
system.time(smoothScatter(predicted_ratings_a, validation$rating, nbin = 100))  ## 3.3 seconds

#Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_a, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_a)$out))  # box plot for 'predicted_ratings'
boxplot(validation$rating, main="Rating", sub=paste("Outlier rows: ", boxplot.stats(validation$rating)$out))  # box plot for 'validation$rating'


#Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_a), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_a), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_a), col="red")
plot(density(validation$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(validation$rating), 2)))  # density plot for 'Ratings'
polygon(density(validation$rating), col="red")

# Correlation coefficient
print(cor(predicted_ratings_a, validation$rating)) 

```


## Age model analyses on edx_30_minus database
##{r, AgeDensity_edx30}
# Scatter plot
par(mar=c(1,1,1,1))
graphics.off()
system.time(smoothScatter(predicted_ratings_a_30, edx_30_minus$rating, nbin = 100))  ## 3.3 seconds

#Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_a_30, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_a_30)$out))  # box plot for 'predicted ratings'
boxplot(edx_30_minus$rating, main="Raiting", sub=paste("Outlier rows: ", boxplot.stats(edx_30_minus$rating)$out))  # box plot for 'ratings'


#Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_a_30), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_a_30), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_a_30), col="red")
plot(density(edx_30_minus$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(edx_30_minus$rating), 2)))  # density plot for 'Ratings'
polygon(density(edx_30_minus$rating), col="red")

# Correlation coefficient
print(cor(predicted_ratings_a_30, edx_30_minus$rating)) 



## On validation database
##{r, echo=FALSE}
 ## build linear regression model on validation database
linearModelMovie <- lm(predicted_ratings_a ~ validation$rating, data=validation)
print(linearModelMovie)
  

## Linear model formula for Age model on validation database
## predicted_ratings_m <- 3.43960 + 0.02081 * validation$rating

## On edx_30_minus database
##{r, echo=FALSE}
# build linear regression model on edx_30_minus database
linearModelMovie <- lm(predicted_ratings_a_30 ~ edx_30_minus$rating, data=edx_30_minus)
print(linearModelMovie)
  

## Linear model formula for Age model on edx_30_minus database
## predicted_ratings_m <- 3.435477 + 0.002405 * validation$rating

## 4.5 Movie and user effect model
## Predicted ratings on validation database
##{r, echo=FALSE}
## Predicted ratings on validation database

## Calculate b_i coefficient
b_i <- validation %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu_hat))
## Calculate b_u coefficient  
b_u <- validation %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - b_i))
## Calculate predicted ratings for movie user effect on validation database 
predicted_ratings_mu <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_hat + b_i +  b_u) %>% pull(pred)


   


## Predicted ratings on edx_30_minus database
##{r, echo=FALSE}

## Predicted ratings on edx_30_minus database

## Calculate b_i coefficient   
b_i <- edx_30_minus %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu_hat))
## Calculate b_u coefficient  
b_u <- edx_30_minus %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - b_i))
## Calculate predicted ratings for movie user effect on edx_30_minus database 
predicted_ratings_mu_30 <- edx_30_minus %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_hat + b_i +  b_u) %>% pull(pred)




## Movie and User Analyse Model on validation database
##{r, MovieUserDensity}

# Scatter plot
scatter.smooth(x=predicted_ratings_mu, y=validation$rating, main="validation$rating ~ predicted_ratings_mu")  

# Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_mu, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_mu)$out))  # box plot for 'predicted ratings'
boxplot(edx_30_minus$rating, main="Raiting", sub=paste("Outlier rows: ", boxplot.stats(edx_30_minus$rating)$out))  # box plot for 'rating'


# Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_mu), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_mu), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_mu), col="red")
plot(density(validation$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(validation$rating), 2)))  # density plot for 'Ratings'
polygon(density(validation$rating), col="red")

## Print Correlation factor
print(cor(predicted_ratings_mu, validation$rating)) 




## Movie and User Analyse Model on edx_30_minus database
##{r, MovieUserDensity_edx30}

# Scatter plot
scatter.smooth(x=predicted_ratings_mu_30, y=edx_30_minus$rating, main="validation$rating ~ predicted_ratings_mu")  

# Box plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(predicted_ratings_mu_30, main="Predicted_ratings", sub=paste("Outlier rows: ", boxplot.stats(predicted_ratings_mu_30)$out))  # box plot for 'predicted ratings'
boxplot(edx_30_minus$rating, main="Rating", sub=paste("Outlier rows: ", boxplot.stats(edx_30_minus$rating)$out))  # box plot for 'rating'


# Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(predicted_ratings_mu_30), main="Density: Predicted ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(predicted_ratings_mu_30), 2)))  # density plot for 'Predicted ratings'
polygon(density(predicted_ratings_mu_30), col="red")
plot(density(edx_30_minus$rating), main="Density Plot: Ratings", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(edx_30_minus$rating), 2)))  # density plot for 'Ratings'
polygon(density(edx_30_minus$rating), col="red")

print(cor(predicted_ratings_mu_30, edx_30_minus$rating)) 




## Linear model formula for Movie User

## On validation database
##{r, echo=FALSE}
 ## build linear regression model for Movie User effect on validation database
linearModelMovie <- lm(predicted_ratings_mu ~ validation$rating, data=validation)
print(linearModelMovie)
  

## Linear model formula for Movie User model on validation database
## predicted_ratings_m <- 2.2249 + 0.3664 * validation$rating

## On edx_30_minus database
##{r, echo=FALSE}
# build linear regression model for Movie User effect on edx_30_minus database
linearModelMovie <- lm(predicted_ratings_mu_30 ~ edx_30_minus$rating, data=edx_30_minus)
print(linearModelMovie)
  

## Linear model formula for Movie User model on validation database
## predicted_ratings_m <- 2.205 + 0.357 * validation$rating

# 5. RMSE

## 5.1 Residual Mean Square Error(RMSE)

## We will use Residual Mean Square Error(RMSE) to measure accuracy and the typical error of the model.
## We define a function for RMSE as follow:
##{r, RMSE}
## Define Residual Mean Square Error(RMSE) function as follow:
RMSE <- function(actual, predicted){
   sqrt(mean((actual - predicted)^2))
   
 }
 



## RMSE for Mean effect model:
## On validation database
##{r, echo=FALSE}
   ## Residual Mean Square Error(RMSE) on validation database for Mean effect model
   mu_rmse <- RMSE(validation$rating, mu_hat)
   mu_rmse

## We save the data in a table named final_results which will be used for conclusions
##{r, echo=FALSE}
    ## Save data in a table named final_results
    final_results <- tibble(method_used = "Average", RMSE = mu_rmse, database="validation")
    ## Print existing data
    print(final_results) 


## On edx_30_minus
##{r, echo=FALSE}
   ## Residual Mean Square Error(RMSE) on edx_30_minus database for Mean effect model
   mu_rmse <- RMSE(edx_30_minus$rating, mu_hat)
   mu_rmse

## Save the data into final_results
##{r, echo=FALSE}
    ## Save the data into final_results
    final_results <- final_results %>% add_row(method_used = "Average", RMSE = mu_rmse,  database = "edx_30_minus" )
    ## Print existing data
    print(final_results) 


## 5.2 RMSE for Movie effect model :
## On validation database
##{r, echo=FALSE}
   ## RMSE for Movie effect model on validation database
   movie_rmse <- RMSE(validation$rating, predicted_ratings_m)
   movie_rmse
  

## Save data in final_results
##{r, echo=FALSE}
##Save data in final_results
final_results <- final_results %>% add_row(method_used = "Movie efect", RMSE = movie_rmse,  database = "validation" )
    ## Print existing data
    print(final_results) 

## On edx_30_minus database
##{r, echo=FALSE}
   ## RMSE for Movie effect model on edx_30_minus database 
   movie_rmse <- RMSE(edx_30_minus$rating,predicted_ratings_m_30)
   movie_rmse
  


## Save data in final_results
##{r, echo=FALSE}
## Save data in final_results   
final_results <- final_results %>% add_row(method_used = "Movie efect", RMSE = movie_rmse,  database = "edx_30_minus" )
    ## Print existing data
    print(final_results) 


## 5.3 RMSE for  User effect model
## On validation database
##{r, echo=FALSE}
   ## RMSE for User effect model on validation database
   user_rmse <- RMSE(validation$rating, predicted_ratings_u)
   
   user_rmse
   


## Save data in final_results
##{r, echo=FALSE}
   ## Save data in final_results
   final_results <- final_results %>% add_row(method_used = "User efect", RMSE = user_rmse, database = "validation" )
    ## Print existing data
    print(final_results) 

## On edx_30_minus database
##{r, echo=FALSE}
   ## RMSE for User effect model on edx_30_minus database
   user_rmse <- RMSE(edx_30_minus$rating, predicted_ratings_u_30)
   
   user_rmse
   

## Save data in final_results
##{r, echo=FALSE}
## Save data in final_results
   final_results <- final_results %>% add_row(method_used = "User efect", RMSE = user_rmse, database = "edx_30_minus" )
## Print existing data
    print(final_results) 


## 5.4  RMSE for Age effect model
## On validation database
##{r, echo=FALSE}
   ## RMSE for Age effect model on validation database
   age_rmse <- RMSE(validation$rating, predicted_ratings_a)
   
   age_rmse
   

## Save data in final_results
##{r, echo=FALSE}
## Save data in final_results
   final_results <- final_results %>% add_row(method_used = "Age efect", RMSE = age_rmse, database = "validation" )
## Print existing data   
print(final_results) 

## On edx_30_minus database
##{r, echo=FALSE}
   ## RMSE for Age effect model on edx_30_minus database
   age_rmse <- RMSE(edx_30_minus$rating, predicted_ratings_a_30)
   
   age_rmse
   


## Save data in final_results
##{r, echo=FALSE}
## Save data in final_results   
final_results <- final_results %>% add_row(method_used = "Age efect", RMSE = age_rmse, database = "edx_30_minus" )
## Print existing data 
    print(final_results) 


## 4.5 RMSE for Movie and user effect model
## On validation database
##{r, echo=FALSE}
   ## RMSE for Movie and user effect model on validation database
   movie_user_rmse <- RMSE(validation$rating, predicted_ratings_mu)
   
   movie_user_rmse

## Save data in final results
##{r, echo=FALSE}
## Save data in final results
   final_results <- final_results %>% add_row(method_used = "Movie and user efect", RMSE = movie_user_rmse, database = "validation" )
## Print existing data 
    print(final_results) 


## On edx_30_minus database
##{r, echo=FALSE}
## RMSE for Movie and user effect model on edx_30_minus database
   movie_user_rmse <- RMSE(edx_30_minus$rating, predicted_ratings_mu_30)
   
   movie_user_rmse

## Save data in final results
##{r, echo=FALSE}
## Save data in final results
   final_results <- final_results %>% add_row(method_used = "Movie and user efect", RMSE = movie_user_rmse, database = "edx_30_minus" )
## Print existing data 
    print(final_results) 



##{r, echo=FALSE}
summary(lm(validation$rating ~ predicted_ratings_mu + predicted_ratings_m, data=validation))

# 6. Conclusion
## We saw that the lowest RMSE is from Movie User model 0.8251770 which show us that is the best model to use in predictions of the new ratings. 




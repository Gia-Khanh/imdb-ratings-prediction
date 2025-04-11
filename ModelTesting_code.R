install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
library(boot)
library(readr)
imdb_data = read.csv("~/Downloads/IMDB_data_Winter_2025.csv")
attach(imdb_data)

##1. Keep only English and color movies
data1 = subset(imdb_data, language =="English" & colour_film == "Color")

##2. Drops columns that are not considered in the model
data2 = data1 [, -c(2,3,6,7,10,13,18,20,22,23,24,26)]

## 3. Group Actors, combine them into a single field and dummify 
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
library(boot)
top_actors <- c( "Robert De Niro", "Bill Murray", "J.K. Simmons", "Jason Statham", "Kevin Spacey")

# Check if the actor names match top actors and replace others with "Others"
data2$actor1 = ifelse(data2$actor1 %in% top_actors, data2$actor1, "Others")
data2$actor2 = ifelse(data2$actor2 %in% top_actors, data2$actor2, "Others")
data2$actor3 = ifelse(data2$actor3 %in% top_actors, data2$actor3, "Others")

# Combine actor names while keeping original spaces and periods
data2$actor = apply(data2[, c("actor1", "actor2", "actor3")], 1, function(x) paste(unique(na.omit(x)), collapse = "|"))

# Only remove spaces between names, not within names
data2$actor = gsub("\\.", "", data2$actor)  # Remove periods only
data2$actor = gsub(" ", "", data2$actor)   # Optionally remove spaces if desired

# Remove original actor columns
data2 = data2[, !(colnames(data2) %in% c("actor1", "actor2", "actor3"))]

# Separate the rows by the actor names, keeping the original actor names intact
data3 = separate_rows(data2, actor, sep = "\\|")
data3 = mutate(data3, category2 = 1)
data3 = pivot_wider(data3, names_from = actor, values_from = category2, values_fill = 0)

# View the resulting data frame
#View(data3)

# Ensure all top_actors columns exist (even if they were missing in the data)
top_actors_clean <- gsub("\\.| ", "", top_actors)  # Remove special characters to match column names
for (actor in top_actors_clean) {
  if (!(actor %in% colnames(data3))) {
    data3[[actor]] <- 0  # Add missing columns with 0 values
  }
}


##4. Group Directors 
top_directors = c("Woody Allen", "Steven Spielberg", "Clint Eastwood", "Spike Lee", "Steven Soderbergh")

##If directors'names are found in top_directors, names are unchanged otherwise "Other directors"
data3$director = ifelse(data3$director %in% top_directors, data3$director, "Other directors") 

##5. Group distributors
top_distributors = c("Warner Bros.", "Universal Pictures", "Paramount Pictures", "Twentieth Century Fox", "Columbia Pictures Corporation")

data3$distributor= ifelse(data3$distributor %in% top_distributors, data3$distributor, "Other distributors") 

##6. Group cinematographers
top_cinemtographers = c("multiple", "Roger Deakins", "Mark Iwins", "John Bailey", "Andrew Dunn")

data3$cinematographer = ifelse(data3$cinematographer %in% top_cinemtographers, data3$cinematographer, "Other cinematographers")

##7. Group production companies
top_production_companies <- c( "Universal Pictures", "Paramount Pictures", "Columbia Pictures Corporation",
                               "Warner Bros.", "New Line Cinema")

data3$production_company <- ifelse(data3$production_company %in% top_production_companies, data3$production_company, "Others companies")

##8. Group Maturity Ratings
data3 = data3 %>%
  mutate(maturity_rating = case_when(
    maturity_rating %in% c("G", "TV-G") ~ "G",
    maturity_rating %in% c("Approved", "PG","GP","Passed") ~ "PG",
    maturity_rating %in% c("PG-13", "TV-14") ~ "PG-13",
    maturity_rating %in% c("R", "X","NC-17","M") ~ "R",
    TRUE ~ maturity_rating
  ))


## Convert categorical predictors to numerical values

data3$distributor= as.factor(data3$distributor)
data3$director= as.factor(data3$director)
data3$production_company = as.factor(data3$production_company)
data3$cinematographer= as.factor(data3$cinematographer)
data3$maturity_rating= as.factor(data3$maturity_rating)

########## FIND THE POLYNOMIAL DEGREE FOR NNON-LINEAR/CONTINUOUS NUMERICAL VARIABLES ###### 

results <- data.frame(a = integer(), b = integer(), c = integer(), mse = numeric())

# possible a, b, c combination
for (a_val in params$a) {
  for (b_val in params$b) {
    for (c_val in params$c) {
      
      formula_str <- paste0(
        "imdb_score ~ movie_budget + poly(duration, ", a_val, ") + nb_faces + ",
        "poly(movie_meter_IMDBpro, ", b_val, ") + poly(nb_news_articles, ", c_val, ") + ",
        "release_year + release_year*movie_budget + action + adventure + scifi + thriller + 
        musical + romance + western + sport + horror + drama + war + animation + crime + 
        maturity_rating + BillMurray + JKSimmons + RobertDeNiro + KevinSpacey + JasonStatham"
      )
      
      fit <- glm(formula_str, data = data3, family = gaussian)
      
      # calculate MSE, k=10 
      cv_result <- cv.glm(data3, fit, K = 10)
      current_mse <- cv_result$delta[1]  
      
      # store values
      results <- rbind(results, data.frame(a = a_val, b = b_val, c = c_val, mse = current_mse))
    }
  }
}

print(results)

# find the a,b,c for the min MSE 
best_row <- results[which.min(results$mse), ]
print(paste0("best fit：a=", best_row$a, ", b=", best_row$b, ", c=", best_row$c, ", MSE=", round(best_row$mse, 3)))




library(readr)

# Load test dataset
data_test = read_csv("~/Downloads/test_data_IMDB_Winter_2025.csv")

#### PREPROCESS ON TESTING DATASET ######## 

##1. Keep only English and color movies
data_test1 = subset(data_test, colour_film == "Color")

##2. Drops columns that are not considered in the model
data_test2 = data_test1[, -c(2,3,6,7,10,13,18,20,22,23,24,26)]

## 3. Group Actors, combine them into a single field, and dummify 
top_actors <- c("Robert De Niro", "Bill Murray", "J.K. Simmons", "Jason Statham", "Kevin Spacey")

# Check if the actor names match top actors and replace others with "Others"
data_test2$actor1 = ifelse(data_test2$actor1 %in% top_actors, data_test2$actor1, "Others")
data_test2$actor2 = ifelse(data_test2$actor2 %in% top_actors, data_test2$actor2, "Others")
data_test2$actor3 = ifelse(data_test2$actor3 %in% top_actors, data_test2$actor3, "Others")

# Combine actor names while keeping original spaces and periods
data_test2$actor = apply(data_test2[, c("actor1", "actor2", "actor3")], 1, function(x) paste(unique(na.omit(x)), collapse = "|"))

# Remove special characters from actor names to match column names
data_test2$actor = gsub("\\.", "", data_test2$actor)  # Remove periods
data_test2$actor = gsub(" ", "", data_test2$actor)    # Remove spaces

# Remove original actor columns
data_test2 = data_test2[, !(colnames(data_test2) %in% c("actor1", "actor2", "actor3"))]

# Separate the rows by the actor names
data_test3 = separate_rows(data_test2, actor, sep = "\\|")

# Create a dummy variable for each actor
data_test3 = mutate(data_test3, category2 = 1)
data_test3 = pivot_wider(data_test3, names_from = actor, values_from = category2, values_fill = 0)

# Ensure all top_actors columns exist (even if they were missing in the data)
top_actors_clean <- gsub("\\.| ", "", top_actors)  # Remove special characters to match column names
for (actor in top_actors_clean) {
  if (!(actor %in% colnames(data_test3))) {
    data_test3[[actor]] <- 0  # Add missing columns with 0 values
  }
}

##4. Group Directors 
top_directors = c("Woody Allen", "Steven Spielberg", "Clint Eastwood", "Spike Lee", "Steven Soderbergh")
data_test3$director = ifelse(data_test3$director %in% top_directors, data_test3$director, "Other directors") 

##5. Group distributors
top_distributors = c("Warner Bros.", "Universal Pictures", "Paramount Pictures", "Twentieth Century Fox", "Columbia Pictures Corporation")
data_test3$distributor = ifelse(data_test3$distributor %in% top_distributors, data_test3$distributor, "Other distributors") 

##6. Group cinematographers
top_cinemtographers = c("multiple", "Roger Deakins", "Mark Iwins", "John Bailey", "Andrew Dunn")
data_test3$cinematographer = ifelse(data_test3$cinematographer %in% top_cinemtographers, data_test3$cinematographer, "Other cinematographers")

##7. Group production companies
top_production_companies <- c("Universal Pictures", "Paramount Pictures", "Columbia Pictures Corporation", "Warner Bros.", "New Line Cinema")
data_test3$production_company <- ifelse(data_test3$production_company %in% top_production_companies, data_test3$production_company, "Others companies")

##8. Group Maturity Ratings
data_test3 = data_test3 %>%
  mutate(maturity_rating = case_when(
    maturity_rating %in% c("G", "TV-G") ~ "G",
    maturity_rating %in% c("Approved", "PG", "GP", "Passed") ~ "PG",
    maturity_rating %in% c("PG-13", "TV-14") ~ "PG-13",
    maturity_rating %in% c("R", "X", "NC-17", "M") ~ "R",
    TRUE ~ maturity_rating
  ))

## Convert categorical predictors to numerical values
data_test3$distributor = as.factor(data_test3$distributor)
data_test3$director = as.factor(data_test3$director)
data_test3$production_company = as.factor(data_test3$production_company)
data_test3$cinematographer = as.factor(data_test3$cinematographer)
data_test3$maturity_rating = as.factor(data_test3$maturity_rating)

# View updated dataset
View(data_test3)





# Install and load caret package
install.packages("caret")
library(caret)


## Model Training and Prediction
install.packages("caTools")
library(caTools)
#### TRAIN-SPLIT TEST CROSS-VALIDATION ######## 

###### Multiple Regression ######### 
split=sample.split(data3$imdb_score, SplitRatio=0.7)

train_data=subset(data3, split==TRUE)
test_data=subset(data3, split==FALSE)
imdb_model = lm(imdb_score ~ movie_budget + duration + nb_faces + release_year + release_year*movie_budget +
                  movie_meter_IMDBpro + nb_news_articles +
                  action + adventure + scifi + thriller + musical + romance + western + sport + 
                  horror + drama + war + animation + crime + maturity_rating + 
                  BillMurray + JKSimmons + RobertDeNiro + KevinSpacey + JasonStatham + Others,
                data = train_data)
# Print model summary
summary(imdb_model)

# Make predictions on test data
test_predictions <- predict(imdb_model, newdata = test_data)

# Compute Mean Squared Error (MSE) on test set
actual_values <- test_data$imdb_score
mse_test <- mean((actual_values - test_predictions)^2)
print(paste("MSE (Test Set):", mse_test))


library(lmtest)
# Check for heteroskedasticity using Breusch-Pagan Test
bptest(imdb_model)

####### Spline Regression ###### 
library(splines)
split=sample.split(data3$imdb_score, SplitRatio=0.7)

train_data=subset(data3, split==TRUE)
test_data=subset(data3, split==FALSE)
imdb_model = glm(imdb_score ~ 
      bs(duration, knots = quantile(duration, probs = c(0.25, 0.5, 0.75)), degree = 4) + 
      bs(movie_meter_IMDBpro, knots = quantile(movie_meter_IMDBpro, probs = c(0.25, 0.5, 0.75)), degree = 1)+
      bs(nb_news_articles, knots = quantile(nb_news_articles, probs = c(0.25, 0.5, 0.75)), degree = 1),
                 data = train_data)
# Print model summary
summary(imdb_model)

# Make predictions on test data
test_predictions <- predict(imdb_model, newdata = test_data)

# Compute Mean Squared Error (MSE) on test set
actual_values <- test_data$imdb_score
mse_test <- mean((actual_values - test_predictions)^2)
print(paste("MSE (Test Set):", mse_test))


library(lmtest)
# Check for heteroskedasticity using Breusch-Pagan Test
bptest(imdb_model)


###### Polynomial Regression ######### 
split=sample.split(data3$imdb_score, SplitRatio=0.7)

train_data=subset(data3, split==TRUE)
test_data=subset(data3, split==FALSE)
imdb_model = lm(imdb_score ~ movie_budget + poly(duration,2) + nb_faces + release_year + release_year*movie_budget +
                  poly(movie_meter_IMDBpro,1) + poly(nb_news_articles,1) +
                  action + adventure + scifi + thriller + musical + romance + western + sport + 
                  horror + drama + war + animation + crime + maturity_rating + 
                  BillMurray + JKSimmons + RobertDeNiro + KevinSpacey + JasonStatham + Others,
                data = train_data)
# Print model summary
summary(imdb_model)

# Make predictions on test data
test_predictions <- predict(imdb_model, newdata = test_data)

# Compute Mean Squared Error (MSE) on test set
actual_values <- test_data$imdb_score
mse_test <- mean((actual_values - test_predictions)^2)
print(paste("MSE (Test Set):", mse_test))


library(lmtest)
# Check for heteroskedasticity using Breusch-Pagan Test
bptest(imdb_model)



###### K-FOLD CROSS VALIADATION #####
######## Multiple Regression #########
mse=rep(NA, 6)  

for (i in 1:6) {
  fit=glm(imdb_score ~ movie_budget + duration + nb_faces  + 
            movie_meter_IMDBpro + nb_news_articles + release_year + release_year*movie_budget + 
            action + adventure + scifi + thriller + musical + romance + western + sport + 
            horror + drama + war + animation + crime + maturity_rating + 
            BillMurray + JKSimmons + RobertDeNiro + KevinSpacey + JasonStatham,
          data = data3)
  
  mse[i]=cv.glm(data3, fit, K=10)$delta[1]
}
mse  

###mse test without polynomial degrees
plot(mse)
lines(mse, col="red") 
which.min(mse) 
mean(mse) 


########### Spline Regression #########
mse=rep(NA, 6)  

for (i in 1:6) {
  fit=glm(imdb_score ~ 
            bs(duration, knots = quantile(duration, probs = c(0.25, 0.5, 0.75)), degree = 4) + 
            bs(movie_meter_IMDBpro, knots = quantile(movie_meter_IMDBpro, probs = c(0.25, 0.5, 0.75)), degree = 1)+
            bs(nb_news_articles, knots = quantile(nb_news_articles, probs = c(0.25, 0.5, 0.75)), degree = 1),
          data = data3)
  
  mse[i]=cv.glm(data3, fit, K=10)$delta[1]
}
mse  

###mse test without polynomial degrees
plot(mse)
lines(mse, col="red") 
which.min(mse) 
mean(mse) 


########### Polynomial Regression ######
mse=rep(NA, 6)  

for (i in 1:6) {
  fit=glm(imdb_score ~ movie_budget + poly(duration,2) + nb_faces  + 
            poly(movie_meter_IMDBpro,1) + poly(nb_news_articles,1) + release_year + release_year*movie_budget + 
            action + adventure + scifi + thriller + musical + romance + western + sport + 
            horror + drama + war + animation + crime + maturity_rating + 
            BillMurray + JKSimmons + RobertDeNiro + KevinSpacey + JasonStatham,
          data = data3)
  
  mse[i]=cv.glm(data3, fit, K=10)$delta[1]
}
mse 

###mse test without polynomial degrees
plot(mse)
lines(mse, col="red") 
which.min(mse) 
mean(mse) 


                                           ##### FINAL MODEL  #####


imdb_model = lm(imdb_score ~ movie_budget + poly(duration,2) + nb_faces + release_year + release_year*movie_budget +
                  poly(movie_meter_IMDBpro,1) + poly(nb_news_articles,1) +
                  action + adventure + scifi + thriller + musical + romance + western + sport + 
                  horror + drama + war + animation + crime + maturity_rating + 
                  BillMurray + JKSimmons + RobertDeNiro + KevinSpacey + JasonStatham + Others,
                data = data3)

model_summary = summary(imdb_model)
model_summary


# Calculate MSE (Mean Squared Error) using actual and predicted values
actual= data3$imdb_score
prediction = predict(imdb_model, data_test3)


squared_error = (actual-prediction)^2
mse = mean(squared_error)
print(paste("MSE : ",mse))
print(paste("R-squared:", model_summary$r.squared))
print(paste("Adjusted R-squared:", model_summary$adj.r.squared))



######## SCORES PREDICTION FOR 12 MOVIES #########

prediction = predict(imdb_model, newdata = data_test3)

# Add predicted scores to the data_test3 dataset
data_test3$predicted_score = prediction

# View a preview of actual vs. predicted IMDb scores
head(data_test3[c("movie_title", "imdb_score", "predicted_score")], 20)

library(tidyverse)
library(rpart)
library(rpart.plot)
library(moments)
library(caret) 
library(corrplot)

######### PART 1. Read in the data
fruits <- read_csv("/Users/spencerthompson/Downloads/Fruits_Consumptions_Habits.csv")

######### PART 2. Clean data
### 2.1: rename columns
fruits <- fruits %>% 
  rename(frequency = `Frequency of Consumption`,
         form = `Preferred Form of Fruit`,
         favorite = `Favorite Fruit`,
         age = Age,
         gender = Gender,
         country = Country) %>%
  select(-ID)

### 2.2: Add Financial Data
# Create a column called price using the research data
fruits <- fruits %>%
  mutate(price = case_when(
    favorite == "Banana" & country == "France" ~ 0.55,
    favorite == "Apple" & country == "France" ~ 0.88,
    favorite == "Orange" & country == "France" ~ 0.94,
    favorite == "Banana" & country == "Germany" ~ 0.35,
    favorite == "Apple" & country == "Germany" ~ 0.84,
    favorite == "Orange" & country == "Germany" ~ 0.65,
    favorite == "Banana" & country == "UK" ~ 0.36,
    favorite == "Apple" & country == "UK" ~ 0.8,
    favorite == "Orange" & country == "UK" ~ 0.8,
    favorite == "Banana" & country == "Australia" ~ 0.66,
    favorite == "Apple" & country == "Australia" ~ 0.96,
    favorite == "Orange" & country == "Australia" ~ 0.93,
    favorite == "Banana" & country == "Canada" ~ 0.36,
    favorite == "Apple" & country == "Canada" ~ 1.26,
    favorite == "Orange" & country == "Canada" ~ 1.14,
    favorite == "Banana" & country == "USA" ~ 0.42,
    favorite == "Apple" & country == "USA" ~ 1.59,
    favorite == "Orange" & country == "USA" ~ 1.35
  ))
# Create a column called yearly_revenue that is the price * frequency * 52
fruits <- fruits %>%
  mutate(annual_revenue = price * case_when(
    frequency == "Daily" ~ 7,
    frequency == "4-5 times a week" ~ 4.5,
    frequency == "3-4 times a week" ~ 3.5,
    frequency == "2-3 times a week" ~ 2.5,
    frequency == "Once a week" ~ 1
  ) * 52)
# Create a df for the revenue data
fruits_revenue <- fruits %>%
  filter(favorite == "Banana" | favorite == "Orange" | favorite == "Apple") %>%
  filter(form != "Juice")

### 2.3: create a numerical dataframe to be used in clustering
fruits_num <- fruits %>%
  mutate(
    form = ifelse(form %in% c("Fresh", "Salad"), 1, 2),
    frequency = case_when(
      frequency %in% c("Daily", "4-5 times a week") ~ 1,
      frequency %in% c("3-4 times a week", "2-3 times a week") ~ 2,
      frequency == "Once a week" ~ 3
    ),
    country = case_when(
      country %in% c("Australia") ~ 1,
      country %in% c("Canada", "USA") ~ 3,
      country %in% c("France", "Germany", "UK") ~ 5
    ),
    gender = ifelse(gender == "Male", 0, 1),
    favorite = match(favorite, c("Strawberry", "Orange", "Mango", "Banana", "Apple"))
  )
# Create dummy variables
fruits_num <- fruits_num %>%
  mutate(
    country_na = as.integer(country == 3),
    country_eu = as.integer(country == 5),
    country_au = as.integer(country == 1),
    very_freq = as.integer(frequency == 1),
    freq = as.integer(frequency == 2),
    infreq = as.integer(frequency == 3),
    fresh = as.integer(form == 1),
    juice = as.integer(form == 2)
  )


######### PART 3. Exploratory Data Analysis
### 3.1: General Overview of Data
summary(fruits)
### 3.2: Data Viz
# Histogram of age
hist(fruits_num$age, main = "Age Distribution", xlab = "Age")

# average age by country
fruits %>%
  group_by(country) %>%
  summarize(mean_age = mean(age), sd_age = sd(age)) %>%
  ggplot(aes(x = country, y = mean_age)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_age - sd_age, ymax = mean_age + sd_age), width = 0.2) +
  labs(title = "Mean Age by Country", x = "Country", y = "Mean Age")

# average age by frequency
fruits %>%
  group_by(frequency) %>%
  summarize(mean_age = mean(age), sd_age = sd(age)) %>%
  ggplot(aes(x = frequency, y = mean_age)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_age - sd_age, ymax = mean_age + sd_age), width = 0.2) +
  labs(title = "Mean Age by Frequency", x = "Frequency", y = "Mean Age")

# average age by favorite fruit
fruits %>%
  group_by(favorite) %>%
  summarize(mean_age = mean(age), sd_age = sd(age)) %>%
  ggplot(aes(x = favorite, y = mean_age)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_age - sd_age, ymax = mean_age + sd_age), width = 0.2) +
  labs(title = "Mean Age by Favorite Fruit", x = "Favorite Fruit", y = "Mean Age")

######### Part 4. T-Tests
### 4.1: T-Test 2 Sample Mean for Age
banana_age <- fruits %>%
  filter(favorite == "Banana") %>%  
  select(age) %>%               
  mean(age)                       

### 4.2: How does form affect frequency
daily <- fruits %>%
  filter(frequency == "Daily")
daily_form <- table(daily$form)
daily_form
porportion_1 <- 126 / 314
porportion_1

non_daily <- fruits %>%
  filter(frequency != "Daily")
non_daily_form <- table(non_daily$form)
non_daily_form
porportion_2 <- 408 / 1123
porportion_2
### make a t-test using the prop.test function. 2 sample
prop.test(c(126, 408), c(314, 1123))

### 4.3: How does favorite affect form
### 4.4: How does favorite affect frequency
strawberry_freq <- fruits %>%
  filter(favorite == "Strawberry") %>%
  select(frequency) %>%
  table()


######### PART 5. K-Means Demographic Clustering
### 5.1: Scale the data. Remove non-numeric columns
clustering <- fruits_num %>%
  select(-favorite, -country, -frequency, -form, -country_au, -price, -annual_revenue, -fresh)
clustering_scaled <- scale(clustering)

### 5.2: Elbow Method
wcss <- numeric(12)
for (i in 1:12) wcss[i] <- sum(kmeans(clustering_scaled, centers = i)$withinss)
plot(1:12, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

### 5.3: K-Means Clustering
set.seed(135)
centers <- 4
kmeans_fruits <- kmeans(clustering_scaled, centers = centers)
fruits_num$demo_cluster <- kmeans_fruits$cluster
clustering$demo_cluster <- kmeans_fruits$cluster
fruits$demo_cluster <- kmeans_fruits$cluster

### 5.4: Cluster Analysis
table(fruits$form)
# Centers
centers_df <- as.data.frame(kmeans_fruits$centers)
centers_df$cluster <- 1:centers
centers_df$count <- fruits_num %>%
  group_by(demo_cluster) %>%
  summarize(count = n()) %>%
  select(count) %>%
  unlist()
# Distribution
fruits_num %>%
  group_by(demo_cluster) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = demo_cluster, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Cluster Distribution", x = "Cluster", y = "Count")
# Cluster 4: Target Market
cluster_4 <- fruits %>%
  filter(demo_cluster == 4)
table(cluster_4$favorite)
boxplot(cluster_4$age, main = "Cluster 4 Age Distribution", xlab = "Age")
boxplot(fruits$age, main = "Overall Age Distribution", xlab = "Age")
# sum the very freq column
table(cluster_4$form)
########## Part 7. Decision Tree for Favorite Fruit Prediction
# 7.1: Partition the Data
fruits_num <- fruits_num %>% mutate(strawberry = ifelse(favorite == 1, 1, 0))
set.seed(123) 
train_index <- createDataPartition(fruits$age, p = 0.70, list = FALSE)
dt_train <- fruits_num[train_index, ]
dt_test <- fruits_num[-train_index, ]

# 7.2: Fit the model
tree_model <- rpart(strawberry ~ country_na + country_eu + freq + infreq + very_freq + gender + fresh + age, 
                    data = dt_train, method = "class")
predictions <- predict(tree_model, dt_test, type = "class")

# 7.3: Analyze the tree
rpart.plot(tree_model, box.palette = "RdBu", tweak = 1.1)
printcp(tree_model)
# Confusion matrix
confusion_matrix <- table(predictions, dt_test$strawberry)
confusion_matrix
# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy


########## Part 8. Linear Regression for Annual Revenue Prediction
# 8.1: Create dummy variables
names(fruits_revenue)
fruits_revenue <- fruits_revenue %>%
  mutate(
    country_na = as.integer(country == "USA" | country == "Canada"),
    country_eu = as.integer(country == "France" | country == "Germany" | country == "UK"),
    country_au = as.integer(country == "Australia"),
    very_freq = as.integer(frequency == "Daily" | frequency == "4-5 times a week"),
    freq = as.integer(frequency == "3-4 times a week" | frequency == "2-3 times a week"),
    infreq = as.integer(frequency == "Once a week"),
    gender = as.integer(gender == "Female"),
    fresh = as.integer(form == "Fresh" | form == "Salad"),
    juice = as.integer(form == "Juice" | form == "Smoothie")
  ) %>%
  select(-country, -frequency, -form, -favorite, -price, -country_au, -infreq, -juice)

# 8.2: Partition the Data
set.seed(123)
train_index <- createDataPartition(fruits_revenue$age, p = 0.75, list = FALSE)
lr_train <- fruits_revenue[train_index, ]
lr_test <- fruits_revenue[-train_index, ]

# 8.3: Correlation Matrix
cor(fruits_revenue)
cor_matrix <- cor(fruits_revenue)
corrplot(cor_matrix, method = "color")

# 8.4: Fit the model
lm_model <- lm(annual_revenue ~ age + country_na + country_eu + very_freq + freq + gender + fresh, data = lr_train)
summary(lm_model)

# 8.5: Make Predictions
predictions <- predict(lm_model, lr_test)
mse <- mean((lr_test$annual_revenue - predictions)^2)
mse
rmse <- sqrt(mse)
rmse
# Save the predictions as a column in the test data
lr_test$predicted_revenue <- predictions

# Plot the predicted revenue vs the actual revenue
ggplot(lr_test, aes(x = annual_revenue, y = predicted_revenue)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual Revenue", x = "Actual Revenue", y = "Predicted Revenue")
names(lr_test)

# 8.6: Use our own data
# Create a dataframe and populate it with our data
my_data <- data.frame(
  age = c(20, 22), gender = c(0, 1), country_na = c(1, 1), country_eu = c(0, 0), very_freq = c(1, 0), freq = c(0, 1), fresh = c(1, 0))
# Predict my expected revenue
my_prediction <- predict(lm_model, my_data)
my_prediction

  



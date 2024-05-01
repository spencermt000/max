library(tidyverse)
library(rpart)
library(moments)
library(caret) 

######### PART 1. Read in the data
fruits <- read_csv("/Users/spencerthompson/Downloads/Fruits_Consumptions_Habits.csv")

######### PART 2. Clean data
# 2.1: rename columns
fruits <- fruits %>% 
  rename(frequency = `Frequency of Consumption`,
         form = `Preferred Form of Fruit`,
         favorite = `Favorite Fruit`,
         age = Age,
         gender = Gender,
         country = Country) %>%
  select(-ID)

# 2.2: create a numerical dataframe to be used in clustering
fruits_num <- fruits %>%
  mutate(
    form = case_when(
      form == "Fresh" ~ 0,
      form == "Juice" ~ 1,
      form == "Salad" ~ 3,
      form == "Smoothie" ~ 2
    ),
    frequency = case_when(
      frequency == "Daily" ~ 1,
      frequency == "4-5 times a week" ~ 1,
      frequency == "3-4 times a week" ~ 2,
      frequency == "2-3 times a week" ~ 2, 
      frequency == "Once a week" ~ 3
    ),
    country = case_when(
      country == "Australia" ~ 1,
      country == "Canada" ~ 3,
      country == "France" ~ 5,
      country == "Germany" ~ 5,
      country == "UK" ~ 5,
      country == "USA" ~ 3,
      
    ),
    gender = case_when(
      gender == "Male" ~ 0,
      gender == "Female" ~ 1
    ),
    favorite = case_when(
      favorite == "Strawberry" ~ 1,
      favorite == "Orange" ~ 2,
      favorite == "Mango" ~ 3,
      favorite == "Banana" ~ 4,
      favorite == "Apple" ~ 5
    )
  )

# make a column called country_na that is 1 if country is == 3. 
fruits_num$country_na <- ifelse(fruits_num$country == 3, 1, 0)
fruits_num$country_eu <- ifelse(fruits_num$country == 5, 1, 0)
fruits_num$country_au <- ifelse(fruits_num$country == 1, 1, 0)

fruits_num$very_freq <- ifelse(fruits_num$frequency == 1, 1, 0)
fruits_num$freq <- ifelse(fruits_num$frequency == 2, 1, 0)
fruits_num$infreq <- ifelse(fruits_num$frequency == 3, 1, 0)


######### PART 3. Exploratory Data Analysis
# 3.1: General Overview of Data
summary(fruits)
# 3.2: Data Viz
# histogram of age
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
# 4.1: T-Test 2 Sample Mean for Age
banana_age <- fruits %>%
  filter(favorite == "Banana") %>%  
  select(age) %>%               
  mean(age)                       


# 4.2: How does form affect frequency
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

# make a t-test using the prop.test function. 2 sample
prop.test(c(126, 408), c(314, 1123))


# 4.3: How does favorite affect form




# 4.4: How does favorite affect frequency
strawberry_freq <- fruits %>%
  filter(favorite == "Strawberry") %>%
  select(frequency) %>%
  table()


######### PART 5. K-Means Demographic Clustering
# 5.1: Scale the data. 
fruits_num2 <- fruits_num
fruits_num <- fruits_num %>%
  select(-favorite, -country, -frequency)
fruits_num_scaled <- scale(fruits_num)

# 5.2: Elbow Method
wcss <- numeric(12)
for (i in 1:12) wcss[i] <- sum(kmeans(fruits_num_scaled, centers = i)$withinss)
plot(1:12, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

# 5.3: K-Means Clustering
set.seed(123)
centers <- 8
kmeans_fruits <- kmeans(fruits_num_scaled, centers = centers)
fruits_num$demo_cluster <- kmeans_fruits$cluster
fruits_num2$demo_cluster <- kmeans_fruits$cluster
fruits$demo_cluster <- kmeans_fruits$cluster

# 5.4: Cluster Analysis
kmeans_fruits$centers
centers_df <- as.data.frame(kmeans_fruits$centers)

fruits_num %>%
  group_by(demo_cluster) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = demo_cluster, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Cluster Distribution", x = "Cluster", y = "Count")

########## Part 6. T-tests for clusters
# 6.1: Cluster 1
cluster_1 <- fruits_num2 %>%
  filter(demo_cluster == 1) %>%
  select(favorite)
table(cluster_1$favorite)
cluster_1_n <- nrow(cluster_1)
cluster_1_fav <- nrow(cluster_1 %>% filter(favorite == 3))
# make a t-test using the prop.test function
prop.test(cluster_1_fav, cluster_1_n, p = 0.25)

# 6.2: Cluster 8
cluster_8 <- fruits_num2 %>%
  filter(demo_cluster == 8) %>%
  select(favorite)
table(cluster_8$favorite)
cluster_8_n <- nrow(cluster_8)
cluster_8_fav <- nrow(cluster_8 %>% filter(favorite == 1))
# make a t-test using the prop.test function
prop.test(cluster_8_fav, cluster_8_n, p = 0.25)





########## Part 7. Decision Tree for Favorite Fruit Prediction
# 7.1: Split the data
fruits_num2 <- fruits_num2 %>% mutate(strawberry = ifelse(favorite == 1, 1, 0))

# 7.2: Decision Tree
set.seed(123) 
train_index <- createDataPartition(fruits$age, p = 0.8, list = FALSE)
train <- fruits_num2[train_index, ]
test <- fruits_num2[-train_index, ]

tree_model <- rpart(strawberry ~ country_na + country_eu + country_au + freq + infreq + very_freq + gender + form + age, data = train, method = "class")
predictions <- predict(tree_model, test, type = "class")

# 7.3: Analyze the tree
plot(tree_model)
text(tree_model, use.n = TRUE)
# Confusion matrix
confusion_matrix <- table(predictions, test$strawberry)
confusion_matrix
# accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy













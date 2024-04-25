library(tidyverse)

#### IMPORT DATA
fruits <- read_csv("/Users/spencerthompson/Downloads/Fruits_Consumptions_Habits.csv")
fruits_raw <- read_csv("/Users/spencerthompson/Downloads/Fruits_Consumptions_Habits.csv")

#### CLEAN UP DATA
fruits <- fruits %>% rename(Frequency = `Frequency of Consumption`,
                            Form = `Preferred Form of Fruit`,
                            Favorite = `Favorite Fruit`)

fruits_raw <- fruits_raw %>% rename(Frequency = `Frequency of Consumption`,
                            Form = `Preferred Form of Fruit`,
                            Favorite = `Favorite Fruit`)
# change the form column to numeric data
fruits <- fruits %>% 
  mutate(Form = case_when(Form == "Fresh" ~ 0,
                          Form == "Juice" ~ 1,
                          Form == "Salad" ~ 2,
                          Form == "Smoothie" ~ 3))
# change frequency to numeric data
fruits <- fruits %>% 
  mutate(Frequency = case_when(Frequency == "Daily" ~ 1,
                               Frequency == "Once a week" ~ 5,
                               Frequency == "4-5 times a week" ~ 2,
                               Frequency == "3-4 times a week" ~ 3,
                               Frequency == "2-3 times a week" ~ 4))
# change country to numeric
fruits <- fruits %>% 
  mutate(Country = case_when(Country == "Australia" ~ 0,
                             Country == "Canada" ~ 1,
                             Country == "France" ~ 2,
                             Country == "Germany" ~ 3,
                             Country == "UK" ~ 4,
                             Country == "US" ~ 5)) %>%
  mutate(Country = ifelse(is.na(Country), 6, Country))
# change gender to binary column
fruits <- fruits %>% 
  mutate(Gender = case_when(Gender == "Male" ~ 0,
                               Gender == "Female" ~ 1))
# change favorite to numeric                              
fruits <- fruits %>% 
  mutate(Favorite = case_when(Favorite == "Strawberry" ~ 1,
                              Favorite == "Orange" ~ 5,
                              Favorite == "Mango" ~ 2,
                              Favorite == "Banana" ~ 3,
                              Favorite == "Apple" ~ 4)) %>%
  select(-ID)


##### CLUSTERING
scaled <- scale(fruits)
kmeans(scaled, centers = 6)
fruits$cluster <- kmeans(scaled, centers = 6)$cluster
fruits_raw$cluster <- kmeans(scaled, centers = 6)$cluster
kmeans(scaled, centers = 6)$centers
# viz
hist(fruits$cluster)
hist(fruits$Frequency)


### looking at each favorite fruit
######## mango 
mango <- fruits %>%
  filter(Favorite == 2)
mango_mean_age <- mean(mango$Age)
mango_mean_age
table(mango$Gender)
table(mango$Country)
hist(mango$Form)
######## strawberry
strawberry <- fruits %>%
  filter(Favorite == 1)
strawberry_mean_age <- mean(strawberry$Age)
strawberry_mean_age
table(strawberry$Gender)
table(strawberry$Country)
hist(strawberry$Form)
######## orange
orange <- fruits %>%
  filter(Favorite == 5)
orange_mean_age <- mean(orange$Age)
orange_mean_age
table(orange$Gender)
table(orange$Country)
hist(orange$Form)
######## banana
banana <- fruits %>%
  filter(Favorite == 3)
banana_mean_age <- mean(banana$Age)
banana_mean_age
table(banana$Gender)
table(banana$Country)
hist(banana$Form)
######## apple
apple <- fruits %>%
  filter(Favorite == 4)
apple_mean_age <- mean(apple$Age)
apple_mean_age
table(apple$Gender)
table(apple$Country)
hist(apple$Form)

#### AGE and FREQUENCY
hist(fruits$Age)
freq_5 <- fruits %>%
  filter(Frequency == 5)
prop.table(table(freq_5$Age))
hist(freq_5$Age)
freq_4 <- fruits %>%
  filter(Frequency == 4)
prop.table(table(freq_4$Age))
hist(freq_4$Age)
freq_3 <- fruits %>%
  filter(Frequency == 3)
prop.table(table(freq_3$Age))
hist(freq_3$Age)
freq_2 <- fruits %>%
  filter(Frequency == 2)
prop.table(table(freq_2$Age))
hist(freq_2$Age)
freq_1 <- fruits %>%
  filter(Frequency == 1)
prop.table(table(freq_1$Age))
hist(freq_1$Age)

#### FREQUENCY AND FAVORITE FRUIT
hist(freq_1$Favorite)
hist(freq_2$Favorite)
hist(freq_3$Favorite)
hist(freq_4$Favorite)
hist(freq_5$Favorite)
#### FORM
hist(fruits$Form)
form_0 <- fruits %>%
  filter(Form == 0)
hist(form_0$Favorite)
form_1 <- fruits %>%
  filter(Form == 1)
hist(form_1$Favorite)
form_2 <- fruits %>%
  filter(Form == 2)
hist(form_2$Favorite)
form_3 <- fruits %>%
  filter(Form == 3)
hist(form_3$Favorite)

#write fruits to a csv called fruits_cleaned.csv
write_csv(fruits, "/Users/spencerthompson/Downloads/fruits_cleaned.csv")
write_csv(fruits_raw, "/Users/spencerthompson/Downloads/fruits_raw.csv")
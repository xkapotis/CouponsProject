library(tidyverse)
library(janitor)
### for installing packages also needed ("  ,dependencies = TRUE, method = "wininet" ")
df <- read.csv("C:/Users/ckapotis/Desktop/MSc Digital Transformation/Customer Analytics/Assignment/coupons.csv")

glimpse(df)
summary(df)


### Step 1: Data Cleaning ###

df <- clean_names(df)
#names(df) to see if everything is lowecase after the clean_names command

# Replace "nan" with NA
df[df == "nan"] <- NA

# Convert categorical variables to factor
df <- df %>%
  mutate(across(where(is.character), as.factor))
##Need categorical variables to be stored as factors, not character strings. So we transformed all categorical text variables into categorical factors. ##

# Convert Target Variable to Factor
df$y <- as.factor(df$y)


#Why?
#Because:
  # If y is numeric (0/1), some models treat it as regression.
  # If it is factor → classification.

# Remove duplicates
df <- distinct(df)

# Check missing values
colSums(is.na(df))

table(df$y)
prop.table(table(df$y))
#they return 0 1 and sum how many zeros and ones
#and the prop the same but perc in based total count results
#check if the dataset is imbalanced.
#This is extremely important for modeling quality.

### Step 1: Data Cleaning ###



### Step 2: Exploratory Data Analysis EDA ###

# 2.1 Coupon Type vs Acceptance
ggplot(df, aes(x = coupon, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Coupon Acceptance Rate by Coupon Type",
       x = "Coupon Type",
       y = "Proportion",
       fill = "Accepted (1)") +
  theme_minimal()

# 2.2 Time of Day vs Acceptance
ggplot(df, aes(x = time, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Coupon Acceptance by Time of Day",
       x = "Time",
       y = "Proportion",
       fill = "Accepted (1)") +
  theme_minimal()

# 2.3 Income vs Acceptance
ggplot(df, aes(x = income, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Coupon Acceptance by Income Level",
       x = "Income",
       y = "Proportion",
       fill = "Accepted (1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2.4 Distance vs Acceptance
ggplot(df, aes(x = as.factor(to_coupon_geq15min), fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Acceptance by Distance (>=15 min)",
       x = "Distance ≥ 15 minutes",
       y = "Proportion",
       fill = "Accepted (1)") +
  theme_minimal()

# 2.5 CoffeeHouse Visits vs Acceptance
ggplot(df, aes(x = coffee_house, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Acceptance by Coffee House Visit Frequency",
       x = "Monthly Coffee Visits",
       y = "Proportion",
       fill = "Accepted (1)") +
  theme_minimal()



### Step 2: Exploratory Data Analysis ###


### Step 3: Predictive Modeling ###

set.seed(123)

library(caret)

trainIndex <- createDataPartition(df$Y, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

# Logistic Regression
log_model <- glm(Y ~ ., data = train, family = binomial)

# Predictions
pred <- predict(log_model, test, type = "response")


### Step 3: Predictive Modeling ###






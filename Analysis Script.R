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

table(df$car, useNA = "always")


# remove car column #
library(tidyverse)

df <- df %>%
  select(-car)
# remove car column #

# Transform the expiration values in hours #

library(dplyr)
library(stringr)

# Suppose your dataframe is called df and your column is 'expiration'

# Function to convert any single value to hours
convert_to_hours <- function(x) {
  # Extract all number-unit pairs (works for compound like "1d 2h 30m")
  matches <- str_match_all(x, "(\\d+\\.?\\d*)\\s*([a-zA-Z]+)")[[1]]
  
  if(nrow(matches) == 0) return(NA_real_)  # fallback if no match
  
  total_hours <- 0
  
  for(i in 1:nrow(matches)) {
    num <- as.numeric(matches[i,2])
    unit <- tolower(matches[i,3])
    
    total_hours <- total_hours + switch(unit,
                                        "m" = num / 60,
                                        "h" = num,
                                        "d" = num * 24,
                                        "w" = num * 24 * 7,
                                        NA_real_)
  }
  
  return(total_hours)
}

# Apply function to your whole column dynamically
df <- df %>%
  mutate(
    expiration_original = expiration,   # backup column
    expiration = vapply(expiration, convert_to_hours, numeric(1))
  )

# Check result
head(df)

df_bckup <- df
#df <- df_bckup

# Transform the expiration values in hours #

# transform time to time hour numeric from AM/PM #

library(dplyr)
library(lubridate)

df <- df %>%
  mutate(time_original = time,
         time = hour(parse_date_time(time, orders = "I!p")))
# transform time to time hour numeric from AM/PM #

# check if a column is factor or not #
class(df$age)

df$age <- factor(df$age,
                 levels = c("below21","21","26","31","36","41","46","50plus"),
                 ordered = TRUE) #Age is numeric — not categorical. Otherwise the model treats age as categories instead of a continuous variable.
table(df$age, useNA = "always")
#Age was treated as an ordinal variable since the dataset provides age groups rather than continuous values.


# replace "" with NAs and remove them 
sapply(df[, c("bar",
              "coffee_house",
              "carry_away",
              "restaurant_less_than20",
              "restaurant20to50")],
       function(x) table(x, useNA = "always"))

freq_cols <- c("bar",
               "coffee_house",
               "carry_away",
               "restaurant_less_than20",
               "restaurant20to50")

df[freq_cols] <- lapply(df[freq_cols], function(x) {
  x[x == ""] <- NA
  x
})

df[freq_cols] <- lapply(df[freq_cols], droplevels) # drop unused levels pair with the above one next command

sapply(df[freq_cols], function(x) table(x, useNA = "always"))

colSums(is.na(df)) # count NAs per column 100 - 200 per column so small percentage based ~5% so we can delete the rows

df <- na.omit(df) #delete rows "Observations with missing frequency values were removed due to their low proportion in the dataset (<5%)."


# We should convert frequency variables to ordered factors:
df[freq_cols] <- lapply(df[freq_cols], function(x) {
  factor(x,
         levels = c("never","less1","1~3","4~8","gt8"),
         ordered = TRUE)
})

str(df$coffee_house)

# drop the cols we kept for backup
df$expiration_original <- NULL
df$time_original  <- NULL

# i will create two y y_numeric and y_factor the numeric will be for  plotting and trends and y_factor will be the target variable for modeling (classification).
df <- df %>%
  mutate(
    y_numeric = as.numeric(as.character(y)),
    y_factor = factor(y, levels = c(0,1))
  )

### Step 1: Data Cleaning ###



### Step 2: Exploratory Data Analysis EDA ###

#2.1 Coupon Type vs Acceptance Rate (Core Business Insight)
df %>%
  group_by(coupon) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(coupon, accept_rate), y = accept_rate)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(title = "Coupon Acceptance Rate by Coupon Type",
       x = "Coupon Type",
       y = "Acceptance Rate") +
  theme_minimal()

#2.2 Acceptance Rate by Time of Day (Behavioral Timing Insight)
df %>%
  group_by(time) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = accept_rate, group = 1)) +
  geom_line(color = "#e41a1c", size = 1.2) +
  geom_point(size = 3, color = "#e41a1c") +
  labs(title = "Coupon Acceptance Rate by Time of Day",
       x = "Time of Day",
       y = "Acceptance Rate") +
  theme_minimal()

#2.3 Income vs Acceptance (Customer Affordability Pattern)
df %>%
  group_by(income) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = income, y = accept_rate)) +
  geom_col(fill = "#4daf4a") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Acceptance Rate by Income Level",
       x = "Income Level",
       y = "Acceptance Rate")

#2.4 Coffee House Visits vs Acceptance (Behavioral Engagement Signal)
df %>%
  group_by(coffee_house) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = coffee_house, y = accept_rate)) +
  geom_col(fill = "#984ea3") +
  labs(title = "Acceptance Rate by Coffee Visit Frequency",
       x = "Monthly Coffee Visits",
       y = "Acceptance Rate") +
  theme_minimal()

#2.5 Heatmap – Coupon Type × Time (Segmentation Insight)
df %>%
  group_by(coupon, time) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = coupon, y = time, fill = accept_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Acceptance Rate: Coupon Type by Time of Day",
       x = "Coupon Type",
       y = "Time of Day",
       fill = "Acceptance Rate") +
  theme_minimal()

### Step 2: Advanced EDA ###



###################################
### Step 3: Predictive Modeling ###
###################################

#We will use 3 models:  Logistic Regression, Decision Tree, Random Forest
#Remove y_numeric
#Keep y_factor as target
#Remove irrelevant columns (IDs etc.)

library(caret)
library(dplyr)

# Keep modeling dataset
df_clean <- df %>%
  select(-y, -y_numeric)



# Remove rows with NA
df_clean <- na.omit(df_clean)


# Train/Test Split (70% / 30%)
set.seed(123)

train_index <- createDataPartition(df_clean$y_factor, p = 0.7, list = FALSE)

train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]



str(train_data)
summary(train_data)
colnames(train_data)
nzv[nzv$nzv == TRUE, ]

#positice class -> 1 not 0
train_data$y_factor <- relevel(train_data$y_factor, ref = "1")
test_data$y_factor  <- relevel(test_data$y_factor, ref = "1")

#train_data$y_factor <- factor(train_data$y_factor, levels = c("0","1"))
#test_data$y_factor  <- factor(test_data$y_factor,  levels = c("0","1"))

# Check Variance
library(caret)

nzv <- nearZeroVar(train_data, saveMetrics = TRUE)

#show what to remove
nzv[nzv$zeroVar == TRUE, ]

train_data <- train_data[, !nzv$zeroVar]
test_data  <- test_data[, !nzv$zeroVar]

# Remove collinearity variable
train_data$direction_opp <- NULL
test_data$direction_opp  <- NULL


# Replace 'time' with categorical version directly
train_data <- train_data %>%
  mutate(time = case_when(
    time < 12 ~ "morning",
    time >= 12 & time < 18 ~ "afternoon",
    TRUE ~ "evening"
  )) %>%
  mutate(time = factor(time, levels=c("morning","afternoon","evening")))

test_data <- test_data %>%
  mutate(time = case_when(
    time < 12 ~ "morning",
    time >= 12 & time < 18 ~ "afternoon",
    TRUE ~ "evening"
  )) %>%
  mutate(time = factor(time, levels=c("morning","afternoon","evening")))

train_data <- train_data %>%
  mutate(age = case_when(
    age %in% c("below21","21","26","31","36") ~ "middle",
    age %in% c("41","46","50plus") ~ "senior",
    TRUE ~ NA_character_  # just in case
  )) %>%
  mutate(age = factor(age, levels=c("middle","senior")))

test_data <- test_data %>%
  mutate(age = case_when(
    age %in% c("below21","21","26","31","36") ~ "middle",
    age %in% c("41","46","50plus") ~ "senior",
    TRUE ~ NA_character_
  )) %>%
  mutate(age = factor(age, levels=c("middle","senior")))

train_data$coffee_bar <- as.numeric(train_data$coffee_house) * as.numeric(train_data$bar)
test_data$coffee_bar <- as.numeric(test_data$coffee_house) * as.numeric(test_data$bar)

library(caret)



### Model 1 – Logistic Regression ###

#check
#library(car)
#log_temp <- glm(y_factor ~ ., data = train_data, family = binomial)
#vif(log_temp)

#levels(test_data$y_factor)

model_log <- glm(y_factor ~ ., 
                 data = train_data,
                 family = binomial)

prob_log <- predict(model_log, test_data, type = "response")

# Reverse threshold
pred_log <- ifelse(prob_log > 0.5, 0, 1)

pred_log <- factor(pred_log, levels = c(0,1))

confusionMatrix(pred_log, test_data$y_factor)

library(pROC)

roc_log <- roc(test_data$y_factor, prob_log)
auc(roc_log)

### Model 1 – Logistic Regression ###

### Model 2 – Decision Tree ###

library(rpart)
library(rpart.plot)

model_tree <- rpart(y_factor ~ ., 
                    data = train_data,
                    method = "class")

rpart.plot(model_tree)

pred_tree <- predict(model_tree, test_data, type = "class")

confusionMatrix(pred_tree, test_data$y_factor)

### Model 2 – Decision Tree ###

### Model 3 – Random Forest ###

library(randomForest)

model_rf <- randomForest(y_factor ~ ., 
                         data = train_data,
                         ntree = 300)

pred_rf <- predict(model_rf, test_data)

confusionMatrix(pred_rf, test_data$y_factor)

# AUC value for Random Forest #
library(pROC)

rf_probs <- predict(model_rf, test_data, type = "prob")[,2]

roc_rf <- roc(test_data$y_factor, rf_probs)

auc(roc_rf)
plot(roc_rf)
# AUC value for Random Forest #

### Model 3 – Random Forest ###

### Add cross - validation ###
train_control <- trainControl(method = "cv", number = 5)

model_rf_cv <- train(y_factor ~ ., 
                     data = train_data,
                     method = "rf",
                     trControl = train_control)

model_rf_cv
### Add cross - validation ###

#plot all ROC curves on one graph (Logistic Regression, Decision Tree, Random Forest) for proper visual comparison.
library(pROC)

# Logistic Regression probabilities (probability of class 1)
log_probs <- predict(model_log, test_data, type = "response")

# Decision Tree probabilities
tree_probs <- predict(model_tree, test_data, type = "prob")[,2]

# Random Forest probabilities
rf_probs <- predict(model_rf, test_data, type = "prob")[,2]

#Step 2: Create ROC Objects
roc_log  <- roc(test_data$y_factor, log_probs)
roc_tree <- roc(test_data$y_factor, tree_probs)
roc_rf   <- roc(test_data$y_factor, rf_probs)

#Step 3: Plot All ROC Curves on One Graph
plot(roc_log, 
     col = "blue", 
     lwd = 2,
     main = "ROC Curve Comparison")

plot(roc_tree, 
     col = "green", 
     lwd = 2,
     add = TRUE)

plot(roc_rf, 
     col = "red", 
     lwd = 2,
     add = TRUE)

legend("bottomright",
       legend = c(
         paste("Logistic Regression (AUC =", round(auc(roc_log),3),")"),
         paste("Decision Tree (AUC =", round(auc(roc_tree),3),")"),
         paste("Random Forest (AUC =", round(auc(roc_rf),3),")")
       ),
       col = c("blue", "green", "red"),
       lwd = 2)

###################################
### Step 3: Predictive Modeling ###
###################################

head(train_data)






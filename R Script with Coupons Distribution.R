





###########################
###########################
#### Coffee house #########
###########################
###########################


library(janitor)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(pROC)


### for installing packages also needed ("  ,dependencies = TRUE, method = "wininet" ")
df <- read.csv("./dataset/coupons.csv")


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
df <- df %>%
  select(-car)
# remove car column #

# Transform the expiration values in hours #
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


########################################
########################################
########################################
### Step 3: Predictive Modeling ########
########################################
########################################
########################################

#We will use 3 models:  Logistic Regression, Decision Tree, Random Forest
#Remove y_numeric
#Keep y_factor as target
#Remove irrelevant columns (IDs etc.)



############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))




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

#train_data$coffee_bar <- as.numeric(train_data$coffee_house) * as.numeric(train_data$bar)
#test_data$coffee_bar <- as.numeric(test_data$coffee_house) * as.numeric(test_data$bar)


##############
########## DISTRIBUTED DATA ##############
##############


coupon_stats

#Split dataset by coupon type

#Create sub-datasets.

coupon_list <- split(train_data, train_data$coupon)
head(coupon_list)
#Now you have:

train_data <- subset(train_data, coupon == "Coffee House")
test_data <- subset(test_data, coupon == "Coffee House")


train_data$coupon <- NULL
test_data$coupon  <- NULL


###################################################
##### Feature Importance ###########
###################################################

############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))

#-----------------------------
# Step 1: Identify feature importance
#-----------------------------

# Numeric features correlation with target
num_vars <- train_data %>% select(where(is.numeric), y_numeric)
num_cor <- cor(num_vars, use = "complete.obs")["y_numeric", ]
num_cor <- num_cor[!names(num_cor) %in% "y_numeric"]

# Categorical features: Cramer's V with target
cat_vars <- train_data %>% select(where(is.factor))
cramer_results <- sapply(cat_vars, function(x) {
  if(length(unique(x)) > 1) {
    assocstats(table(x, train_data$y_factor))$cramer
  } else {0} # avoid single-level factors
})

# Combine numeric and categorical importance
feature_scores <- data.frame(
  feature = c(names(num_cor), names(cramer_results)),
  score   = c(abs(num_cor), cramer_results)
)

# Rank features descending
feature_scores <- feature_scores %>% arrange(desc(score))
print("Top 10 Features by Importance:")
head(feature_scores,30)




###################################################
##### Feature Importance ###########
###################################################



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



roc_log <- roc(test_data$y_factor, prob_log)
auc(roc_log)

### Model 1 – Logistic Regression ###

### Model 2 – Decision Tree ###



model_tree <- rpart(y_factor ~ ., 
                    data = train_data,
                    method = "class")

rpart.plot(model_tree)

pred_tree <- predict(model_tree, test_data, type = "class")

confusionMatrix(pred_tree, test_data$y_factor)

### Model 2 – Decision Tree ###

### Model 3 – Random Forest ###



#varImpPlot(model_rf) # check for near zero importance variables if yes then delete them and retrain model


model_rf <- randomForest(y_factor ~ ., 
                         data = train_data,
                         ntree = 300)

pred_rf <- predict(model_rf, test_data)

confusionMatrix(pred_rf, test_data$y_factor)

# AUC value for Random Forest #

rf_probs <- predict(model_rf, test_data, type = "prob")[,2]

roc_rf <- roc(test_data$y_factor, rf_probs)

auc(roc_rf)
plot(roc_rf)
# AUC value for Random Forest #

### Model 3 – Random Forest ###



###########################
###########################
#### Coffee house #########
###########################
###########################



###########################
###########################
#### BAR #########
###########################
###########################

library(janitor)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(pROC)


### for installing packages also needed ("  ,dependencies = TRUE, method = "wininet" ")
df <- read.csv("./dataset/coupons.csv")


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
df <- df %>%
  select(-car)
# remove car column #

# Transform the expiration values in hours #
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


########################################
########################################
########################################
### Step 3: Predictive Modeling ########
########################################
########################################
########################################

#We will use 3 models:  Logistic Regression, Decision Tree, Random Forest
#Remove y_numeric
#Keep y_factor as target
#Remove irrelevant columns (IDs etc.)



############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))




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


##############
########## DISTRIBUTED DATA ##############
##############


coupon_stats

#Split dataset by coupon type

#Create sub-datasets.

coupon_list <- split(train_data, train_data$coupon)
head(coupon_list)
#Now you have:

train_data <- subset(train_data, coupon == "Bar")
test_data <- subset(test_data, coupon == "Bar")


train_data$coupon <- NULL
test_data$coupon  <- NULL


##############
########## DISTRIBUTED DATA ##############
##############


###################################################
##### Feature Importance ###########
###################################################

############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))

#-----------------------------
# Step 1: Identify feature importance
#-----------------------------

# Numeric features correlation with target
num_vars <- train_data %>% select(where(is.numeric), y_numeric)
num_cor <- cor(num_vars, use = "complete.obs")["y_numeric", ]
num_cor <- num_cor[!names(num_cor) %in% "y_numeric"]

# Categorical features: Cramer's V with target
cat_vars <- train_data %>% select(where(is.factor))
cramer_results <- sapply(cat_vars, function(x) {
  if(length(unique(x)) > 1) {
    assocstats(table(x, train_data$y_factor))$cramer
  } else {0} # avoid single-level factors
})

# Combine numeric and categorical importance
feature_scores <- data.frame(
  feature = c(names(num_cor), names(cramer_results)),
  score   = c(abs(num_cor), cramer_results)
)

# Rank features descending
feature_scores <- feature_scores %>% arrange(desc(score))
print("Top 10 Features by Importance:")
head(feature_scores,30)




###################################################
##### Feature Importance ###########
###################################################



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



roc_log <- roc(test_data$y_factor, prob_log)
auc(roc_log)

### Model 1 – Logistic Regression ###

### Model 2 – Decision Tree ###



model_tree <- rpart(y_factor ~ ., 
                    data = train_data,
                    method = "class")

rpart.plot(model_tree)

pred_tree <- predict(model_tree, test_data, type = "class")

confusionMatrix(pred_tree, test_data$y_factor)

### Model 2 – Decision Tree ###

### Model 3 – Random Forest ###



#varImpPlot(model_rf) # check for near zero importance variables if yes then delete them and retrain model


model_rf <- randomForest(y_factor ~ ., 
                         data = train_data,
                         ntree = 300)

pred_rf <- predict(model_rf, test_data)

confusionMatrix(pred_rf, test_data$y_factor)

# AUC value for Random Forest #

rf_probs <- predict(model_rf, test_data, type = "prob")[,2]

roc_rf <- roc(test_data$y_factor, rf_probs)

auc(roc_rf)
plot(roc_rf)
# AUC value for Random Forest #

### Model 3 – Random Forest ###+


###########################
###########################
#### BAR #########
###########################
###########################



###########################
###########################
#### Restaurant(<20) #########
###########################
###########################


library(janitor)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(pROC)


### for installing packages also needed ("  ,dependencies = TRUE, method = "wininet" ")
df <- read.csv("./dataset/coupons.csv")


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
df <- df %>%
  select(-car)
# remove car column #

# Transform the expiration values in hours #
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


########################################
########################################
########################################
### Step 3: Predictive Modeling ########
########################################
########################################
########################################

#We will use 3 models:  Logistic Regression, Decision Tree, Random Forest
#Remove y_numeric
#Keep y_factor as target
#Remove irrelevant columns (IDs etc.)



############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))




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


##############
########## DISTRIBUTED DATA ##############
##############


coupon_stats

#Split dataset by coupon type

#Create sub-datasets.

coupon_list <- split(train_data, train_data$coupon)
head(coupon_list)
#Now you have:


train_data <- subset(train_data, coupon == "Restaurant(<20)")
test_data <- subset(test_data, coupon == "Restaurant(<20)")


train_data$coupon <- NULL
test_data$coupon  <- NULL





##############
########## DISTRIBUTED DATA ##############
##############


###################################################
##### Feature Importance ###########
###################################################

############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))

#-----------------------------
# Step 1: Identify feature importance
#-----------------------------

# Numeric features correlation with target
num_vars <- train_data %>% select(where(is.numeric), y_numeric)
num_cor <- cor(num_vars, use = "complete.obs")["y_numeric", ]
num_cor <- num_cor[!names(num_cor) %in% "y_numeric"]

# Categorical features: Cramer's V with target
cat_vars <- train_data %>% select(where(is.factor))
cramer_results <- sapply(cat_vars, function(x) {
  if(length(unique(x)) > 1) {
    assocstats(table(x, train_data$y_factor))$cramer
  } else {0} # avoid single-level factors
})

# Combine numeric and categorical importance
feature_scores <- data.frame(
  feature = c(names(num_cor), names(cramer_results)),
  score   = c(abs(num_cor), cramer_results)
)

# Rank features descending
feature_scores <- feature_scores %>% arrange(desc(score))
print("Top 10 Features by Importance:")
head(feature_scores,30)




###################################################
##### Feature Importance ###########
###################################################



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



roc_log <- roc(test_data$y_factor, prob_log)
auc(roc_log)

### Model 1 – Logistic Regression ###

### Model 2 – Decision Tree ###



model_tree <- rpart(y_factor ~ ., 
                    data = train_data,
                    method = "class")

rpart.plot(model_tree)

pred_tree <- predict(model_tree, test_data, type = "class")

confusionMatrix(pred_tree, test_data$y_factor)

### Model 2 – Decision Tree ###

### Model 3 – Random Forest ###



#varImpPlot(model_rf) # check for near zero importance variables if yes then delete them and retrain model


model_rf <- randomForest(y_factor ~ ., 
                         data = train_data,
                         ntree = 300)

pred_rf <- predict(model_rf, test_data)

confusionMatrix(pred_rf, test_data$y_factor)

# AUC value for Random Forest #

rf_probs <- predict(model_rf, test_data, type = "prob")[,2]

roc_rf <- roc(test_data$y_factor, rf_probs)

auc(roc_rf)
plot(roc_rf)
# AUC value for Random Forest #

### Model 3 – Random Forest ###+




###########################
###########################
#### Restaurant(<20) #########
###########################
###########################



###########################
###########################
#### Restaurant(20-50) #########
###########################
###########################


library(janitor)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(pROC)


### for installing packages also needed ("  ,dependencies = TRUE, method = "wininet" ")
df <- read.csv("./dataset/coupons.csv")


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
df <- df %>%
  select(-car)
# remove car column #

# Transform the expiration values in hours #
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


########################################
########################################
########################################
### Step 3: Predictive Modeling ########
########################################
########################################
########################################

#We will use 3 models:  Logistic Regression, Decision Tree, Random Forest
#Remove y_numeric
#Keep y_factor as target
#Remove irrelevant columns (IDs etc.)



############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))




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


##############
########## DISTRIBUTED DATA ##############
##############


coupon_stats

#Split dataset by coupon type

#Create sub-datasets.

coupon_list <- split(train_data, train_data$coupon)
head(coupon_list)
#Now you have:


train_data <- subset(train_data, coupon == "Restaurant(20-50)")
test_data <- subset(test_data, coupon == "Restaurant(20-50)")


train_data$coupon <- NULL
test_data$coupon  <- NULL


##############
########## DISTRIBUTED DATA ##############
##############


###################################################
##### Feature Importance ###########
###################################################

############################## NEW WAY with incremental update of columns that will participate in the models ##########

### ===========================================
### Staged Modeling Pipeline for Coupon Data
### ===========================================

library(dplyr)
library(caret)
library(vcd)
library(randomForest)
library(rpart)
library(rpart.plot)

#-----------------------------
# Step 0: Prepare numeric target
#-----------------------------
train_data$y_numeric <- as.numeric(as.character(train_data$y_factor))
test_data$y_numeric  <- as.numeric(as.character(test_data$y_factor))

#-----------------------------
# Step 1: Identify feature importance
#-----------------------------

# Numeric features correlation with target
num_vars <- train_data %>% select(where(is.numeric), y_numeric)
num_cor <- cor(num_vars, use = "complete.obs")["y_numeric", ]
num_cor <- num_cor[!names(num_cor) %in% "y_numeric"]

# Categorical features: Cramer's V with target
cat_vars <- train_data %>% select(where(is.factor))
cramer_results <- sapply(cat_vars, function(x) {
  if(length(unique(x)) > 1) {
    assocstats(table(x, train_data$y_factor))$cramer
  } else {0} # avoid single-level factors
})

# Combine numeric and categorical importance
feature_scores <- data.frame(
  feature = c(names(num_cor), names(cramer_results)),
  score   = c(abs(num_cor), cramer_results)
)

# Rank features descending
feature_scores <- feature_scores %>% arrange(desc(score))
print("Top 10 Features by Importance:")
head(feature_scores,30)




###################################################
##### Feature Importance ###########
###################################################



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



roc_log <- roc(test_data$y_factor, prob_log)
auc(roc_log)

### Model 1 – Logistic Regression ###

### Model 2 – Decision Tree ###



model_tree <- rpart(y_factor ~ ., 
                    data = train_data,
                    method = "class")

rpart.plot(model_tree)

pred_tree <- predict(model_tree, test_data, type = "class")

confusionMatrix(pred_tree, test_data$y_factor)

### Model 2 – Decision Tree ###

### Model 3 – Random Forest ###



#varImpPlot(model_rf) # check for near zero importance variables if yes then delete them and retrain model


model_rf <- randomForest(y_factor ~ ., 
                         data = train_data,
                         ntree = 300)

pred_rf <- predict(model_rf, test_data)

confusionMatrix(pred_rf, test_data$y_factor)

# AUC value for Random Forest #

rf_probs <- predict(model_rf, test_data, type = "prob")[,2]

roc_rf <- roc(test_data$y_factor, rf_probs)

auc(roc_rf)
plot(roc_rf)
# AUC value for Random Forest #

### Model 3 – Random Forest ###+




###########################
###########################
#### Restaurant(20-50) #########
###########################
###########################


########################################
########### LIBRARIES ##################
########################################

library(janitor)
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)
library(pROC)
library(stringr)
library(lubridate)

########################################
########### LOAD DATA ##################
########################################

df <- read.csv("./dataset/coupons.csv")

########################################
########### DATA CLEANING ##############
########################################

df <- clean_names(df)

# Replace "nan"
df[df == "nan"] <- NA

# Convert characters to factors
df <- df %>%
  mutate(across(where(is.character), as.factor))

# Convert target
df$y <- as.factor(df$y)

# Remove duplicates
df <- distinct(df)

# Remove car column
df <- df %>% select(-car)

########################################
######## TIME TRANSFORMATION ###########
########################################

df <- df %>%
  mutate(time = hour(parse_date_time(time, orders = "I!p")))

########################################
######## EXPIRATION TRANSFORMATION #####
########################################

convert_to_hours <- function(x) {
  matches <- str_match_all(x, "(\\d+\\.?\\d*)\\s*([a-zA-Z]+)")[[1]]
  
  if(nrow(matches) == 0) return(NA_real_)
  
  total_hours <- 0
  
  for(i in 1:nrow(matches)){
    
    num <- as.numeric(matches[i,2])
    unit <- tolower(matches[i,3])
    
    total_hours <- total_hours + switch(unit,
                                        "m" = num/60,
                                        "h" = num,
                                        "d" = num*24,
                                        "w" = num*24*7,
                                        NA_real_
    )
  }
  
  return(total_hours)
}

df$expiration <- sapply(df$expiration, convert_to_hours)

########################################
######## HANDLE FREQUENCY VARIABLES ####
########################################

freq_cols <- c(
  "bar",
  "coffee_house",
  "carry_away",
  "restaurant_less_than20",
  "restaurant20to50"
)

df[freq_cols] <- lapply(df[freq_cols], function(x){
  
  x[x == ""] <- NA
  
  factor(
    x,
    levels = c("never","less1","1~3","4~8","gt8"),
    ordered = TRUE
  )
})

########################################
######## REMOVE MISSING ################
########################################

df <- na.omit(df)

########################################
######## TARGET VARIABLES ##############
########################################

df <- df %>%
  mutate(
    y_numeric = as.numeric(as.character(y)),
    y_factor = factor(y, levels=c(0,1))
  )

########################################
######## MODEL DATASET #################
########################################

df_clean <- df %>%
  select(-y, -y_numeric)

########################################
######## TRAIN TEST SPLIT ##############
########################################

set.seed(123)

train_index <- createDataPartition(df_clean$y_factor, p=0.7, list=FALSE)

train_data <- df_clean[train_index,]
test_data  <- df_clean[-train_index,]

########################################
######## REMOVE COLLINEAR FEATURE ######
########################################

train_data$direction_opp <- NULL
test_data$direction_opp  <- NULL

########################################
######## TIME CATEGORIES ###############
########################################

train_data$time <- cut(train_data$time,
                       breaks=c(-Inf,12,18,Inf),
                       labels=c("morning","afternoon","evening"))

test_data$time <- cut(test_data$time,
                      breaks=c(-Inf,12,18,Inf),
                      labels=c("morning","afternoon","evening"))

########################################
######## COUPON LIST ###################
########################################

coupon_types <- unique(train_data$coupon)

results <- data.frame()

########################################
######## MODEL LOOP ####################
########################################

for(coupon_name in coupon_types){
  
  cat("Running models for:", coupon_name,"\n")
  
  train_coupon <- subset(train_data, coupon == coupon_name)
  test_coupon  <- subset(test_data,  coupon == coupon_name)
  
  train_coupon$coupon <- NULL
  test_coupon$coupon  <- NULL
  
  ################################
  #### LOGISTIC REGRESSION #######
  ################################
  
  model_log <- glm(
    y_factor ~ .,
    data = train_coupon,
    family = binomial
  )
  
  prob_log <- predict(model_log, test_coupon, type="response")
  
  pred_log <- ifelse(prob_log > 0.5,1,0)
  pred_log <- factor(pred_log, levels=c(0,1))
  
  acc_log <- confusionMatrix(pred_log, test_coupon$y_factor)$overall["Accuracy"]
  
  roc_log <- roc(test_coupon$y_factor, prob_log)
  auc_log <- auc(roc_log)
  
  ################################
  #### DECISION TREE #############
  ################################
  
  model_tree <- rpart(
    y_factor ~ .,
    data = train_coupon,
    method="class"
  )
  
  pred_tree <- predict(model_tree, test_coupon, type="class")
  
  acc_tree <- confusionMatrix(pred_tree, test_coupon$y_factor)$overall["Accuracy"]
  
  ################################
  #### RANDOM FOREST #############
  ################################
  
  model_rf <- randomForest(
    y_factor ~ .,
    data=train_coupon,
    ntree=300
  )
  
  pred_rf <- predict(model_rf, test_coupon)
  
  acc_rf <- confusionMatrix(pred_rf, test_coupon$y_factor)$overall["Accuracy"]
  
  rf_probs <- predict(model_rf, test_coupon, type="prob")[,2]
  
  roc_rf <- roc(test_coupon$y_factor, rf_probs)
  auc_rf <- auc(roc_rf)
  
  ################################
  #### SAVE RESULTS ##############
  ################################
  
  results <- rbind(
    results,
    data.frame(
      Coupon = coupon_name,
      Logistic_Accuracy = acc_log,
      Logistic_AUC = auc_log,
      Tree_Accuracy = acc_tree,
      RF_Accuracy = acc_rf,
      RF_AUC = auc_rf
    )
  )
}

########################################
######## FINAL RESULTS #################
########################################

print(results)
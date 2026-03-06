library(tidyverse)
library(janitor)
### for installing packages also needed ("  ,dependencies = TRUE, method = "wininet" ")
df <- read.csv("./dataset/coupons.csv")

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
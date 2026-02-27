#################

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

### Step 2: Advanced EDA ###
#install.packages("GGally",dependencies = TRUE, method = "wininet")
library(ggplot2)
library(dplyr)
library(GGally) 

### Step 2: Advanced EDA ###

# 2.1 Acceptance Trend Over Time (line plot)
df %>%
  group_by(time) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = time, y = accept_rate, group = 1)) +
  geom_line(color = "#2c7fb8", size = 1.2) +
  geom_point(color = "#2c7fb8", size = 3) +
  labs(title = "Coupon Acceptance Rate by Time of Day",
       x = "Time of Day", y = "Acceptance Rate") +
  theme_minimal()

# 2.2 Acceptance vs Income (scatter + smooth)
ggplot(df, aes(x = income, y = y_numeric)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "#377eb8") +
  geom_smooth(method = "loess", color = "#e41a1c") +
  labs(title = "Coupon Acceptance vs Income Level",
       x = "Income", y = "Acceptance Rate") +
  theme_minimal()

# 2.3 Heatmap: Coupon Type vs Time of Day
df %>%
  group_by(coupon, time) %>%
  summarise(accept_rate = mean(y_numeric, na.rm = TRUE)) %>%
  ggplot(aes(x = coupon, y = time, fill = accept_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Acceptance Heatmap: Coupon Type vs Time of Day",
       x = "Coupon Type", y = "Time of Day", fill = "Acceptance Rate") +
  theme_minimal()

# 2.4 Violin + Boxplot: Distance vs Acceptance
ggplot(df, aes(x = as.factor(to_coupon_geq15min), y = y_numeric)) +
  geom_violin(fill = "#377eb8", alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Coupon Acceptance Distribution by Distance",
       x = "Distance ≥ 15 min", y = "Acceptance Rate") +
  theme_minimal()

# 2.5 Facetted Bar Plot: Coffee Visits vs Acceptance by Coupon Type
ggplot(df, aes(x = coffee_house, fill = y_factor)) +
  geom_bar(position = "fill") +
  facet_wrap(~coupon) +
  labs(title = "Acceptance by Coffee Visits Across Coupon Types",
       x = "Monthly Coffee Visits", y = "Proportion", fill = "Accepted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2.6 Pair Plot: Explore Relationships Between Variables
df_subset <- df %>% select(y_numeric, income, to_coupon_geq15min, coffee_house, y_factor)

ggpairs(df_subset, mapping = aes(color = y_factor)) +
  labs(title = "Pairwise Relationships Between Features and Acceptance")

# 2.7 Optional: Acceptance Counts by Coupon Type (dodge bar)
ggplot(df, aes(x = coupon, fill = y_factor)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Accepted vs Rejected Coupons by Type",
       x = "Coupon Type", y = "Count", fill = "Accepted") +
  theme_minimal()
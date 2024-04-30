## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)
library(readxl)

# Load data 
data <- read_delim("raw_data.csv")



mean(data$likes)
mean(data$follower_count)
mean(data$uplifting)
mean(data$envious)
mean(data$funny)
mean(data$motivated)
mean(data$desensitized)
table(data$uplifting)
table(data$envious)
table(data$funny)
table(data$motivated)
table(data$desensitized)

table(data$follower_count)
sd(data$likes)
sd(data$follower_count)
min(data$likes)
min(data$follower_count)
max(data$likes)
max(data$follower_count)
hist(data$follower_count)
data$ln_follower_count <- log(data$follower_count)
hist(data$ln_follower_count)


table(data$likes,data$follower_count)
chisq.test(data$uplifting,data$desensitized)

ggplot(raw_data, aes(x = as.factor(data$uplifting), y = data$ln_follower_count)) +
  geom_boxplot() +
  labs(title = "Box Plot of Likes by Uplifting",
       x = "Uplifting",
       y = "Likes") +
  theme_minimal()
       
       
       
linear_plot <- plot(data$likes, data$follower_count)
       print(linear_plot)
 # add x line and y line for means
meany <- mean(data$likes)
meanx <- mean(data$follower_count)
       
       abline(h = meanx, col = "black")
       abline(v = meany, col = "black")
       
       
       
       ##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
       linear_relationship <- lm(data$likes ~ data$follower_count, data = data)
       summary(linear_relationship)
       
       # Add the linear regression line to the scatter plot
       # NOTE: double check the scatter plot is currently in your utilities window!
       abline(linear_relationship, col = "red")
       
       ##### STEP 3: Plot the residuals
       
       # Plot the residuals
       plot(data$likes, residuals(linear_relationship))
       
       # Add a horizontal line at zero to indicate the baseline
       abline(h = 0, col = "red") 

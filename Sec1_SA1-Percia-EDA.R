# Load necessary packages
library(tidyverse)
library(corrplot)
library(ggplot2)
library(caret)
library(GGally)
library(knitr)
library(broom)

# Read in the dataset
data <- read.csv("your_dataset.csv")

# Display the first few rows
head(data)

# Check structure and summary
str(data)
summary(data)

# Handle missing values (example: removing them)
data <- na.omit(data)

# Convert factors if necessary
data$Category <- as.factor(data$Category)  # Example column

# Descriptive statistics
summary(data)

# Histogram for a numeric variable
ggplot(data, aes(x = Price)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Price", x = "Price", y = "Count")

# Pairwise plot for numeric features
num_data <- select(data, where(is.numeric))
ggpairs(num_data)

# Compute correlation matrix
cor_matrix <- cor(num_data)

# Visualize correlation
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Example linear regression: Price as outcome
model <- lm(Price ~ Feature1 + Feature2 + Feature3, data = data)

# Model summary
summary(model)

# Residual plot
par(mfrow = c(2, 2))
plot(model)

# Tidy model results
tidy(model)

# Model performance summary
glance(model)

# New sample data for prediction
new_data <- data.frame(Feature1 = c(5), Feature2 = c(3.5), Feature3 = c(2))

# Predict
predict(model, newdata = new_data)

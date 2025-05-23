# Create the data frame
mortality_data <- read.csv("C:/Users/john/Downloads/mortality_by_latitude.csv")

#### NUM 1

# Create the plot
library(ggplot2)

ggplot(mortality_data, aes(x = temperature, y = mortality_index)) +
  geom_point(size = 3, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  labs(title = "Mortality Index vs. Mean Average Temperature",
       x = "Mean Average Temperature (F)",
       y = "Mortality Index") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### hollow-up

## The relationship is hollow-up (concave upward) as mortality increases with temperature. This suggests an exponential relationship rather than logarithmic

## Transform Temeperature to Linearize the Relationship

# Square temperature
mortality_data$temp_squared <- mortality_data$temperature^2

# Plot transformed relationship
ggplot(mortality_data, aes(x = temp_squared, y = mortality_index)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "orange") +
  labs(
    title = "Mortality Index vs. Squared Temperature",
    x = "Temperature Squared (°F²)",
    y = "Mortality Index"
  ) +
  theme_minimal()

# Apply exponential scaling (e.g., exp(temperature/10) to avoid extreme values)
mortality_data$temp_exp <- exp(mortality_data$temperature / 10)

ggplot(mortality_data, aes(x = temp_exp, y = mortality_index)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "orange") +
  labs(
    title = "Mortality Index vs. Exponential Temperature",
    x = "exp(Temperature / 10)",
    y = "Mortality Index"
  ) +
  theme_minimal()

# Fit model (using squared temperature)
model <- lm(mortality_index ~ temp_squared, data = mortality_data)

# Residual plot
ggplot(mortality_data, aes(x = fitted(model), y = resid(model))) +
  geom_point(size = 3, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Residuals vs. Fitted Values (Squared Temp)",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()


#### NUM 2
data(diamonds)

## Load Data & Apply Log Transformation

# Subset for faster computation (e.g., 10% of data)
set.seed(123)
diamonds_sub <- diamonds[sample(nrow(diamonds), size = 0.1 * nrow(diamonds)), ]

# Add log(price)
diamonds_sub$log_price <- log(diamonds_sub$price)

##  Plot with LOESS Smoother
# Default parameters
ggplot(diamonds_sub, aes(x = carat, y = log_price)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", span = 0.75, degree = 2, 
              color = "red", se = FALSE, linewidth = 1) +
  labs(title = "Log(Price) vs. Carat (Default LOESS: span=0.75, degree=2)",
       x = "Carat", y = "Log(Price)") +
  theme_minimal()

#Tuning span
# Example: Larger span (smoother)
ggplot(diamonds_sub, aes(x = carat, y = log_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 1.0, color = "darkgreen", se = FALSE) +
  labs(title = "Span = 1.0 (Smoother)")

#Tuning degree
# Example: Degree = 1 (linear local fits)
ggplot(diamonds_sub, aes(x = carat, y = log_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.75, degree = 1, 
              color = "purple", se = FALSE) +
  labs(title = "Degree = 1 (Linear Local Fits)")

# Final recommended plot
ggplot(diamonds_sub, aes(x = carat, y = log_price)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "loess", span = 0.5, degree = 2,
              color = "red", se = FALSE, linewidth = 1) +
  labs(title = "Recommended: Log(Price) vs. Carat (span=0.5, degree=2)",
       x = "Carat", y = "Log(Price)") +
  theme_minimal()


#### Number 3
#Comparing them starting from scratch

library(ggplot2)
library(patchwork)  # For combining plots
data(diamonds)

# Subset for speed (10% of data)
set.seed(123)
diamonds_sub <- diamonds[sample(nrow(diamonds), 1000), ]
diamonds_sub$log_price <- log(diamonds_sub$price)  # Log-transform price

# Model 1: LOESS smoother (span=0.5, degree=2)
loess_fit <- loess(log_price ~ carat, data = diamonds_sub, span = 0.5, degree = 2)
diamonds_sub$loess_resid <- residuals(loess_fit)  # Add LOESS residuals

# Model 2: Polynomial (cubic) + step function
diamonds_sub$carat_bin <- cut(diamonds_sub$carat, breaks = c(0, 1, 2, 3, 5))  # Binning
poly_step_fit <- lm(log_price ~ poly(carat, 3) + carat_bin, data = diamonds_sub)
diamonds_sub$poly_step_resid <- residuals(poly_step_fit)  # Add polynomial+step residuals

# LOESS residuals plot
p1 <- ggplot(diamonds_sub, aes(x = carat, y = loess_resid)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "LOESS Residuals", x = "Carat", y = "Residuals") +
  theme_minimal()

# Polynomial + Step residuals plot
p2 <- ggplot(diamonds_sub, aes(x = carat, y = poly_step_resid)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Polynomial + Step Residuals", x = "Carat", y = "Residuals") +
  theme_minimal()

# Combine plots
p1 + p2 + plot_layout(ncol = 2)
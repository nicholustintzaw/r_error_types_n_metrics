################################################################################
# Load necessary library
################################################################################
library(ggplot2)  # For plotting
library(dplyr)  
library(tidyr) # For pivoting the data
library(gridExtra) # For combining plots
library(grid)  # Load the grid package for textGrob

################################################################################
# Use built-in mtcars dataset
data(mtcars)

# Fit a linear regression model (predicting mpg from wt)
model <- lm(mpg ~ wt, data = mtcars)

# Get predicted values and residuals
predicted_values <- predict(model)
residuals <- mtcars$mpg - predicted_values

# Calculate Error Metrics and Components
SST <- sum((mtcars$mpg - mean(mtcars$mpg))^2)
SSR <- sum((predicted_values - mean(mtcars$mpg))^2)
SSE <- sum(residuals^2)
MSE <- mean(residuals^2)
R_squared <- SSR / SST

# Print the results
cat("Sum of Squares Total (SST):", SST, "\n")
cat("Sum of Squares Regression (SSR):", SSR, "\n")
cat("Sum of Squares Error (SSE):", SSE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("R-squared:", R_squared, "\n")

################################################################################
# Visualization 1: SST, SSR, and SSE
################################################################################
# Create a dataframe for the segments
mtcars_segments <- mtcars %>%
  mutate(
    fitted = predict(model),
    mean_mpg = mean(mpg),
    segment_type = case_when(
      mpg > mean_mpg & mpg > fitted ~ "SST",
      mpg > mean_mpg & mpg <= fitted ~ "SSR",
      mpg <= mean_mpg & mpg <= fitted ~ "SSE",
      TRUE ~ "Other"
    )
  )

# Plot with ggplot2
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line
  geom_segment(data = mtcars_segments, aes(x = wt, xend = wt, y = mpg, yend = mean_mpg, color = "SST")) +  # SST lines
  geom_segment(data = mtcars_segments, aes(x = wt, xend = wt, y = mean_mpg, yend = fitted, color = "SSR"), linetype = "dotted") +  # SSR lines
  geom_segment(data = mtcars_segments, aes(x = wt, xend = wt, y = fitted, yend = mpg, color = "SSE"),linetype = "dashed") +  # SSE lines
  scale_color_manual(values = c("SST" = "red", "SSR" = "orange", "SSE" = "blue")) +
  labs(
    title = "SST, SSR, and SSE in Linear Regression",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon (MPG)",
    color = "Error Type"
  ) +
  theme(legend.position = "right")

################################################################################
### 3 Seperate plot for better illustration ###
################################################################################
# Define the limits for the x-axis
x_limits <- c(1.5, 5.5)

# Plot for SST
plot_sst <- ggplot(mtcars_segments, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +  # Regression line
  geom_hline(yintercept = mean(mtcars$mpg), linetype = "longdash", color = "purple", size = 0.5) + 
  geom_segment(aes(x = wt, xend = wt, y = mpg, yend = mean_mpg), color = "red") +  # SST lines
  labs(title = "SST") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  xlim(x_limits)

# Plot for SSR
plot_ssr <- ggplot(mtcars_segments, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +  # Regression line
  geom_segment(aes(x = wt, xend = wt, y = mean_mpg, yend = predict(model)), color = "blue", linetype = "dashed") +  # SSR lines
  labs(title = "SSR") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  xlim(x_limits)

# Plot for SSE
plot_sse <- ggplot(mtcars_segments, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +  # Regression line
  geom_segment(aes(x = wt, xend = wt, y = predict(model), yend = mpg), color = "darkgreen", linetype = "dashed") +  # SSE lines
  labs(title = "SSE") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  xlim(x_limits)

# Combine the three plots
combined_plot <- grid.arrange(plot_sse, plot_ssr, plot_sst, ncol = 1)

grid.arrange(combined_plot,
             bottom = textGrob("Weight (1000 lbs)", gp = gpar(fontsize = 15)),
             left = textGrob("Miles per Gallon (MPG)", rot = 90, gp = gpar(fontsize = 15)))

################################################################################
# Visualization 2: MSE and R-squared
################################################################################
ggplot(mtcars_segments, aes(x = fitted, y = residuals(model))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Zero line
  labs(title = "Residual Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Calculate Mean Absolute Deviation (MAD)
mad <- mean(abs(residuals(model)))

# Create the residual plot
ggplot(mtcars_segments, aes(x = fitted, y = residuals(model))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Zero line
  geom_hline(yintercept = mad, linetype = "dotted", color = "blue") +  # MAD line
  labs(title = "Residual Plot with MAD Line",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Calculate squared errors (SSE)
squared_errors <- residuals(model)^2

# Calculate MSE
mse <- mean(squared_errors)

# Create the plot
ggplot(data.frame(squared_errors), aes(x = squared_errors)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +  # Histogram of squared errors
  geom_vline(xintercept = mse, linetype = "dashed", color = "red", size = 1) +  # Vertical line for MSE
  labs(title = "Distribution of Squared Errors (SSE) and MSE",
       x = "Squared Error",
       y = "Frequency") +
  theme_minimal()

# Load necessary packages
library(ggplot2)
library(mice)  # For imputation

# Load the dataset
coffee_data <- read.csv("coffee_data_complete.csv")


# --- Option A: Remove rows with missing data ---

# Remove rows with missing values in 'cups_per_day'
complete_cases <- coffee_data[complete.cases(coffee_data$cups_per_day), ]

# Summary statistics for 'cups_per_day'
summary(complete_cases$cups_per_day)

# Histogram of 'cups_per_day' using ggplot2 for better visualization
plot_a <- ggplot(complete_cases, aes(x = cups_per_day)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Cups per Day (Complete Cases)",
       x = "Cups per Day", y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("challenge1_plot_a.png", plot_a)

# --- Option B: Impute missing values with the mean ---

# Calculate the mean of 'cups_per_day'
mean_cups <- mean(coffee_data$cups_per_day, na.rm = TRUE)

# Impute missing values with the mean
mean_imputed_data <- coffee_data  # Create a copy of the data
mean_imputed_data$cups_per_day[is.na(mean_imputed_data$cups_per_day)] <- mean_cups

# Summary statistics for 'cups_per_day'
summary(mean_imputed_data$cups_per_day)

# Histogram of 'cups_per_day' using ggplot2
plot_b <- ggplot(mean_imputed_data, aes(x = cups_per_day)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Cups per Day (Mean Imputation)",
       x = "Cups per Day", y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("challenge1_plot_b.png", plot_b)

# --- Option C: Impute missing values using a prediction model ---

# Impute missing values using predictive mean matching
imputed_data <- mice(coffee_data, m = 5, method = "pmm", seed = 42)

# Complete the dataset using the first imputed set
completed_data <- complete(imputed_data, 1)

# Summary statistics for 'cups_per_day'
summary(completed_data$cups_per_day)

# Histogram of 'cups_per_day' using ggplot2
plot_c <- ggplot(completed_data, aes(x = cups_per_day)) +
  geom_histogram(binwidth = 0.5, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Cups per Day (Predictive Mean Matching)",
       x = "Cups per Day", y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("challenge1_plot_c.png", plot_c)

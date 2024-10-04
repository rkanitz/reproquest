# Load necessary libraries
library(ggplot2)

# Load the dataset
coffee_data <- read.csv("coffee_data_complete.csv")

# --- Option A: Scatter plot ---

# Scatter plot using ggplot2
plot_a <- ggplot(coffee_data, aes(x = age, y = cups_per_day)) +
  geom_point(color = "skyblue", alpha = 0.7) +
  labs(title = "Scatter Plot of Cups per Day vs. Age",
       x = "Age", y = "Cups per Day") +
  theme_minimal()

# Save the plot
ggsave("challenge2_plot_a.png", plot_a)

# --- Option B: Box plot ---

# Box plot using ggplot2
plot_b <- ggplot(coffee_data, aes(x = as.factor(age), y = cups_per_day)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Box Plot of Cups per Day by Age",
       x = "Age", y = "Cups per Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot
ggsave("challenge2_plot_b.png", plot_b)

# --- Option C: Line graph with smoothed trend ---

# Line graph with smoothed trend using ggplot2
plot_c <- ggplot(coffee_data, aes(x = age, y = cups_per_day)) +
  geom_smooth(method = "loess", color = "lightcoral") +  # Use loess smoothing
  labs(title = "Line Graph of Cups per Day by Age (Smoothed Trend)",
       x = "Age", y = "Cups per Day") +
  theme_minimal()

# Save the plot
ggsave("challenge2_plot_c.png", plot_c)
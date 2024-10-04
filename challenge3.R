# Load necessary libraries
library(ggplot2)
library(dplyr)  # For data manipulation

# Load the dataset
coffee_data <- read.csv("coffee_data_complete.csv")

# --- Option A: Use the self-reported 'expertise_level' ---

# Bar chart of expertise levels
plot_a <- ggplot(coffee_data, aes(x = expertise_level)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Self-Reported Coffee Expertise",
       x = "Expertise Level", y = "Count") +
  theme_minimal()

# Save the plot
ggsave("challenge3_plot_a.png", plot_a)

# --- Option B: Create a 'connoisseur score' ---

# Assign scores to different levels of each factor
brewing_method_scores <- c("Drip" = 1, "French press" = 3, "Espresso machine" = 4, "Pour over" = 5, "Pod machine" = 1, "Instant" = 1, "Other" = 2)
sugar_preference_scores <- c("None" = 4, "Little" = 3, "Moderate" = 2, "A lot" = 1)
main_reason_scores <- c("Taste" = 4, "Energy boost" = 1, "Social habit" = 2, "Routine/habit" = 3)
roast_preference_scores <- c("Light" = 3, "Medium" = 2, "Dark" = 1)  # Assuming connoisseurs prefer lighter roasts

# Calculate the connoisseur score
coffee_data <- coffee_data %>%
  mutate(
    brewing_score = brewing_method_scores[brewing_method],
    sugar_score = sugar_preference_scores[sugar_preference],
    reason_score = main_reason_scores[main_reason],
    roast_score = roast_preference_scores[roast_preference],
    connoisseur_score = brewing_score + sugar_score + reason_score + roast_score
  )

# Histogram of the connoisseur score
plot_b <- ggplot(coffee_data, aes(x = connoisseur_score)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Connoisseur Score",
       x = "Connoisseur Score", y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("challenge3_plot_b.png", plot_b)

# --- Option C: Focus on those who spend the most on coffee ---

# Define a threshold for high spenders (e.g., top 25%)
high_spender_threshold <- quantile(coffee_data$spend_per_week, 0.75, na.rm = TRUE)

# Identify high spenders
coffee_data <- coffee_data %>%
  mutate(high_spender = ifelse(spend_per_week >= high_spender_threshold, "Yes", "No"))

# Box plot of spending by high spender status
plot_c <- ggplot(data=subset(coffee_data, !is.na(spend_per_week)), aes(x = high_spender, y = spend_per_week)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Spending Habits of High Spenders vs. Others",
       x = "High Spender", y = "Spend per Week") +
  theme_minimal()

# Save the plot
ggsave("challenge3_plot_c.png", plot_c)

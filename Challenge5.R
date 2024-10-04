# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
coffee_data <- read.csv("coffee_data_complete.csv")

# Omit NAs 
coffee_data <- na.omit(coffee_data)

# --- Option A: Compare proportions of brewing methods in each region ---

# Calculate proportions
brewing_proportions <- coffee_data %>%
  group_by(location, brewing_method) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

# Grouped bar chart
plot_a <- ggplot(brewing_proportions, aes(x = location, y = proportion, fill = brewing_method)) +
  geom_col(position = "dodge") +
  labs(title = "Proportions of Brewing Methods by Location",
       x = "Location", y = "Proportion", fill = "Brewing Method") +
  theme_minimal()

ggsave("challenge5_plot_a.png", plot_a)

# --- Option B: Conduct a statistical test ---

# Chi-squared test for independence
chisq_result <- chisq.test(table(coffee_data$location, coffee_data$brewing_method))

# Print the results
print(chisq_result)

# Create a table of expected vs. observed frequencies
observed_freqs <- table(coffee_data$location, coffee_data$brewing_method)
expected_freqs <- chisq_result$expected

# Create a data frame for plotting
chisq_df <- data.frame(
  Location = rep(rownames(observed_freqs), times = ncol(observed_freqs)),
  Brewing_Method = rep(colnames(observed_freqs), each = nrow(observed_freqs)),
  Observed = as.vector(observed_freqs),
  Expected = as.vector(expected_freqs)
)

# Deviation bar chart
plot_b <- ggplot(chisq_df, aes(x = Brewing_Method, y = Observed - Expected, fill = Location)) +
  geom_col(position = "dodge") +
  labs(title = "Deviation from Expected Frequencies",
       x = "Brewing Method", y = "Observed - Expected", fill = "Location") +
  theme_minimal()

ggsave("challenge5_plot_b.png", plot_b)

# --- Option C: Visualize on a map ---

# (This option requires a map of the regions in your dataset)
# For demonstration purposes, let's assume a hypothetical map
# Replace with your actual map data and visualization code

# Hypothetical data for map visualization
map_data <- data.frame(
  region = c("Urban", "Suburban", "Rural"),
  longitude = c(-100, -98, -102),  # Replace with actual longitude values
  latitude = c(40, 38, 36),      # Replace with actual latitude values
  most_common_method = c("Drip", "French press", "Drip")
)

# Hypothetical map visualization (replace with your actual code)
plot_c <- ggplot(map_data, aes(x = longitude, y = latitude, color = most_common_method)) +
  geom_point(size = 5) +
  labs(title = "Most Common Brewing Method by Region (Hypothetical)",
       x = "Longitude", y = "Latitude", color = "Brewing Method") +
  theme_minimal()

ggsave("challenge5_plot_c.png", plot_c)

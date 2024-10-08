# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
coffee_data <- read.csv("coffee_data_complete.csv")

# Omit NAs 
coffee_data <- na.omit(coffee_data)

# --- Option A: Write a concise summary ---

# Calculate the proportion of strong roast preference among those who drink coffee for energy
energy_boosters <- coffee_data %>%
  filter(main_reason == "Energy boost")

strong_roast_proportion <- mean(energy_boosters$roast_preference == "Dark")

# Create a text summary
summary_text <- paste0(
  "Among those who drink coffee primarily for an energy boost, ",
  round(strong_roast_proportion * 100, 1),
  "% prefer strong roasts."
)

# Create a simple plot for visual interest (optional)
plot_a <- ggplot() +
  annotate("text", x = 0, y = 0, label = summary_text, size = 5) +
  theme_void()

ggsave("challenge6_plot_a.png", plot_a)

# --- Option B: Create an infographic ---

# (This option requires more advanced visualization techniques)
# For demonstration purposes, let's create a simple bar chart

# Calculate proportions for all roast preferences
roast_proportions <- energy_boosters %>%
  group_by(roast_preference) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

# Bar chart
plot_b <- ggplot(roast_proportions, aes(x = roast_preference, y = proportion, fill = roast_preference)) +
  geom_col() +
  labs(title = "Roast Preference Among Energy Boosters",
       x = "Roast Preference", y = "Proportion") +
  theme_minimal()

ggsave("challenge6_plot_b.png", plot_b)

# --- Option C: Craft a blog post ---

# (This option involves writing a blog post with a compelling narrative)
# Here's an example of a short blog post excerpt:

blog_post_excerpt <- "
## The Buzz on Roast Preference: Darker for the Driven?

Ever wondered if your coffee choice reflects your personality? 
Our recent deep dive into coffee consumption habits revealed an interesting trend: 
those who reach for coffee as an energy boost tend to favor darker roasts. 
Could it be that the bold, intense flavors of a dark roast better complement the 
drive and determination of these caffeine-fueled go-getters? Or perhaps 
it's simply a matter of maximizing the caffeine kick! Whatever the reason, 
it seems there's a fascinating link between personality and palate when it comes to coffee.
"

# Create a simple plot for visual interest (optional)
plot_c <- ggplot() +
  annotate("text", x = 0, y = 0, label = blog_post_excerpt, size = 3) +
  theme_void()

ggsave("challenge6_plot_c.png", plot_c)

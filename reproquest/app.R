library(shiny)
library(shinyMobile)

# Define UI for the application
ui <- f7Page(
  title = "Reproducibility Quest",
  f7SingleLayout(
    navbar = f7Navbar(title = "The Coffee Conundrum"),
    f7Card(
      h2("Welcome to Reproducibility Quest!"),
      p("Explore the fascinating world of coffee consumption habits and uncover hidden patterns in this interactive data adventure."),
      p("Each step presents a challenge with different analytical approaches. Choose your path and discover the unique insights that unfold."),
      br(),
      actionButton("start_quest", "Start the Quest!")
    ),
    uiOutput("challenge_ui")
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value to store the current challenge
  current_challenge <- reactiveVal(1)
  
  # Observe the "Start the Quest!" button
  observeEvent(input$start_quest, {
    current_challenge(2) # Move to the first challenge
  })
  
  # Render UI for the current challenge
  output$challenge_ui <- renderUI({
    if (current_challenge() == 2) {
      f7Card(
        h3("Challenge 1: Dealing with Missing Data"),
        p("The dataset has some missing values for 'cups per day.' How do you want to handle them?"),
        f7Radio(
          inputId = "challenge_1_choice",
          label = "Choose your approach:",  # Added 'label' argument
          choices = c(
            "A: Remove rows with missing data.",
            "B: Impute missing values with the mean.",
            "C: Impute missing values using a prediction model."
          )
        ),
        br(),
        actionButton("challenge_1_submit", "Submit")
      )
    } else if (current_challenge() == 3) {
      f7Card(
        h3("Challenge 2: Exploring Consumption Patterns"),
        p("You want to visualize how coffee consumption varies across different age groups. What type of plot is most suitable?"),
        f7Radio(
          inputId = "challenge_2_choice",
          label = "Choose your approach:",  # Added 'label' argument
          choices = c(
            "A: Scatter plot.",
            "B: Box plot.",
            "C: Line graph with smoothed trend."
          )
        ),
        br(),
        actionButton("challenge_2_submit", "Submit")
      )
    } else if (current_challenge() == 4) {
      f7Card(
        h3("Challenge 3: Identifying Coffee Connoisseurs"),
        p("You want to identify a group of coffee drinkers who are particularly knowledgeable and passionate about coffee. How do you approach this?"),
        f7Radio(
          inputId = "challenge_3_choice",
          label = "Choose your approach:",  # Added 'label' argument
          choices = c(
            "A: Use the self-reported 'coffee expertise' level.",
            "B: Create a 'connoisseur score' based on multiple factors.",
            "C: Focus on those who spend the most on coffee."
          )
        ),
        br(),
        actionButton("challenge_3_submit", "Submit")
      )
    } else if (current_challenge() == 5) {
      f7Card(
        h3("Challenge 4: Segmenting Coffee Drinkers"),
        p("You want to segment coffee drinkers into distinct groups based on their habits and preferences. Which clustering method do you choose?"),
        f7Radio(
          inputId = "challenge_4_choice",
          label = "Choose your approach:",  # Added 'label' argument
          choices = c(
            "A: K-means clustering.",
            "B: Hierarchical clustering."
          )
        ),
        br(),
        actionButton("challenge_4_submit", "Submit")
      )
    } else if (current_challenge() == 6) {
      f7Card(
        h3("Challenge 5: Analyzing Brewing Method Preferences"),
        p("You want to understand how brewing method preferences vary across different regions. What analysis do you perform?"),
        f7Radio(
          inputId = "challenge_5_choice",
          label = "Choose your approach:",  # Added 'label' argument
          choices = c(
            "A: Compare the proportions of each brewing method in each region.",
            "B: Conduct a statistical test to see if the differences are significant.",
            "C: Visualize the data on a map."
          )
        ),
        br(),
        actionButton("challenge_5_submit", "Submit")
      )
    } else if (current_challenge() == 7) {
      f7Card(
        h3("Challenge 6: Communicating Your Findings"),
        p("You've discovered that people who drink coffee primarily for the 'energy boost' tend to prefer stronger roasts. How do you present this finding?"),
        f7Radio(
          inputId = "challenge_6_choice",
          label = "Choose your approach:",  # Added 'label' argument
          choices = c(
            "A: Write a concise summary of the key insight.",
            "B: Create an infographic with visuals and statistics.",
            "C: Craft a blog post with a relatable story and personal anecdotes."
          )
        ),
        br(),
        actionButton("challenge_6_submit", "Submit")
      )
    } else {
      # Display the outcome summary based on the choices made
      f7Card(
        h3("Outcome Summary"),
        # ... (Display the outcome summary here)
      )
    }
  })
  
  # Observe the "Submit" button for each challenge
  observeEvent(input$challenge_1_submit, {
    current_challenge(3) # Move to the next challenge
  })
  
  observeEvent(input$challenge_2_submit, {
    current_challenge(4)
  })
  
  observeEvent(input$challenge_3_submit, {
    current_challenge(5)
  })
  
  observeEvent(input$challenge_4_submit, {
    current_challenge(6)
  })
  
  observeEvent(input$challenge_5_submit, {
    current_challenge(7)
  })
  
  observeEvent(input$challenge_6_submit, {
    current_challenge(8) # Move to the outcome summary
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
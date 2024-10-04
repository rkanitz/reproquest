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
  
  # Reactive values to store user choices
  user_choices <- reactiveValues(
    challenge1 = NULL,
    challenge2 = NULL,
    challenge3 = NULL,
    challenge4 = NULL,
    challenge5 = NULL,
    challenge6 = NULL
  )
  
  # Observe the "Start the Quest!" button
  observeEvent(input$start_quest, {
    current_challenge(2) # Move to the first challenge
  })
  
  
  # Render UI for the current challenge
  output$challenge_ui <- renderUI({
    if (current_challenge() == 2) {
      # Challenge 1 UI
      f7Card(
        h3("Challenge 1: Dealing with Missing Data"),
        p("The dataset has some missing values for 'cups per day.' How do you want to handle them?"),
        f7Radio(
          inputId = "challenge_1_choice",
          label = "Choose your approach:",
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
      # Challenge 2 UI 
      # ... (rest of your challenge UIs) ...
    } else if (current_challenge() == 7) {
      # Challenge 6 UI
      # ... 
    } else if (current_challenge() == 8) {  
      # Outcome Summary UI
      f7Card(
        h3("Outcome Summary"),
        
        # Challenge 1 outcome
        if (user_choices$challenge1 == "A") {
          img(src = "challenge1_plot_a.png", width = "100%")
        } else if (user_choices$challenge1 == "B") {
          img(src = "challenge1_plot_b.png", width = "100%")
        } else if (user_choices$challenge1 == "C") {
          img(src = "challenge1_plot_c.png", width = "100%")
        }, # Close the if statement for Challenge 1 outcome
        
        # Challenge 2 outcome
        if (user_choices$challenge2 == "A") {
          img(src = "challenge2_plot_a.png", width = "100%")
        } else if (user_choices$challenge2 == "B") {
          img(src = "challenge2_plot_b.png", width = "100%")
        } else if (user_choices$challenge2 == "C") {
          img(src = "challenge2_plot_c.png", width = "100%")
        }, # Close the if statement for Challenge 2 outcome
        
        # Challenge 3 outcome
        if (user_choices$challenge3 == "A") {
          img(src = "challenge3_plot_a.png", width = "100%")
        } else if (user_choices$challenge3 == "B") {
          img(src = "challenge3_plot_b.png", width = "100%")
        } else if (user_choices$challenge3 == "C") {
          img(src = "challenge3_plot_c.png", width = "100%")
        },
        
        # Challenge 4 outcome
        if (user_choices$challenge4 == "A") {
          img(src = "challenge4_plot_a.png", width = "100%")
        } else if (user_choices$challenge4 == "B") {
          img(src = "challenge4_plot_b.png", width = "100%")
        },
        
        # Challenge 5 outcome
        if (user_choices$challenge5 == "A") {
          img(src = "challenge5_plot_a.png", width = "100%")
        } else if (user_choices$challenge5 == "B") {
          img(src = "challenge5_plot_b.png", width = "100%")
        } else if (user_choices$challenge5 == "C") {
          img(src = "challenge5_plot_c.png", width = "100%")
        },
        
        # Challenge 6 outcome
        if (user_choices$challenge6 == "A") {
          img(src = "challenge6_plot_a.png", width = "100%")
        } else if (user_choices$challenge6 == "B") {
          img(src = "challenge6_plot_b.png", width = "100%")
        } else if (user_choices$challenge6 == "C") {
          img(src = "challenge6_plot_c.png", width = "100%")
        }
      ) # Close the f7Card for the outcome summary
    } # Close the outermost else if for the outcome summary
  }) # Close renderUI
  
  
  
  # Observe the "Submit" button for each challenge
  observeEvent(input$challenge_1_submit, {
    user_choices$challenge1 <- input$challenge_1_choice
    current_challenge(3)
  })
  
  observeEvent(input$challenge_2_submit, {
    user_choices$challenge2 <- input$challenge_2_choice
    current_challenge(4)
  })
  
  observeEvent(input$challenge_3_submit, {
    user_choices$challenge3 <- input$challenge_3_choice
    current_challenge(5)
  })
  
  observeEvent(input$challenge_4_submit, {
    user_choices$challenge4 <- input$challenge_4_choice
    current_challenge(6)
  })
  
  observeEvent(input$challenge_5_submit, {
    user_choices$challenge5 <- input$challenge_5_choice
    current_challenge(7)
  })
  
  observeEvent(input$challenge_6_submit, {
    user_choices$challenge6 <- input$challenge_6_choice
    current_challenge(8)  # Move to the outcome summary
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

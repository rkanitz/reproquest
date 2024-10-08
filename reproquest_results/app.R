library(shiny)
library(png)

ui <- fluidPage(
  titlePanel("Can you find yourself?"),
  
  mainPanel(
    uiOutput("image_container")
  )
)

server <- function(input, output) {
    # Read the PNG file from the data folder
    img_path <- "data/plot.png"
    
    # # Check if the file exists
    # if (!file.exists(img_path)) {
    #   stop("Image file not found. Please ensure 'picture.png' is in the 'data' folder.")
    # }
    
    # Read the PNG file
    img <- readPNG(img_path)
    
    # Get image dimensions
    img_height <- dim(img)[1]
    img_width <- dim(img)[2]
    
    # Set maximum dimension (adjust this value as needed)
    max_dim <- 400
    
    # Calculate scaling factor
    scale_factor <- min(max_dim / max(img_width, img_height), 1)
    
    # Calculate new dimensions
    new_width <- round(img_width * scale_factor)
    new_height <- round(img_height * scale_factor)
    
    output$image_container <- renderUI({
      plotOutput("image", width = paste0(new_width, "px"), height = paste0(new_height, "px"))
    })
    
    output$image <- renderPlot({
      # Plot the image
      par(mar = c(0,0,0,0)) # Remove margins
      plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
      rasterImage(img, 0, 0, 1, 1)
    }, width = new_width, height = new_height)
  }

shinyApp(ui = ui, server = server)

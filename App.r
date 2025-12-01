# HEVY WRAPPED - SIMPLE VERSION
# Copy this entire file into RStudio and click "Run App"

# Step 1: Install packages (run these lines once in your R console)
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("readr")

# Step 2: Load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# ============================================================================
# USER INTERFACE (what the user sees)
# ============================================================================

ui <- fluidPage(
  # App title
  titlePanel("🏋️ Hevy Wrapped - Your Fitness Year in Review"),
  
  # Sidebar for file upload
  sidebarLayout(
    sidebarPanel(
      h3("Upload Your Data"),
      p("Export your workout data from Hevy app and upload the CSV here."),
      
      # File input
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", ".csv")),
      
      hr(),
      
      # Action button
      actionButton("analyze", "Analyze My Workouts!", 
                   class = "btn-primary btn-lg")
    ),
    
    # Main panel for outputs
    mainPanel(
      # Summary stats
      h3("Your Stats"),
      fluidRow(
        column(4, 
               wellPanel(
                 h4("Total Workouts"),
                 textOutput("total_workouts")
               )
        ),
        column(4,
               wellPanel(
                 h4("Total Volume"),
                 textOutput("total_volume")
               )
        ),
        column(4,
               wellPanel(
                 h4("Favorite Exercise"),
                 textOutput("top_exercise")
               )
        )
      ),
      
      hr(),
      
      # Charts
      h3("Your Progress"),
      plotOutput("workout_trend", height = "300px"),
      
      hr(),
      
      h3("Top 10 Exercises by Volume"),
      plotOutput("top_exercises_plot", height = "400px")
    )
  )
)

# ============================================================================
# SERVER (what happens behind the scenes)
# ============================================================================

server <- function(input, output, session) {
  
  # Store the uploaded data
  workout_data <- reactive({
    req(input$file)  # Require file to be uploaded
    
    # Read the CSV
    data <- read_csv(input$file$datapath)
    
    # Clean the data
    data <- data %>%
      mutate(
        # Parse dates
        start_time = dmy_hm(start_time),
        date = as.Date(start_time),
        # Calculate volume
        volume = weight_lbs * reps
      ) %>%
      # Remove rows without volume data
      filter(!is.na(volume))
    
    return(data)
  })
  
  # ---- OUTPUT 1: Total Workouts ----
  output$total_workouts <- renderText({
    data <- workout_data()
    n_workouts <- n_distinct(data$date)
    paste0("🗓️ ", n_workouts, " workouts")
  })
  
  # ---- OUTPUT 2: Total Volume ----
  output$total_volume <- renderText({
    data <- workout_data()
    total_vol <- sum(data$volume, na.rm = TRUE)
    paste0("💪 ", format(round(total_vol), big.mark = ","), " lbs")
  })
  
  # ---- OUTPUT 3: Top Exercise ----
  output$top_exercise <- renderText({
    data <- workout_data()
    
    top <- data %>%
      group_by(exercise_title) %>%
      summarise(total_volume = sum(volume, na.rm = TRUE)) %>%
      arrange(desc(total_volume)) %>%
      slice(1)
    
    paste0("⭐ ", top$exercise_title)
  })
  
  # ---- OUTPUT 4: Workout Trend Chart ----
  output$workout_trend <- renderPlot({
    data <- workout_data()
    
    # Count workouts by month
    monthly <- data %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(workouts = n_distinct(date))
    
    # Plot
    ggplot(monthly, aes(x = month, y = workouts)) +
      geom_line(color = "#1DB954", size = 1.5) +
      geom_point(color = "#1DB954", size = 3) +
      labs(
        title = "Workouts per Month",
        x = "Month",
        y = "Number of Workouts"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
  })
  
  # ---- OUTPUT 5: Top Exercises Chart ----
  output$top_exercises_plot <- renderPlot({
    data <- workout_data()
    
    # Get top 10 exercises by volume
    top_10 <- data %>%
      group_by(exercise_title) %>%
      summarise(total_volume = sum(volume, na.rm = TRUE)) %>%
      arrange(desc(total_volume)) %>%
      head(10)
    
    # Plot
    ggplot(top_10, aes(x = reorder(exercise_title, total_volume), 
                       y = total_volume / 1000)) +
      geom_col(fill = "#1DB954") +
      coord_flip() +
      labs(
        title = "Your Top 10 Exercises",
        x = "",
        y = "Total Volume (1000s lbs)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16)
      )
  })
}

# ============================================================================
# RUN THE APP
# ============================================================================

shinyApp(ui = ui, server = server)
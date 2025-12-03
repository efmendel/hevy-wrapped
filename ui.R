# ui.R - Simple Hevy Wrapped UI

library(shiny)

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
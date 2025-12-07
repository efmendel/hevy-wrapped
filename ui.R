library(shiny)
library(shinythemes)

ui <- navbarPage(
  title = "🏋️ Hevy Wrapped",
  theme = shinytheme("flatly"),
  
  # ---- TAB 1: INTRO ----
  tabPanel("Intro",
           fluidPage(
             column(width = 10, offset = 1, align = "center",
                    br(), br(),
                    h1("Welcome to Your Fitness Life in Review", style = "font-weight: 600;"),
                    h4("Upload your Hevy data to see your gym story.", style = "color: #7f8c8d;"),
                    br(), hr(),
                    
                    # Instructions
                    h4("How to get your Hevy workout data:"),
                    br(),
                    fluidRow(
                      column(width = 4, offset = 2, style = "text-align: left;",
                             p(strong("1."), "Open the Hevy App"),
                             p(strong("2."), "Tap on", strong("Profile"), " (bottom right)"),
                             p(strong("3."), "Tap on", strong("Settings"), " (Gear Icon)"),
                             p(strong("4."), "Scroll to", strong("Export & Import Data"))
                      ),
                      column(width = 4, style = "text-align: left;",
                             p(strong("5."), "Tap on", strong("Export Data")),
                             p(strong("6."), "Tap button", strong("Export Workouts")),
                             p(strong("7."), "Send the CSV file to yourself"),
                             p(strong("8."), "Upload the CSV file below")
                      )
                    ),
                    hr(), br(),
                    
                    # Upload Box
                    column(width = 6, offset = 3,
                           wellPanel(
                             style = "background-color: #f8f9fa; border-color: #e9ecef; padding: 30px;",
                             h4("Step 1: Choose your .csv file", style = "margin-top: 0;"),
                             fileInput("file", label = NULL, accept = c("text/csv", ".csv"), width = "100%"), 
                             helpText("Export this directly from the Hevy App Settings."),
                             br(),
                             actionButton("analyze", "Step 2: Analyze My Workouts!", class = "btn-primary btn-lg", width = "100%"),
                             br(), br(),
                             p("Once you click Analyze, explore tabs to gain insight about your fitness journey!", 
                               style = "font-size: 0.9em; color: #7f8c8d; font-style: italic; margin-bottom: 0;")
                           )
                    )
             )
           )
  ),
  
  # ---- TAB 2: FITNESS LIFE IN REVIEW ----
  tabPanel("Fitness Life in Review",
           fluidPage(
             br(),
             h3("Your Stats"),
             fluidRow(
               column(3, wellPanel(h4("Total Workouts"), textOutput("total_workouts"))),
               column(3, wellPanel(h4("Total Volume"), textOutput("total_volume"))),
               column(3, wellPanel(h4("Favorite Exercise"), textOutput("top_exercise"))),
               column(3, wellPanel(h4("Avg. Duration"), textOutput("avg_duration")))
             ),
             hr(),
             h3("Your Progress"),
             fluidRow(
               column(6, plotOutput("workout_trend", height = "300px")),
               column(6, plotOutput("top_exercises_plot", height = "300px"))
             ),
             hr(),
             h3("Consistency Tracker"),
             fluidRow(
               column(4, wellPanel(h4("Longest Streak"), textOutput("longest_streak_text"))),
               column(8, plotOutput("streak_plot", height = "250px"))
             ),
             br()
           )
  ),
  
  # ---- TAB 3: STRENGTH & GAINS (Updated with New Graphs) ----
  tabPanel("Strength & Gains",
           fluidPage(
             br(),
             
             # NEW: Most/Least Improved
             h3("Gains Analysis"),
             p("Percentage improvement in max weight lifted (First Month vs Last Month)."),
             fluidRow(
               column(6, 
                      h4("🚀 Most Improved"),
                      plotOutput("most_improved_plot", height = "400px")),
               column(6, 
                      h4("🐢 Least Improved"),
                      plotOutput("least_improved_plot", height = "400px"))
             ),
             
             hr(),
             
             # Existing 1RM Chart
             h3("Exercise Deep Dive"),
             wellPanel(
               fluidRow(
                 column(6, uiOutput("exercise_select_ui")), # Dynamic Dropdown
                 column(3, h4("Max 1RM Ever:"), textOutput("max_1rm")),
                 column(3, h4("Failure Rate:"), textOutput("failure_rate"))
               )
             ),
             
             tabsetPanel(
               tabPanel("Estimated 1RM Trend", 
                        br(),
                        plotOutput("progression_chart", height = "400px")),
               
               # NEW: Custom Scatter Plot
               tabPanel("Weight & Reps Scatter", 
                        br(),
                        p("Bubble size represents number of reps performed."),
                        plotOutput("exercise_scatter", height = "400px"))
             ),
             
             hr(),
             
             h3("Training Style"),
             fluidRow(
               column(6, 
                      h4("Rep Range Distribution"),
                      plotOutput("reps_histogram", height = "300px")),
               column(6, 
                      h4("Set Types (Intensity)"),
                      plotOutput("set_type_plot", height = "300px"))
             )
           )
  ),
  
  # ---- TAB 4: WORKOUT HABITS (Updated with Weekly Freq) ----
  tabPanel("Workout Habits",
           fluidPage(
             br(),
             h3("Consistency"),
             # NEW: Weekly Frequency
             plotOutput("weekly_frequency_plot", height = "300px"),
             hr(),
             
             h3("Routine Analysis"),
             fluidRow(
               column(6, 
                      h4("Workout Duration Distribution"),
                      plotOutput("duration_histogram", height = "300px")),
               column(6, 
                      h4("Most Common Routines"),
                      plotOutput("top_workout_title_plot", height = "300px"))
             ),
             hr(),
             h3("When You Train"),
             fluidRow(
               column(6, 
                      h4("Day of Week"),
                      plotOutput("dow_plot", height = "300px")),
               column(6, 
                      h4("Heaviest Sessions"),
                      plotOutput("strongest_days_plot", height = "300px"))
             )
           )
  ),
  
  # ---- TAB 5: MUSCLE SPLIT (New Pie Chart) ----
  tabPanel("Muscle Split",
           fluidPage(
             br(),
             h3("Training Balance (Push / Pull / Legs)"),
             p("Classification based on your exercise titles."),
             # NEW: Pie Chart
             plotOutput("workout_type_pie", height = "600px")
           )
  )
)
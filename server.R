library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

server <- function(input, output, session) {
  
  # process data
  workout_data <- eventReactive(input$analyze, {
    req(input$file)
    data <- read_csv(input$file$datapath, show_col_types = FALSE)
    
    data <- data %>%
      mutate(
        # 1. Parse Dates
        start_time = dmy_hm(start_time),
        end_time = dmy_hm(end_time),
        date = as.Date(start_time),
        week = floor_date(date, "week"), # Used for Weekly Frequency Chart
        
        # 2. Calculate Volume
        volume = weight_lbs * reps,
        
        # 3. Calculate Duration in Minutes
        duration_min = as.numeric(difftime(end_time, start_time, units = "mins")),
        
        # 4. Calculate Estimated 1RM (Epley Formula)
        est_1rm = ifelse(!is.na(weight_lbs) & reps < 30 & reps > 0, 
                         weight_lbs * (1 + (reps / 30)), 
                         NA)
      ) %>%
      filter(!is.na(volume))
    
    return(data)
  })
  
  # Helper for Workout Level Data
  workout_summary_data <- reactive({
    data <- workout_data()
    data %>%
      group_by(title, date) %>%
      summarise(duration_min = mean(duration_min, na.rm = TRUE), .groups = "drop") %>%
      filter(duration_min > 0 & duration_min <= 300)
  })
  
  # dropdown ui
  output$exercise_select_ui <- renderUI({
    data <- workout_data()
    valid_exercises <- data %>% count(exercise_title) %>% filter(n >= 5) %>% pull(exercise_title) %>% sort()
    selectInput("selected_exercise", "Select Exercise to Visualize:", choices = valid_exercises, selected = valid_exercises[1])
  })
  
  # simple text outputs
  output$total_workouts <- renderText({ paste0("🗓️ ", n_distinct(workout_data()$date), " workouts") })
  output$total_volume <- renderText({ paste0("💪 ", format(round(sum(workout_data()$volume, na.rm = TRUE)), big.mark = ","), " lbs") })
  output$top_exercise <- renderText({ 
    top <- workout_data() %>% group_by(exercise_title) %>% summarise(vol = sum(volume, na.rm = TRUE)) %>% arrange(desc(vol)) %>% slice(1)
    paste0("⭐ ", top$exercise_title) 
  })
  output$avg_duration <- renderText({ paste0("⏱️ ", round(mean(workout_summary_data()$duration_min, na.rm = TRUE)), " mins") })
  
  output$longest_streak_text <- renderText({
    dates <- sort(unique(workout_data()$date))
    if (length(dates) == 0) return("0 Days")
    streaks <- rep(1, length(dates))
    if (length(dates) > 1) {
      for (i in 2:length(dates)) {
        if (as.integer(dates[i] - dates[i - 1]) == 1) streaks[i] <- streaks[i - 1] + 1
        else streaks[i] <- 1
      }
    }
    paste0("🔥 ", max(streaks), " Days")
  })
  
  output$max_1rm <- renderText({
    req(input$selected_exercise)
    val <- max(workout_data()$est_1rm[workout_data()$exercise_title == input$selected_exercise], na.rm = TRUE)
    if(is.infinite(val)) return("N/A")
    paste0(round(val), " lbs")
  })
  
  output$failure_rate <- renderText({
    rate <- round((sum(workout_data()$set_type == "failure", na.rm = TRUE) / nrow(workout_data())) * 100, 1)
    paste0("💀 ", rate, "% Sets to Failure")
  })
  
  # plots for trends and habits
  
  output$workout_trend <- renderPlot({
    monthly <- workout_data() %>% mutate(month = floor_date(date, "month")) %>% group_by(month) %>% summarise(count = n_distinct(date))
    ggplot(monthly, aes(x = month, y = count)) + geom_line(color = "#1DB954", size = 1.5) + geom_point(color = "#1DB954", size = 3) +
      labs(title = "Workouts per Month", x = "", y = "") + theme_minimal()
  })
  
  output$top_exercises_plot <- renderPlot({
    top10 <- workout_data() %>% group_by(exercise_title) %>% summarise(vol = sum(volume, na.rm = TRUE)) %>% arrange(desc(vol)) %>% head(10)
    ggplot(top10, aes(x = reorder(exercise_title, vol), y = vol/1000)) + geom_col(fill = "#1DB954") + coord_flip() +
      labs(title = "Top 10 Exercises (Volume)", x = "", y = "1000s lbs") + theme_minimal()
  })
  
  output$streak_plot <- renderPlot({
    dates <- sort(unique(workout_data()$date))
    if (length(dates) == 0) return(NULL)
    streaks <- rep(1, length(dates))
    if (length(dates) > 1) {
      for (i in 2:length(dates)) {
        if (as.integer(dates[i] - dates[i - 1]) == 1) streaks[i] <- streaks[i - 1] + 1
        else streaks[i] <- 1
      }
    }
    ggplot(data.frame(date=dates, streak=streaks), aes(x=date, y=streak)) + geom_area(fill="#1DB954", alpha=0.4) + geom_line(color="#1DB954") +
      theme_minimal() + labs(title = "Streak History", x="", y="Streak")
  })
  
  output$weekly_frequency_plot <- renderPlot({
    weekly <- workout_data() %>% group_by(week) %>% summarise(workouts = n_distinct(date))
    avg_workouts <- mean(weekly$workouts)
    ggplot(weekly, aes(x = week, y = workouts)) + geom_line(color = "#E91E63", size = 1.2) + geom_point(color = "#E91E63", size = 3) +
      geom_hline(yintercept = avg_workouts, linetype = "dashed", color = "gray40") +
      annotate("text", x = max(weekly$week), y = avg_workouts, label = paste("Avg:", round(avg_workouts, 1)), vjust = -1, hjust = 1) +
      labs(title = "Weekly Workout Frequency", x = "Week", y = "Workouts") + theme_minimal()
  })
  
  output$duration_histogram <- renderPlot({
    ggplot(workout_summary_data(), aes(x = duration_min)) + geom_histogram(bins = 20, fill = "#9b59b6", color = "white") +
      labs(title = "Workout Duration", x = "Minutes", y = "Count") + theme_minimal()
  })
  
  output$top_workout_title_plot <- renderPlot({
    top5 <- workout_summary_data() %>% count(title) %>% arrange(desc(n)) %>% head(5)
    ggplot(top5, aes(x = reorder(title, n), y = n)) + geom_col(fill = "#9b59b6") + coord_flip() +
      labs(title = "Top Routine Names", x = "", y = "Workouts") + theme_minimal()
  })
  
  output$dow_plot <- renderPlot({
    days <- workout_data() %>% distinct(date) %>% mutate(wd = wday(date, label = TRUE, abbr = TRUE)) %>% count(wd)
    ggplot(days, aes(x = reorder(wd, n), y = n)) + geom_col(fill = "#34495e") + coord_flip() +
      labs(title = "Workouts by Day", x = "", y = "") + theme_minimal()
  })
  
  output$strongest_days_plot <- renderPlot({
    top5 <- workout_data() %>% group_by(date) %>% summarise(vol = sum(volume)) %>% arrange(desc(vol)) %>% head(5)
    ggplot(top5, aes(x = reorder(as.character(date), vol), y = vol/1000)) + geom_col(fill = "#34495e") + coord_flip() +
      labs(title = "Heaviest Sessions", x = "", y = "1000s lbs") + theme_minimal()
  })
  
  # plots for strengths and gains
  
  output$progression_chart <- renderPlot({
    req(input$selected_exercise)
    prog_data <- workout_data() %>% filter(exercise_title == input$selected_exercise, !is.na(est_1rm)) %>% group_by(date) %>% summarise(est_1rm = max(est_1rm))
    ggplot(prog_data, aes(x = date, y = est_1rm)) + geom_smooth(method = "loess", color = "gray", alpha = 0.2) +
      geom_line(color = "#1DB954", size = 1) + geom_point(color = "#2c3e50") +
      labs(title = paste("Strength Progress:", input$selected_exercise), y = "Est. 1RM (lbs)", x = "") + theme_minimal()
  })
  
  output$exercise_scatter <- renderPlot({
    req(input$selected_exercise)
    ex_data <- workout_data() %>% filter(exercise_title == input$selected_exercise)
    ggplot(ex_data, aes(x = date, y = weight_lbs)) +
      geom_point(aes(size = reps), alpha = 0.6, color = "#2196F3") +
      geom_smooth(method = "loess", se = TRUE, color = "#1976D2") +
      labs(title = "Weight & Reps Scatter", x = "", y = "Weight (lbs)", size = "Reps") + theme_minimal()
  })
  
  output$reps_histogram <- renderPlot({
    ggplot(workout_data() %>% filter(reps <= 20), aes(x = reps)) + geom_histogram(binwidth = 1, fill = "#34495e", color = "white") +
      scale_x_continuous(breaks = seq(1, 20, 2)) + labs(title = "Rep Range Frequency", x = "Reps", y = "Count") + theme_minimal()
  })
  
  output$set_type_plot <- renderPlot({
    ggplot(workout_data() %>% filter(set_type != "warmup"), aes(x = set_type, fill = set_type)) + geom_bar() +
      scale_fill_manual(values = c("normal" = "#3498db", "failure" = "#e74c3c", "drop_set" = "#f1c40f")) +
      labs(title = "Intensity (Set Types)", x = "", y = "Sets") + theme_minimal() + theme(legend.position = "none")
  })
  
  improvement_data <- reactive({
    workout_data() %>% mutate(month = floor_date(date, "month")) %>% group_by(exercise_title, month) %>%
      summarise(max_weight = max(weight_lbs, na.rm = TRUE), .groups = "drop") %>%
      group_by(exercise_title) %>%
      summarise(first_weight = first(max_weight), last_weight = last(max_weight), n_months = n()) %>%
      filter(n_months >= 2, first_weight > 0) %>%
      mutate(improvement_pct = ((last_weight - first_weight) / first_weight) * 100)
  })
  
  output$most_improved_plot <- renderPlot({
    data <- improvement_data() %>% arrange(desc(improvement_pct)) %>% head(10)
    if(nrow(data) == 0) return(NULL)
    ggplot(data, aes(x = reorder(exercise_title, improvement_pct), y = improvement_pct)) + geom_col(fill = "#4CAF50") + coord_flip() +
      labs(title = "Top 10 Most Improved", x = "", y = "% Improvement") + theme_minimal()
  })
  
  output$least_improved_plot <- renderPlot({
    data <- improvement_data() %>% arrange(improvement_pct) %>% head(10)
    if(nrow(data) == 0) return(NULL)
    ggplot(data, aes(x = reorder(exercise_title, -improvement_pct), y = improvement_pct)) + geom_col(fill = "#FF9800") + coord_flip() +
      labs(title = "Top 10 Least Improved", x = "", y = "% Change") + theme_minimal()
  })
  
  output$workout_type_pie <- renderPlot({
    types <- workout_data() %>% mutate(type = case_when(
      grepl("Leg|Calf|Squat", exercise_title, ignore.case = TRUE) ~ "Legs",
      grepl("Bench|Chest|Pec|Push|Shoulder|Tricep", exercise_title, ignore.case = TRUE) ~ "Push",
      grepl("Pull|Lat|Row|Back|Bicep|Curl", exercise_title, ignore.case = TRUE) ~ "Pull",
      TRUE ~ "Other")) %>%
      count(type) %>% mutate(pct = n / sum(n) * 100)
    
    ggplot(types, aes(x = "", y = n, fill = type)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(type, "\n", round(pct, 1), "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
      theme_void() + scale_fill_brewer(palette = "Set2") + labs(title = "Muscle Group Split")
  })
}
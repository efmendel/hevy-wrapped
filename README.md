# Hevy Wrapped 🏋️

A Spotify Wrapped-style summary for your workout data from Hevy, the workout tracking app.

---

## What Does This Do?

Upload your workout data and instantly see:

- 📊 Total workouts and volume lifted
- ⭐ Your favorite exercises  
- 📈 Workout frequency trends over time
- 💪 Top 10 exercises by total volume

---

## Getting Started

### 1. Install R and RStudio

- **Download R**: https://cran.r-project.org/
- **Download RStudio**: https://posit.co/download/rstudio-desktop/

### 2. Install Required Packages

Open RStudio and run these commands in the Console:
```r
install.packages("shiny")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("readr")
```

### 3. Get Your Hevy Data

1. Open the Hevy app on your phone
2. Go to **Settings** → **Export Data**
3. Download the CSV file to your computer

### 4. Run the App

1. Open `app.R` in RStudio
2. Click the **"Run App"** button (green play button at the top)
3. Click **"Browse"** to upload your Hevy CSV file
4. Click **"Analyze My Workouts!"**
5. See your stats! 🎉

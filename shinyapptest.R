library(tidyverse)
library(shiny)
library(tidyverse)
library(nflreadr)
library(nflfastR)
library(cfbfastR)

pbp <- load_pbp(2023)

#create a column called wp_filter that is the higher value between the home_wp and away_wp columns
pbp <- pbp %>% 
  mutate(wp_filter = if_else(home_wp > away_wp, home_wp, away_wp))
# use column desc, if there is not "incomplete" and pass == 1, then create a column called comp
pbp <- pbp %>% 
  mutate(comp = if_else(str_detect(desc, "incomplete") == FALSE & pass == 1, 1, 0))
# print column names
data <- pbp %>% 
  filter(pass == 1) %>%
  select(week, season, game_date, posteam, wp_filter, pass_location, passer,  passer_id, passer_player_name, first_down,
         receiver_id, receiver, receiver_player_name, cp, yards_gained, air_yards, yards_after_catch, epa, cpoe, comp, desc, xyac_epa,
         shotgun, down, ydstogo, air_epa, yac_epa, pass, comp_air_epa, comp_yac_epa, wpa, air_wpa, yac_wpa, comp_air_wpa, comp_yac_wpa)
# Read the data
data <- read.csv("rec.csv", header = TRUE, sep = ",")
data$week <- as.integer(data$week)
data <- data %>% filter(!is.na(receiver_id))

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("NFL Data Explorer"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Filter by Team
      selectInput("teamInput", "Team:",
                  choices = unique(data$posteam)),
      
      # Filter by Season
      selectInput("seasonInput", "Season:",
                  choices = unique(data$season)),
      
      # Filter by Weeks
      sliderInput("weeksInput", "Weeks:",
                  min = min(data$week), max = max(data$week),
                  value = c(min(data$week), max(data$week)),
                  step = 1), # Ensure only whole numbers
      
      # Filter by Garbage Time
      sliderInput("garbageTimeInput", "Garbage Time:",
                  min = 0, max = 100,
                  value = c(0, 100)),
      
      # Filter by Downs
      fluidRow(
        checkboxGroupInput("downsInput", "Downs:",
                           choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4),
                           selected = c(1, 2, 3, 4),
                           inline = TRUE) # Display horizontally
      ),
      
      # Filter by Yards To Go
      sliderInput("yardsToGoInput", "Yards To Go:",
                  min = 0, max = 40, # Adjusted range
                  value = c(0, 40)),
      
      # Apply Filters button
      actionButton("applyFiltersButton", "Apply Filters")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Tabset panel with tabs
      tabsetPanel(
        tabPanel("Filtered Data", tableOutput("filteredDataTable")), # New tab for filtered data
        tabPanel("YAC Data", tableOutput("yacDataTable")), # New tab for YAC data
        tabPanel("Analysis") # Placeholder for analysis tab
      )
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to filter data based on inputs
  filteredData <- reactive({
    filterData <- data
    
    # Filter by Team
    filterData <- filterData[filterData$posteam == input$teamInput, ]
    
    # Filter by Season
    filterData <- filterData[filterData$season == input$seasonInput, ]
    
    # Filter by Weeks
    filterData <- filterData[filterData$week >= input$weeksInput[1] & filterData$week <= input$weeksInput[2], ]
    
    # Filter by Garbage Time
    filterData <- filterData[filterData$wp_filter >= input$garbageTimeInput[1] & filterData$wp_filter <= input$garbageTimeInput[2], ]
    
    # Filter by Downs
    filterData <- filterData[filterData$down %in% input$downsInput, ]
    
    # Filter by Yards To Go
    filterData <- filterData[filterData$ydstogo >= input$yardsToGoInput[1] & filterData$ydstogo <= input$yardsToGoInput[2], ]
    
    return(filterData)
  })
  
  # Output filtered data table
  output$filteredDataTable <- renderTable({
    req(input$applyFiltersButton)
    
    filtered <- filteredData()
    
    if (nrow(filtered) == 0) {
      return("No data available with selected filters")
    }
    
    filtered %>%
      group_by(receiver_player_name) %>%
      summarise(Targets = sum(if_else(is.finite(pass), pass, 0)),
                Receptions = sum(if_else(is.finite(comp), comp, 0)),
                Yards = sum(if_else(is.finite(yards_gained), yards_gained, 0)),
                `First Downs` = sum(if_else(is.finite(first_down), first_down, 0)),
                `Total EPA` = sum(if_else(is.finite(epa), epa, 0)),
                `Total WP` = sum(if_else(is.finite(wpa), wpa, 0)),
                `cpoe` = sum(if_else(is.finite(cpoe), cpoe, 0)),
                `CPOE/tgt` = sum(if_else(is.finite(cpoe) & Targets > 0, cpoe / Targets, 0)),
                `CPOE/rec` = sum(if_else(is.finite(cpoe) & Receptions > 0, cpoe / Receptions, 0)),
                `EPA/tgt` = sum(if_else(is.finite(epa) & Targets > 0, epa / Targets, 0)),
                `EPA/rec` = sum(if_else(is.finite(epa) & Receptions > 0, epa / Receptions, 0)),
                `WP/tgt` = sum(if_else(is.finite(wpa) & Targets > 0, wpa / Targets, 0)),
                `WP/rec` = sum(if_else(is.finite(wpa) & Receptions > 0, wpa / Receptions, 0))) %>%
      arrange(desc(Receptions)) %>%
      na.omit()
  })
  
  # Output YAC data table
  output$yacDataTable <- renderTable({
    req(input$applyFiltersButton)
    
    filtered <- filteredData()
    
    if (nrow(filtered) == 0) {
      return("No data available with selected filters")
    }
    
    filtered %>%
      group_by(receiver_player_name) %>%
      summarise(Receptions = sum(if_else(is.finite(comp), comp, 0)),
                `Yards after Catch` = sum(if_else(is.finite(yards_after_catch), yards_after_catch, 0)),
                `YAC per Rec` = sum(if_else(Receptions > 0, `Yards after Catch` / Receptions, 0)),
                `YAC EPA` = sum(if_else(is.finite(yac_epa), yac_epa, 0)),
                `YAC WP` = sum(if_else(is.finite(yac_wpa), yac_wpa, 0)),
                `xPct YAC EPA` = sum(if_else(is.finite(xyac_epa), xyac_epa, 0)),
                `YAC EPA over xPct` = sum(if_else(is.finite(yac_epa) & is.finite(xyac_epa), yac_epa - xyac_epa, 0)),
                `YAC EPAoe/Rec` = sum(if_else(is.finite(yac_epa) & is.finite(xyac_epa) & Receptions > 0, (yac_epa - xyac_epa) / Receptions, 0))) %>%
      arrange(desc(Receptions)) %>%
      na.omit()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

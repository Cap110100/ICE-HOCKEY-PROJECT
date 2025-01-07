library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jpeg)
library(png)
library(grid)
library(fmsb) # For radar charts

# /*MIH_2024_teamstats <- readRDS("Data/MIH_2024_teamstats.rds")
# MIH_2023_teamstats <- readRDS("Data/MIH_2023_teamstats.rds")
# MIH_2024_playerstats <- readRDS("Data/MIH_2024_playerstats.rds")
# MIH_2023_playerstats <- readRDS("Data/MIH_2023_playerstats.rds")
# app_data <- readRDS("Data/app1.rds")
# MIH_2024_teamstats <- MIH_2024_teamstats |>
#   mutate(season = '2024')
# 
# MIH_2023_teamstats <- MIH_2023_teamstats |>
#   mutate(season = '2023')
# 
# MIH_2024_playerstats <- MIH_2024_playerstats |>
#   mutate(season = '2024')
# MIH_2023_playerstats <- MIH_2023_playerstats |>
#   mutate(season = '2023')
# 
# MIH_23_24_Teamstats <- bind_rows(MIH_2024_teamstats,MIH_2023_teamstats)
# MIH_23_24_playerstats <- bind_rows(MIH_2024_playerstats,MIH_2023_playerstats)
# MIH_23_24_playerstats$Player_name = paste(MIH_23_24_playerstats$First_name, MIH_23_24_playerstats$Last_name)
# MIH_23_24_playerstats <- MIH_23_24_playerstats |>
#   filter(Last_name != "Do Not Modify" & Last_name != "Team")
# player_profile <- app_data |>
#   filter(play_action_type == 'shot'& !is.na(home_team)&academic_year!=2022)|>
#   select(contest_id,game_period_id,academic_year,period_number,area_of_action,dsp_name,name_tabular,x_coordinate,y_coordinate,conference_name,play_action_type,play_shot_type)|>
#   group_by(contest_id,game_period_id,name_tabular,period_number,area_of_action)|>
#   arrange(contest_id,game_period_id,name_tabular,period_number,area_of_action)|>
#   mutate(new_side= ifelse(sum(x_coordinate >= 50) >= n() / 2, "right", "left"))|>
#   mutate(x=ifelse(new_side=='left',100-x_coordinate,x_coordinate),
#          y=ifelse(new_side=='left',100-y_coordinate,y_coordinate)) |>
#   select(contest_id, game_period_id, period_number,academic_year,area_of_action,dsp_name,name_tabular,conference_name,play_shot_type,x,y)

MIH_23_24_Teamstats <- readRDS("Data/MIH_23_24_Teamstats.rds")
MIH_23_24_playerstats <- readRDS("Data/MIH_23_24_playerstats.rds")
player_profile <- readRDS("Data/player_profile.rds")

## User Interface
ui <- fluidPage(
  titlePanel("IceScout Pro - Ice Hockey Scouting Platform"),
  
  tabsetPanel(
    tabPanel("Team Leaderboard",
             fluidRow(
               column(width = 4,
                      selectInput("conference_team", "Select Conference:",
                                  choices = unique(MIH_23_24_Teamstats$conference_name))),
               column(width = 4,
                      selectInput("season_team", "Select Season:",
                                  choices = unique(MIH_23_24_Teamstats$season))),
               column(width = 4,
                      selectInput("statistic_team", "Select Statistic:",
                                  choices = c("Pts", "G", "W", "L", "GS", "GA", "HT", "S", "A", "P", "Min.P", "Maj.P", 
                                              "Games.Mis", "DQ", "PPP", "FOP", "SP")))
             ),
             DT::dataTableOutput("leaderboard_team")
    ),
    tabPanel("Player Leaderboard",
             fluidRow(
               column(width = 4,
                      selectInput("season_player", "Select Season:",
                                  choices = unique(MIH_23_24_playerstats$season))),
               column(width = 4,
                      selectInput("statistic_player", "Select Statistic:",
                                  choices = c("G", "GS", "A", "Pts", "+/-", "PIM", "PPP", "SHG", "GWG", "OTG", "FOP")))
             ),
             DT::dataTableOutput("leaderboard_player")
    ),
    tabPanel("Player Performance Profile",
             fluidRow(
               column(width = 4,
                      selectizeInput("Player_season", "Select Season:",
                                     choices = unique(player_profile$academic_year))),
               column(width = 4,
                      selectizeInput(
                        inputId = "Player_PPP",
                        label = "Select Player:",
                        choices = NULL,  # Initialize with no choices
                        options = list(placeholder = 'Start typing to search for a player')
                      )),
             ),
             
             plotOutput("Player_Performance_Profile"),
             
             # Layout with radar plot, data table, and pie plot in one row
             fluidRow(
               column(width = 4, plotOutput("radar_plot")),
               column(width = 4, DT::dataTableOutput("player_profile_table")),
               column(width = 4, plotOutput("Pie_Plot"))
             )
    )
  )
)

## Server
server <- function(input, output, session) {
  img <- readPNG("www/Field.png")
  observeEvent(input$Player_season, {
    players_in_season <- player_profile %>%
      filter(academic_year == input$Player_season) %>%
      pull(dsp_name) %>%
      unique()
    
    updateSelectizeInput(session, "Player_PPP", choices = players_in_season, server = TRUE)
  })
  
  filtered_team_data <- reactive({
    req(input$conference_team, input$season_team, input$statistic_team)
    
    data <- MIH_23_24_Teamstats %>%
      filter(conference_name == input$conference_team & season == input$season_team) %>%
      select(team_name, Points, Games, wins, losses, Goals, Goals.Allowed, Hat.Tricks, Saves, Assists, Penalties, Minor.Penalty, Major.Penalty, Game.Misconduct, Disqualifications, Power.Play.Percentage, Faceoff.Percentage, Save.Percentage) %>%
      rename(Team = team_name,
             Pts = Points,
             G = Games,
             W = wins,
             L = losses,
             GS = Goals,
             GA = Goals.Allowed,
             HT = Hat.Tricks,
             S = Saves,
             A = Assists,
             P = Penalties,
             Min.P = Minor.Penalty,
             Maj.P = Major.Penalty,
             Games.Mis = Game.Misconduct,
             DQ = Disqualifications,
             PPP = Power.Play.Percentage,
             FOP = Faceoff.Percentage,
             SP = Save.Percentage) %>%
      mutate(PPP = round(PPP,2),
             FOP = round(FOP,2),
             SP = round(SP,2))
    
    data <- data %>%
      arrange(desc(!!sym(input$statistic_team))) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, everything())
    
    return(data)
  })
  
  filtered_player_data <- reactive({
    req(input$season_player, input$statistic_player)
    
    data <- MIH_23_24_playerstats %>%
      filter(season == input$season_player) %>%
      select(Player_name,team_name, season, Games, Goals, Assists, Points, Plus.Minus, Penalty.Minutes, 
             Power.Play.Goals, Short.Handed.Goals, Game.Winning.Goals, Overtime.Goals, 
             Faceoff.Percentage) %>%
      mutate(Faceoff.Percentage = round(Faceoff.Percentage, 2)) %>%
      rename(Player = Player_name,
             G = Games,
             GS = Goals,
             A = Assists,
             Pts = Points,
             `+/-` = Plus.Minus,
             PIM = Penalty.Minutes,
             PPG = Power.Play.Goals,
             SHG = Short.Handed.Goals,
             GWG = Game.Winning.Goals,
             OTG = Overtime.Goals,
             FOP = Faceoff.Percentage)
    
    data <- data %>%
      arrange(desc(!!sym(input$statistic_player))) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, everything())
    
    return(data)
  })
  
  output$leaderboard_team <- DT::renderDataTable({
    DT::datatable(filtered_team_data())
  })
  
  output$leaderboard_player <- DT::renderDataTable({
    DT::datatable(filtered_player_data())
  })
  
  observe({
    req(input$Player_PPP, input$Player_season)
    
    data <- player_profile %>%
      filter(dsp_name == input$Player_PPP & academic_year == input$Player_season) %>%
      filter(is.finite(x), is.finite(y)) %>%  # Remove non-finite values
      filter(play_shot_type %in% c("goal", "save", "block", "missed"))
    output$Player_Performance_Profile <- renderPlot({
      custom_colors <- c("goal" = "black",  # green
                         "save" = "#ff7f0e",  # orange
                         "block" = "#1f77b4", # blue
                         "missed" = "#d62728")  # red
      
      ggplot(data, aes(x = x * 2, y = y)) +
        annotation_raster(img, xmin = 0, xmax = 200, ymin = 0, ymax = 100) +
        geom_hex(aes(fill = play_shot_type), size = 0.5, alpha = 0.7) +
        scale_fill_manual(values = custom_colors, name = "Shot Type") +
        coord_fixed() +
        xlim(0, 200) + ylim(0, 100) +
        labs(title = paste("Heat Map of", input$Player_PPP, "(", data$name_tabular, ")")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", 
                                    hjust = 0.5, margin = margin(b=10)),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          plot.margin = margin(1, 1, 1, 1, "cm")
        )
      
      
    })
    
    radar_data <- reactive({
      selected_season <- input$Player_season
      selected_player <- input$Player_PPP
      
      stats <- c("Points", "Goals", "Assists", "Penalty.Minutes", "Game.Winning.Goals")
      
      # Step 1: Filter data for the selected season and replace NAs with 0
      season_data <- MIH_23_24_playerstats %>%
        filter(season == selected_season) %>%                      # Filter by season first
        select(Player_name, all_of(stats)) %>%                      # Select relevant columns
        mutate(across(all_of(stats), ~ replace_na(.x, 0)))          # Replace NAs with 0 using `across`
      
      # Step 2: Extract player-specific data
      player_data <- season_data %>%
        filter(Player_name == selected_player) %>%
        select(all_of(stats)) %>%
        pivot_longer(cols = all_of(stats),
                     names_to = "Statistic",
                     values_to = "Player_score")
      
      # Step 3: Handle cases where the player is not found
      if (nrow(player_data) == 0) {
        # Create a tibble with zero values for each statistic
        player_data <- tibble(
          Statistic = stats,
          Player_score = 0
        )
      }
      
      # Step 4: Calculate maximum values for each statistic within the season
      max_values <- season_data %>%
        summarise(across(all_of(stats), ~ max(.x, na.rm = TRUE))) %>%
        pivot_longer(cols = all_of(stats),
                     names_to = "Statistic",
                     values_to = "Stat_max")
      
      # Step 5: Combine player data with max values
      combined_data <- player_data %>%
        left_join(max_values, by = "Statistic")                      # Join on 'Statistic'
      
      # Optional: Arrange data in a specific order if desired
      combined_data <- combined_data %>%
        mutate(Statistic = factor(Statistic, levels = stats)) %>%    # Ensure order of stats
        arrange(Statistic)
      
      # Return the final combined data
      return(combined_data)
    })
    
    
    output$radar_plot <- renderPlot({
      player_data <- MIH_23_24_playerstats %>%
        select(Player_name, season, Points, Goals, Assists, Penalty.Minutes, Game.Winning.Goals) %>%
        mutate_all(~ ifelse(is.na(.), 0, .)) %>%
        filter(Player_name == input$Player_PPP & season == input$Player_season) %>%
        select(Player_name, Points, Goals, Assists, Penalty.Minutes, Game.Winning.Goals)
      
      maxmin_data <- MIH_23_24_playerstats %>%
        select(season, Points, Goals, Assists, Penalty.Minutes, Game.Winning.Goals) %>%
        mutate_all(~ ifelse(is.na(.), 0, .)) %>%
        select(Points, Goals, Assists, Penalty.Minutes, Game.Winning.Goals)
      
      min_values <- maxmin_data %>%
        summarise(across(everything(), ~ min(.x, na.rm = TRUE)))
      max_values <- maxmin_data %>%
        summarise(across(everything(), ~ max(.x, na.rm = TRUE)))
      min_values$Player_name <- 'min'
      max_values$Player_name <- 'max'
      
      data <- bind_rows(max_values, min_values, player_data) %>%
        select(-Player_name)
      
      colors_border = c(rgb(0.2, 0.5, 0.5, 0.9), rgb(0.8, 0.2, 0.5, 0.9), rgb(0.7, 0.5, 0.1, 0.9))
      colors_in = c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4))
      
      par(mar = c(1, 1, 1, 1))  # Adjust margins if necessary
      radarchart(data, axistype = 0, maxmin = FALSE,
                 pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
                 cglcol = "grey", cglty = 1, axislabcol = "black", cglwd = 0.8,
                 vlcex = 0.8)
      mtext(paste(input$Player_PPP, "Tendencies"), side = 3, line = -0.5, cex = 1.2, font = 2)
    })
    
    
    output$Pie_Plot <- renderPlot({
      Pie_data <- player_profile %>%
        filter(dsp_name == input$Player_PPP & academic_year == input$Player_season) %>%
        group_by(academic_year, dsp_name, play_shot_type) %>%
        arrange(academic_year, dsp_name, play_shot_type) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(fraction = count / sum(count)) %>%
        select(play_shot_type, count, fraction)
      
      Pie_data$ymax <- cumsum(Pie_data$fraction)
      Pie_data$ymin <- c(0, head(Pie_data$ymax, n = -1))
      Pie_data$label <- paste0(Pie_data$play_shot_type, "\n value: ", round(Pie_data$fraction, 2))
      Pie_data$labelPosition <- (Pie_data$ymax + Pie_data$ymin) / 2
      
      ggplot(Pie_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = play_shot_type)) +
        geom_rect() +
        geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
        scale_fill_brewer(palette = 4) +  coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(t = 21))) +
        ggtitle(paste("Shots conversion by", input$Player_PPP))
    })
    
    output$player_profile_table <- DT::renderDataTable({
      data <- radar_data()
      DT::datatable(data)
    })
  })
}

## Shinyapp

shinyApp(ui, server)






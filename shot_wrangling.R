# Load Packages
library(tidyverse)
library(hoopR)
library(dplyr)
library(purrr)
library(janitor)
library(devtools)

# gather player index for active players in 2024
players <- hoopR::nba_playerindex()
player_index <- players$PlayerIndex %>% 
  # filter to only include players who are active
  filter(TO_YEAR == 2024) %>% 
  select(player_id = PERSON_ID, player_first_name = PLAYER_FIRST_NAME, player_last_name = PLAYER_LAST_NAME)

# function to pull shot chart data for a specific player
fetch_shot_data <- function(player_id){
  tryCatch({
    shotchart <- hoopR::nba_shotchartdetail(
      context_measure = "FGA",
      player_id = player_id
    )
    
    # extract the shot data
    shot_data <- shotchart$Shot_Chart_Detail
    
    Sys.sleep(2.2) # pause to avoid hitting rate limits
    
    if(is.null(shot_data) || nrow(shot_data) == 0){
      message(paste("No shot data found for player:", player_id))
      return(NULL)
    }
    
    shot_data
  }, error = function(e){
    # Return NULL if an error occurs
    NULL
  }
  )
}

# loop through all games and players 
all_shot_data <- map_dfr(player_index$player_id, function(player_id){
  shot_data <- fetch_shot_data(player_id)
  if (!is.null(shot_data)){
    # add game and player details 
    shot_data <- shot_data %>% 
      mutate(player_id = player_id)
  }
  shot_data
})

# Load NBA schedule
nba_schedule24 <- hoopR::load_nba_schedule(seasons = most_recent_nba_season()) %>%
  filter(play_by_play_available == T)# nba schedule with only gamees that have pbp data

# Gather all game_ids from the nba_schedule24 data frame
game_ids24 <- nba_schedule24$id

# initialize a list to store box score data for each game in our game_ids24 list
box_scores24 <- list()

# Loop through each game_id and pull box score data
for (game_id in game_ids24){
  tryCatch({
    box_score <- hoopR::espn_nba_player_box(game_id = game_id)
    box_scores24[[as.character(game_id)]] <- box_score
  }, error = function(e){
    message(paste("Error with game_id:", game_id, "-", e$message))
  })
}

# combine all box score data into a single data frame
all_box_scores <- do.call(rbind, lapply(box_scores24, function(x) x))

# create all_box_scores composite key 
player_info <- all_box_scores %>% 
  mutate(
    composite_key = paste(athlete_display_name, team_display_name, sep = "_")
  ) %>% 
  distinct(composite_key, .keep_all = T)


# create all_shot_data composite key 
all_shot_data <- all_shot_data %>% 
  mutate(
    composite_key = paste(PLAYER_NAME, TEAM_NAME, sep = "_")
  )

# join these two 
all_shot_joined <- all_shot_data %>% 
  left_join(player_info, join_by("composite_key" == "composite_key"))

# save to your machine 
write_csv("FILE_NAME.csv", all_shot_joined, col_names = T)

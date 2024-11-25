# Load Packages 
library(ggplot2)
library(tidyverse)
library(devtools)
library(plotly)
library(grid)

# Load Data 
all_shots24 <- read_csv("all_shots24.csv", col_names = T)

# Load court dimensions
devtools <- source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")


# Grab all shot data for the boston celtics 
celtics <- all_shots24 %>% 
  filter(TEAM_NAME == "Boston Celtics" & SHOT_DISTANCE <= 35) %>% 
  # clean data to fit court dimensions 
  mutate(
    x = as.numeric(as.character(LOC_X)) / 10, 
    y = as.numeric(as.character(LOC_Y)) / 10 + hoop_center_y,
    SHOT_MADE_FLAG = as.factor(SHOT_MADE_FLAG)
   )

# Now do the same thing for the Raptors 
raptors <- all_shots24 %>% 
  filter(TEAM_NAME == "Toronto Raptors") %>% 
  # clean data to fit court dimensions 
  mutate(
    x = as.numeric(as.character(LOC_X)) / 10, 
    y = as.numeric(as.character(LOC_Y)) / 10 + hoop_center_y
  )
  



# Create court 
p <- ggplot()+
  geom_point(data = celtics, aes(x=x, y=y, color = SHOT_MADE_FLAG,
                                 text = paste("Player:", PLAYER_NAME,
                                              "<br>Shot Distance (Feet):", SHOT_DISTANCE),
                                 fill = SHOT_MADE_FLAG),
             size = 2, shape = 21, stroke = .5)+
  geom_path(data = court_points, 
            aes(x=x, y=y, group = desc))+
  scale_color_manual(values = c("green4", "gray20"), aesthetics = "color", breaks = c("0", "1"), labels = c("Made", "Missed"))+
  scale_fill_manual(values = c("green2", "red3"), aesthetics = "fill", breaks = c("0", "1"), labels = c("Made", "Missed"))

interactive_plot <- ggplotly(p, tooltip = "text")

interactive_plot

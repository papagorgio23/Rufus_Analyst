


# Question 6




# Load packages
library(gganimate)
library(cowplot)
library(tidyverse)

# Load NFL data
file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv"
tracking.example <- read_csv(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays) 

# Join datasets
tracking.example.merged <- tracking.example %>% 
  inner_join(games.sum) %>% 
  inner_join(plays.sum) 

# filter to single play
example.play <- tracking.example.merged %>% 
  filter(playId == 938)

# view play description
example.play %>% 
  select(playDescription) %>% 
  slice(1)


## find long TD play
new.play <- tracking.example.merged %>%
  group_by(playId) %>% 
  summarise(moments = n())#,
            event = last(event))# %>%
  filter(event == "touchdown")
  
test <- tracking.example.merged %>%
  left_join(new.play) %>%
  filter(gameId == 2017090700) %>%
  filter(event == "touchdown") %>%
  filter(team == "away") %>% 
  arrange(desc(moments))

table(test$playId)

3725

# filter to single play
example.play2 <- tracking.example.merged %>% 
  filter(playId == 3725)

str(tracking.example.merged)
summary(tracking.example.merged)


#### Plotting the data

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

# build visual
animate.play <- ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(data = example.play, aes(x = (xmax-y), y = x, shape = team,
                                      fill = team, group = nflId, 
                                      size = team, colour = team), alpha = 0.7) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))



# view gif
animate(animate.play, fps = 10, nframe = play.length.ex)

# save gif
anim_save("nfl_play1.gif")








## Specific boundaries for a given play
ymin <- max(round(min(example.play2$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play2$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)


# build visual
animate.play2 <- ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(data = example.play2, aes(x = (xmax-y), y = x, shape = team,
                                      fill = team, group = nflId, size = team, colour = team), alpha = 0.7) + 
  geom_text(data = example.play2, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex2 <- length(unique(example.play2$frame.id))

# view gif
animate(animate.play2, fps = 10, nframe = play.length.ex2)

# save gif
anim_save("nfl_play2.gif")






## FIFA
install.packages("ggforce")
library(ggforce)
devtools::install_github("jogall/soccermatics")
library(soccermatics)

statsbomb %>%
  filter(team.name == "France") %>%
  soccerShotmap(theme = "dark")

??soccerShotmap
statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerShotmap(theme = "grass", colGoal = "yellow", colMiss = "blue", legend = T)
statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerShotmap(theme = "grass")

soccerShotmap(argentina, )
table(statsbomb$type.name)

statsbomb %>%
  filter(type.name == "Pass" & team.name == "France") %>% 
  soccerHeatmap(x = "location.x", y = "location.y",
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Passing heatmap")

?soccerHeatmap
d3 <- statsbomb %>% 
  filter(type.name == "Pass" & team.name == "France") %>% 
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 1, 0)))

soccerPitch(arrow = "r",
            title = "France (vs Argentina, 30th June 2016)", 
            subtitle = "Pass map") +
  geom_segment(data = d3, aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y, col = pass.outcome), alpha = 0.75) +
  geom_point(data = d3, aes(x = location.x, y = location.y, col = pass.outcome), alpha = 0.5) +
  guides(colour = FALSE)


subset(tromso, id == 8)[1:1800,] %>%
  soccerPath(col = "red", theme = "grass", arrow = "r",
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)",
             subtitle = "Player #8 path (1' - 3')")

# great plot with 
subset(tromso, id == 8)[1:3600,] %>%
  soccerPath(col = "red", theme = "grass", arrow = "r",
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)",
             subtitle = "Player #8 path (1' - 6')")

# great plot with 
subset(tromso, id == 8)[1:12366,] %>%
  soccerPath(col = "red", theme = "grass", arrow = "r",
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)",
             subtitle = "Player #8 path (1' - 21')")

table(tromso$id)
# great plot with 
subset(tromso, id == 16)[18001:36000,] %>%
  soccerPath(col = "red", theme = "grass", arrow = "r",
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)",
             subtitle = "Player #8 path (31' - 60')")

tromso %>%
  dplyr::group_by(id) %>%
  dplyr::slice(1:1200) %>%
  soccerPath(id = "id", arrow = "r", 
             title = "Tromsø IL (vs. Strømsgodset, 3rd Nov 2013)", 
             subtitle = "Player paths (1')")


statsbomb %>% 
  filter(type.name == "Pass" & team.name == "France" & minute < 43) %>% 
  soccerPositionMap(id = "player.name", x = "location.x", y = "location.y", 
                    fill1 = "blue", theme = "grass",
                    arrow = "r", 
                    title = "France (vs Argentina, 30th June 2016)", 
                    subtitle = "Average pass position (1' - 42')")

statsbomb %>%
  filter(type.name == "Pressure" & team.name == "France") %>% 
  as.data.frame() %>% 
  soccerHeatmap(x = "location.x", y = "location.y", xBins = 21, yBins = 14,
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Defensive pressure heatmap")

heaploting <- statsbomb %>%
  filter(type.name == "Pressure" & team.name == "France")

statsbomb %>%
  filter(type.name == "Pass" & team.name == "France") %>% 
  as.data.frame() %>% 
  soccerHeatmap(x = "location.x", y = "location.y",
                title = "France (vs Argentina, 30th June 2016)", 
                subtitle = "Passing heatmap")

str(heaploting)

function (df, lengthPitch = 105, widthPitch = 68, xBins = 10, 
          yBins = NULL, kde = FALSE, arrow = c("none", "r", "l"), 
          colLow = "white", colHigh = "red", title = NULL, subtitle = NULL, 
          x = "x", y = "y") 

?rmarkdown::html_notebook
install.packages("rmdformats")
library(rmdformats)


?rpois
rpois(9, 10)

soccerPath(theme = "grass")


# look closer at data

str(statsbomb)

table(statsbomb$team.name)

# 
str(tromso)



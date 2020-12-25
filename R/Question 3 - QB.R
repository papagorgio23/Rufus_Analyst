


# Load libraries
library("tidyverse")
library("tidymodels")
library("parsnip")
library("brotools")
library("mlbench")
library("tibbletime")


# Load data
qb_by_game <- read_delim("qb_by_game.csv", 
                         "\t", 
                         escape_double = FALSE, 
                         trim_ws = TRUE)



str(qb_by_game)
summary(qb_by_game)
table(qb_by_game$player)



# add metrics
qb_by_game <- qb_by_game %>%
  filter(reg_play >= 10) %>% 
  mutate(age = season - yob,
         seasons_played = season - start) %>% 
  group_by(player) %>% 
  mutate(prev_yards_adj = lag(yards_adj),
         games_played = row_number()) %>% 
  ungroup() %>% 
  select(tm, opp, season, week, player, yards_adj, prev_yards_adj, everything()) %>% 
  arrange(player, season, week)


# aggregate data
qb_group <- qb_by_game %>%
  group_by(player) %>% 
  summarise(games_played = n(),
            avg_yds = mean(yards_adj),
            med_yds = median(yards_adj),
            sd_yds = sd(yards_adj),
            max_yds = max(yards_adj),
            min_yds = min(yards_adj),
            med_plays = median(reg_play),
            avg_plays = mean(reg_play))


# filter out qbs with few games
qb_group1 <- qb_group %>% 
  filter(games_played >= 10)

# filter qbs
qbs_list <- qb_group1$player
qb_by_game1 <- qb_by_game %>% 
  filter(player %in% qbs_list)


# The function to use at each step is `mean`.
# The window size is 5
rolling_mean3 <- rollify(mean, window = 3)
rolling_mean6 <- rollify(mean, window = 6)
rolling_mean9 <- rollify(mean, window = 9)

summary(qb_by_game1)
# add metrics
qb_by_game1 <- qb_by_game1 %>%
  group_by(player) %>% 
  mutate(rolling_3 = lag(rolling_mean3(yards_adj)),
         rolling_6 = lag(rolling_mean6(yards_adj)),
         rolling_9 = lag(rolling_mean9(yards_adj)),
         median_yds = 5.324,
         experience = case_when(
           games_played <= 48 ~ "Young",
           games_played > 48 & games_played <= 96 ~ "Prime",
           TRUE ~ "Old"
           ))

# ggplot(aes(rolling_3, yards_adj, col = pow))

qb_by_game1 %>% 
  ggplot(aes(rolling_6, yards_adj, col = experience)) +
  geom_point(alpha=0.5) +
  geom_smooth() +
  #scale_color_manual(values = c("#D53E4F", "#3ed5c4", "green")) +
  labs(x = "3 Game Average",
       y = "Yards Per Attempt") +
  theme_bw() +
  theme(legend.title=element_blank())  
  # coord_equal()


hist(qb_by_game1$rolling_3)
hist(qb_by_game1$rolling_6)
hist(qb_by_game1$rolling_9)



## basic regression
reg3 <- lm(yards_adj ~ prev_yards_adj + rolling_3 + rolling_6 + rolling_9 + age + games_played, data = qb_by_game1) 
summary(reg1)

reg4 <- lm(yards_adj ~ prev_yards_adj + rolling_3 + rolling_9 + seasons_played, data = qb_by_game1) 
summary(reg2)


with(qb_by_game1, plot(seasons_played, yards_adj))
abline(reg2, col = "red")

with(data, plot(Exercise, Mood))
abline(reg1)




## outliers
bad <- qb_by_game %>% 
  filter(yards_adj >20 | yards_adj < 0)

## EDA
qb_by_game %>%
  ggplot() +
  geom_histogram(aes(x = yards_adj), binwidth = 1, alpha = 0.9, fill = "blue", color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nYards Per Attempt Adjusted",
    y = "Count",
    title = "QB Adjusted Yards per Attempt Distribution",
    subtitle = "2000-2019",
    caption = "Data: @RufusPeabody"
  )


## EDA
qb_by_game %>%
  ggplot() +
  geom_histogram(aes(x = reg_play), binwidth = 2, alpha = 0.9, fill = "skyblue", color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nEffective Plays",
    y = "Count",
    title = "QB's Effective Plays per Game Distribution",
    subtitle = "2000-2019",
    caption = "Data: @RufusPeabody"
  )





##### COR PLOT

## heatmap
cor_qb1 <- 
  qb_by_game1 %>% 
  ungroup() %>% 
  select(3, 4, 6:18) %>% 
  replace(., is.na(.), 5.2) %>% 
  cor()
str(qb_by_game1)
# correlation plot
corrplot(cor_qb1, col = colorRampPalette(c("royalblue", "white", "orangered2"))(200))

5.324
summary(qb_by_game)
summary(qb_by_game1)



##### KERAS RNN
ggplot(qb_by_game1, aes(x = 1:nrow(qb_by_game1), y = yards_adj, color = experience)) + 
  geom_line() +
  theme_bw() +
  labs(
    x = "Games",
    y = "Adjusted Yards Per Attempt",
    title = "QB's Effective Plays per Game Distribution",
    subtitle = "2000-2019",
    caption = "Data: @RufusPeabody"
  )






install.packages("brotools")
# Load libraries
library("tidyverse")
library("tidymodels")
library("parsnip")
library("brotools")
library("mlbench")


# Load data
college <- read_table2("Data/cfb_games_for_ml_task.csv")
View(college)


# add score diff and implied scores
college <- college %>%
  mutate(score_diff = opp_score - tm_score,
         error_rate = score_diff - tm_line,
         abs_error = abs(error_rate),
         home_win = ifelse(score_diff < 0, 1, 0),
         imp_tm_score = (total/2) - (tm_line/2),
         imp_opp_score = (total/2) + (tm_line/2))



# filter out huge lines
college <- college %>% 
  filter(between(tm_line, -50, 50))


summary(college)
mean(abs(college$error_rate))
sd(college$error_rate)

sd_ncaaf <- sd(college$error_rate)

## the Colts are 7 point favorites going into the superbowl. What is the probability that they will win the game
1 - (pnorm(.5, mean = 7, sd = sd_ncaaf)) # mean is the spread. and we want it to be greater than .5 but the function is for less than or equal to because of this we have to have 1 - percentage we get to find the precentage of it being greater than .5
# they have a 68.04% chance of winning the game

## probabiltiy of a tie... Big number has to come first and subtract the small number
(pnorm(.5, mean = 7, sd = sd_ncaaf)) - (pnorm(-.5, mean = 7, sd = sd_ncaaf))
## probability of a tie is 2.53% chance


### Assuming that a Tie = 50% win prob

# function to calculate win prob
win_prob <- function(home_line = -3, sd = 15.75){
  
  # change sign of the line number
  home_line <- home_line * -1
  
  # prob they win game in regulation
  winner <- 1 - (pnorm(.5, mean = home_line, sd = sd))
  
  ## probabiltiy of a tie... Big number has to come first and subtract the small number
  tie_prob <- (pnorm(.5, mean = home_line, sd = sd)) - (pnorm(-.5, mean = home_line, sd = sd))
  
  # add the probability of winning in overtime
  winner <- winner + (tie_prob*0.5)
  
  # return the probability
  win_message <- paste0("There is a ", round(winner * 100, 2), "% likelihood that your team will win!")
  return(win_message)
}

win_prob(-50)
sd_ncaaf <- 13.86


## redo function
# function to calculate win prob
win_prob <- function(home_line){
  
  # change sign of the line number
  home_line <- home_line * -1
  
  # prob they win game in regulation
  winner <- 1 - (pnorm(.5, mean = home_line, sd = sd_ncaaf))
  
  ## probabiltiy of a tie... Big number has to come first and subtract the small number
  tie_prob <- (pnorm(.5, mean = home_line, sd = sd_ncaaf)) - (pnorm(-.5, mean = home_line, sd = sd_ncaaf))
  
  # add the probability of winning in overtime
  winner <- winner + (tie_prob*0.5)
  
  # return the probability
  #win_message <- paste0("There is a ", round(winner * 100, 2), "% likelihood that your team will win!")
  return(winner)
}
game_lines <- seq(-40, 40, by = 0.5)
line_win <- tibble(game_lines, win_prob = NA)

line_win$win_prob <- win_prob(line_win$game_lines)

plot(line_win$win_prob)

line_win %>% 
  ggplot() +
  geom_line(aes(x = game_lines, y = win_prob)) +
  geom_hline(yintercept = 0.5, color = "red", linetype = 3) +
  labs(
    x = "Spread",
    y = "Win Probability",
    title = "Win Probability Chart by Spread",
    caption = "\n\nBased on normal random distribution with standard deviation of 15.75"
  ) +
  scale_x_continuous(limits = c(-40, 40),
                     breaks = seq(-40, 40, by = 5)) +
  theme_bw()

line_win %>% 
  ggplot() +
  geom_line(aes(x = game_lines, y = win_prob)) +
  geom_hline(yintercept = 0.5, color = "red", linetype=3) +
  labs(
    x = "Spread",
    y = "Win Probability",
    title = "Win Probability Chart by Spread",
    caption = "\n\nBased on normal random distribution with standard deviation of 15.75"
  ) +
  scale_x_continuous(limits = c(-10, 10),
                     breaks = seq(-10, 10, by = 1)) +
  scale_y_continuous(limits = c(0.25, 0.75),
                     breaks = seq(0.25, 0.75, by = 0.05)) +
  theme_bw()


bad <- college %>% 
  filter(error_rate < -50)

# plot score difference
college %>%
  ggplot() +
  geom_histogram(aes(x = score_diff), binwidth = 1, alpha = 0.9, fill = "red", color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nScore Differential",
    y = "Count",
    title = "Score Differential Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )

# plot absolute value for game margin
college %>%
  ggplot() +
  geom_histogram(aes(x = abs(score_diff)), binwidth = 1, alpha = 0.9, fill = "red", color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nScore Differential",
    y = "Count",
    title = "Score Differential Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )

college %>%
  ggplot() +
  geom_histogram(aes(x = error_rate), binwidth = 1, alpha = 0.9, fill = "blue", color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nPoints",
    y = "Count",
    title = "Difference in Spread vs. Actual Score Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )






home_scores <- college %>%
  select(gamecode, tm_score, imp_tm_score) %>%
  mutate(Team = "Home") %>%
  rename(Score = tm_score,
         `Implied Score` = imp_tm_score)

road_scores <- college %>%
  select(gamecode, opp_score, imp_opp_score) %>%
  mutate(Team = "Road") %>%
  rename(Score = opp_score,
         `Implied Score` = imp_opp_score)

scores <- rbind(home_scores, road_scores)




college %>%
  ggplot() +
  geom_histogram(aes(x = tm_score), binwidth = 1, alpha = 0.9, fill = "blue", color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nTeam Score",
    y = "Count",
    title = "Home Team Score Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )


college %>%
  ggplot() +
  geom_histogram(aes(x = tm_line), binwidth = 1, alpha = 0.9, fill = "blue", color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nLine",
    y = "Count",
    title = "Home Team Line Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )

college %>%
  ggplot() +
  geom_histogram(aes(x = opp_score), binwidth = 1, alpha = 0.9, fill = "red", color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nTeam Score",
    y = "Count",
    title = "Road Team Score Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )




college %>%
  ggplot() +
  geom_histogram(aes(x = score_diff), binwidth = 1, alpha = 0.9, fill = "red", color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nScore Differential",
    y = "Count",
    title = "Score Differential Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )



scores %>%
  filter(Score < 70) %>%
  ggplot(aes(x = Score, fill = Team)) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  scale_fill_manual(values = c("red", "blue")) +
  geom_hline(yintercept = 0, size = 1) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9)
  ) +
  #scale_x_continuous(breaks = seq(-10, 60, 10)) +
  labs(
    x = "\nTeam Score",
    y = "Count",
    title = "NCAAF Score Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )



scores %>%
  filter(Score < 70) %>%
  ggplot(aes(x = Score, fill = Team)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.6, 0.9)
  ) +
  scale_x_continuous(breaks = seq(-10, 60, 10))


?arrange
test_home <- college$tm_score
test_road <- college$opp_score
test_home <- test_home[order(test_home)]
test_road <- test_road[order(test_road)]

test <- as.data.frame(cbind(test_home, test_road))
test$same <- ifelse(test$test_home == test$test_road, 0, 1)

sum(test$same)

(test_road == test_home)

order()


# mean and standard deviations
summary(college)
mean(college$score_diff)
median(college$score_diff)
DescTools::Desc(college$score_diff)

mean(college$tm_score)
sd(college$tm_score)
sd(college$opp_score)
sd(college$score_diff)



(1 - (pnorm(7, mean = 3.5, sd = sd(college$tm_score))))*100




#### BUILD THE MODEL
model_data <- college %>%
  select(gamecode,)

train_test_split <- initial_split(college, prop = 0.9)

housing_train <- training(train_test_split)

housing_test <- testing(train_test_split)



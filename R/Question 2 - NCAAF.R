


#############################################
####   Q2. NCAAF Win Probability Model   ####
####          Normal Distribution        ####
#############################################



#' This script will generate win probabilities for each NCAA Football
#' based on the home spread. 





##############################
## Load necessary libraries ## --------------------------------------------------------------------------
##############################

library("tidyverse")
library("DescTools")
library("gt")




###################################
#####                         #####
##### ALL NECESSARY FUNCTIONS ##### ---------------------------------------------------------------------
#####                         #####
###################################

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
  #win_message <- paste0("There is a ", round(winner * 100, 2), "% likelihood that your team will win!")
  return(winner)
}





##############################
## Load necessary Datafiles ## --------------------------------------------------------------------------
##############################


# data from Rufus
college <- read_delim("Data/cfb_games_for_ml_task.csv", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)





##############################
##     Feature Engineer     ## --------------------------------------------------------------------------
##############################


# add score diff and implied scores
college <- college %>%
  mutate(score_diff = opp_score - tm_score,
         error_rate = score_diff - tm_line,
         abs_error = abs(error_rate),
         home_win = ifelse(score_diff < 0, 1, 0),
         imp_tm_score = (total/2) - (tm_line/2),
         imp_opp_score = (total/2) + (tm_line/2))


# plot error between spread and actual margin
college %>%
  ggplot() +
  geom_histogram(aes(x = error_rate), binwidth = 1, alpha = 0.9, fill = "blue", color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_bw() +
  geom_hline(yintercept = 0, size = 1) +
  labs(
    x = "\nError",
    y = "Count",
    title = "Difference in Spread vs. Actual Score Distribution",
    subtitle = "2005-2019",
    caption = "Data: @RufusPeabody"
  )




##############################
##       Calculations       ## --------------------------------------------------------------------------
##############################




# Calculate standard deviation of the error
sd_ncaaf <- sd(college$error_rate, na.rm = TRUE)
print(paste0("The standard deviation is ", round(sd_ncaaf,2)))



# function to calculate win prob
win_prob_1 <- function(home_line = -3, sd = 15.51){
  
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

# run function
win_prob_1(home_line = -5, sd = sd_ncaaf) 


# create a win probability dataframe by spread
game_lines <- seq(-40, 40, by = 0.5)
line_win <- tibble(game_lines, win_prob = NA)
line_win$win_prob <- win_prob(line_win$game_lines)


# we'll cap the spread at 40 both ways for historical lines
avg_win_prob <- college %>% 
  mutate(game_lines = ifelse(tm_line >= 40, 40, tm_line),
         game_lines = ifelse(game_lines <= -40, -40, game_lines)) %>% 
  group_by(game_lines) %>% 
  summarise(Games = n(),
            Hist_Win_Prob = mean(home_win)) %>% 
  arrange(game_lines)

# join historical with win probs
line_win <- line_win %>% 
  left_join(avg_win_prob, by = "game_lines")


##############################
##       Plot Results       ## --------------------------------------------------------------------------
##############################


# win probabilities by spread -40 to +40
line_win %>% 
  ggplot() +
  geom_line(aes(x = game_lines, y = win_prob)) +
  geom_hline(yintercept = 0.5, color = "red", linetype = 3) +
  labs(
    x = "\nSpread",
    y = "Win Probability",
    title = "Win Probability Chart by Spread",
    caption = "\n\nBased on normal random distribution with standard deviation of 15.75"
  ) +
  scale_x_continuous(limits = c(-40, 40),
                     breaks = seq(-40, 40, by = 5)) +
  theme_bw()


# win probabilities by spread -40 to +40 with Actuals
line_win %>% 
  ggplot() +
  geom_area(aes(x = game_lines, y = win_prob), fill = "lightblue", color = "skyblue", alpha = 0.5) +
  geom_line(aes(x = game_lines, y = Hist_Win_Prob), color = "black") +
  geom_hline(yintercept = 0.5, color = "red", linetype = 3) +
  labs(
    x = "\nSpread",
    y = "Win Probability",
    title = "Win Probability Chart by Home Spread",
    subtitle = "Historical Win Rate vs. Predicted Win Probability",
    caption = "\n\nBased on normal distribution with the mean equal to the home spread and a standard deviation of 15.75"
  ) +
  scale_x_continuous(limits = c(-40, 40),
                     breaks = seq(-40, 40, by = 5)) +
  theme_bw()



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







# mean and standard deviations
summary(college)
mean(college$error_rate)
median(college$error_rate)
sd(college$error_rate)
DescTools::Desc(college$error_rate)





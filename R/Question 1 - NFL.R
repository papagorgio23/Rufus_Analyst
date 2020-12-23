




###########################################
####   Q1. NFL Win Probability Model   ####
####       Monte Carlo Simulation      ####
###########################################



#' This script will generate win probabilities for each NFL matchup for
#' weeks 16 and 17 using only aggregated metrics from weeks 1-15. 





##############################
## Load necessary libraries ## --------------------------------------------------------------------------
##############################

library("tidyverse")
library("rvest")
library("teamcolors")
library("cowplot")
library("gt")




###################################
#####                         #####
##### ALL NECESSARY FUNCTIONS ##### ---------------------------------------------------------------------
#####                         #####
###################################


# Scrape NFL schedule 
scrape_game_ids <- function(season, type = "reg", weeks = NULL, teams = NULL) {
  
  # First check that the type is one of the required options:
  assertthat::assert_that(tolower(type) %in% c("reg", "pre", "post"),
                          msg = "Input for type is not valid! Should either be 'reg', 'pre', or 'post'.")
  
  # Next check that if the type is pre then the season is at least 1999:
  if (tolower(type) == "pre") {
    assertthat::assert_that(as.numeric(season) >= 2000,
                            msg = "Preseason game ids with data are only available starting in 2000!")
    # Otherwise check to see that it's at least 1998
  } else {
    assertthat::assert_that(as.numeric(season) >= 1998,
                            msg = "Regular and post-season game ids with data are only available starting in 1998!")
  }
  
  # Change the weeks from NULL if type is either pre or reg to their default values
  # (catching the case for 2011)
  
  if (is.null(weeks) & tolower(type) == "pre" & season != 2011) {
    weeks <- 0:4
  }
  if (is.null(weeks) & tolower(type) == "pre" & season == 2011) {
    weeks <- 1:4
  }
  if (is.null(weeks) & tolower(type) == "reg") {
    weeks <- 1:17
  }
  
  # Print out a message if the user entired values for weeks but for post type:
  if (!is.null(weeks) & tolower(type) == "post") {
    print("Ignoring the weeks input given the selected post-season game type.")
  }
  
  # Next check to see that if the type is pre then all of the weeks are between
  # 0 to 4, or 1 to 4 if season is 2011:
  if (tolower(type) == "pre" & season != 2011) {
    assertthat::assert_that(all(weeks >= 0) & all(weeks <= 4),
                            msg = "Please enter appropriate values for the pre-season weeks input between 0 to 4!")
  }
  if (tolower(type) == "pre" & season == 2011) {
    assertthat::assert_that(all(weeks >= 1) & all(weeks <= 4),
                            msg = "Please enter appropriate values for the 2011 pre-season weeks input between 1 to 4!")
  }
  
  # For regular season between 1 and 17:
  if (tolower(type) == "reg") {
    assertthat::assert_that(all(weeks >= 1) & all(weeks <= 17),
                            msg = "Please enter appropriate values for the regular season weeks input between 1 to 17!")
  }
  
  # Construct base schedule url for the season and type:
  base_url_schedule <- paste("http://www.nfl.com/schedules", season,
                             toupper(type), sep = "/")
  
  # Define the pipeline that will be used for type of games to scrape:
  
  fetch_game_ids <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-gameid=\"[0-9]{10}\"") %>%
    unlist() %>%
    stringr::str_extract("[0-9]{10}") %>%
    unlist()
  
  # Pipeline to get away team abbreviations:
  fetch_away_team_id <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-away-abbr=\"[:upper:]{2,3}\"") %>%
    unlist() %>%
    stringr::str_extract("[:upper:]{2,3}") %>%
    unlist()
  
  # Home team abbreviations:
  fetch_home_team_id <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-home-abbr=\"[:upper:]{2,3}\"") %>%
    unlist() %>%
    stringr::str_extract("[:upper:]{2,3}") %>%
    unlist()
  
  # Pipeline to fetch state of game:
  fetch_gamestate <- . %>%
    scrapeR::scrape(url = ., headers = TRUE, parse = FALSE) %>%
    unlist() %>%
    stringr::str_extract_all("data-gamestate=\"[:upper:]{2,4}\"") %>%
    unlist() %>%
    stringr::str_extract("[:upper:]{2,4}") %>%
    unlist()
  
  
  # If the type is post then just gather all the game ids from the post season
  # schedule page (since they are not separated by the week unlike the pre or
  # regular season) and put them together in data frame with week as 18 for now:
  
  if (tolower(type) == "post") {
    
    playoff_game_ids <- base_url_schedule %>%
      fetch_game_ids
    
    playoff_game_home <- base_url_schedule %>%
      fetch_home_team_id
    playoff_game_away <- base_url_schedule %>%
      fetch_away_team_id
    playoff_gamestate <- base_url_schedule %>%
      fetch_gamestate
    
    game_ids_df <- data.frame("game_id" = playoff_game_ids,
                              "week" = rep(18, length(playoff_game_ids)),
                              "home_team" = playoff_game_home,
                              "away_team" = playoff_game_away,
                              "state_of_game" = playoff_gamestate)
    
    # Else need to do it by the given weeks for the pre and regular season
  } else {
    
    # Now apply the pipeline to each week returning a data frame of game ids with
    # a column containing the week
    game_ids_df <- suppressWarnings(purrr::map_dfr(weeks, function(x) {
      game_ids <- paste0(base_url_schedule, x) %>%
        fetch_game_ids
      game_home <- paste0(base_url_schedule, x) %>%
        fetch_home_team_id
      game_away <- paste0(base_url_schedule, x) %>%
        fetch_away_team_id
      gamestate <- paste0(base_url_schedule, x) %>%
        fetch_gamestate
      data.frame("game_id" = game_ids,
                 "week" = rep(x, length(game_ids)),
                 "home_team" = game_home,
                 "away_team" = game_away,
                 "state_of_game" = gamestate)
    }))
  }
  
  # If teams is not null, check to make sure that there are games with
  # the given teams as either home or away and filter down to only
  # include those:
  if (!is.null(teams)) {
    assertthat::assert_that(any(teams %in% game_ids_df$home_team |
                                  teams %in% game_ids_df$away_team),
                            msg = "There are no games available for your entered team(s)!")
    game_ids_df <- game_ids_df %>%
      dplyr::filter(home_team %in% teams | away_team %in% teams)
  }
  
  
  # Return the game ids in a data frame with columns for the season and type:
  game_ids_df %>%
    dplyr::mutate(type = rep(type, nrow(game_ids_df)),
                  season = rep(season, nrow(game_ids_df))) %>%
    dplyr::select(type, game_id, home_team, away_team, week, season, state_of_game) %>%
    as.data.frame() %>%
    # Due to how the NFL displays Thursday Night Football games, only use 
    # the distinct rows:
    dplyr::distinct() %>%
    # Next create a variable with the url for each game:
    dplyr::mutate(game_url = sapply(game_id, create_game_json_url),
                  # Now for each game, if it is over based on the
                  # state_of_game field then access the scores of the
                  # game for the home and away teams:
                  home_score = purrr::map2_dbl(game_url, state_of_game,
                                               .f = function(x, y) {
                                                 ifelse(y == "POST",
                                                        max(RJSONIO::fromJSON(RCurl::getURL(x))[[1]]$home$score),
                                                        NA)
                                               }),
                  away_score = purrr::map2_dbl(game_url, state_of_game,
                                               .f = function(x, y) {
                                                 ifelse(y == "POST",
                                                        max(RJSONIO::fromJSON(RCurl::getURL(x))[[1]]$away$score),
                                                        NA)
                                               })) %>%
    return
}


# Helper function for scraping NFL schedule
# Creates the url with the location of NFL game JSON data
create_game_json_url <- function(game_id) {
  
  # First check to make sure that the first six digits of the game id are at
  # least 200904 (meaning they're at least from the 2009 season and have data
  # from the NFL API):
  assertthat::assert_that(as.numeric(stringr::str_sub(as.character(game_id), 1, 6)) > 200904,
                          msg = "You entered an invalid game id! JSON urls are supported starting with the 2009 season.")
  
  # Paste together the proper location of the JSON data
  paste0("http://www.nfl.com/liveupdate/game-center/", game_id, "/",
         game_id, "_gtd.json")
}

# Helper function for scraping NFL schedule
# Creates the url with the location of raw NFL play-by-play HTML
create_game_html_url <- function(game_id) {
  
  # First check to make sure that the first six digits of the game id are at
  # least 199804 (meaning they're at least from the 1998 season and have raw
  # HTML play-by-play:
  assertthat::assert_that(as.numeric(stringr::str_sub(as.character(game_id), 1, 6)) > 199804,
                          msg = "You entered an invalid game id! Raw HTML urls are supported starting with the 1998 season.")
  
  # Paste together the proper location of the raw HTML play-by-play data
  paste0("http://www.nfl.com/widget/gc/2011/tabs/cat-post-playbyplay?gameId=", 
         game_id, "&enableNGS=false")
}


# Scrape Pro Football Reference Website
get_data <- function(url){
  ## basic scrape has most the tables hidden so a work around is used
  # data = basic scrape
  # data1 = more complicated work around for hidden tables
  
  # Get the first 2 tables 
  data <- read_html(url) %>%
    html_table(fill = TRUE)
  
  # 1. AFC Standings
  AFC <- data[[1]] %>% 
    filter(!str_detect(Tm, "AFC"))
  
  # 2. NFC Standings
  NFC <- data[[2]] %>% 
    filter(!str_detect(Tm, "NFC"))
  
  # combine AFC & NFC tables
  Teams <- rbind(AFC, NFC)
  
  # grab the last 10 tables
  data1 <- read_html(url) %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>% 
    paste(collapse = "") %>%
    read_html() %>%
    html_table(fill = TRUE)
  
  # 5. Team Offense
  team_offense <- data1[[3]] %>%
    fix_headers1(df_name = "tm_off")
  
  # 6. Passing Offense
  pass_offense <- data1[[4]] %>%
    fix_headers2(df_name = "pass_off")
  
  # 7. Rushing Offense
  rush_offense <- data1[[5]] %>%
    fix_headers2(df_name = "rush_off")
  
  # 8. Kick & Punt Returns
  kick_returns <- data1[[6]] %>%
    fix_headers1(df_name = "return")
  
  # 9. Kicking & Punting
  kicking <- data1[[7]] %>%
    fix_headers1(df_name = "kick")
  
  # 10. Scoring Offense
  scoring <- data1[[8]] %>%
    fix_headers2(df_name = "score")
  
  # 11. Conversions
  conversion <- data1[[9]] %>%
    fix_headers1(df_name = "conv")
  
  # 12. Drive Averages
  drives <- data1[[10]] %>% 
    fix_headers1(df_name = "drive") %>% 
    mutate(`drive_Average Drive_Start` = gsub("Own ", "", `drive_Average Drive_Start`))
  
  # Join all the data together
  full_data <- Teams %>% 
    left_join(team_offense, by = c("Tm" = "tm_off_Tm")) %>% 
    left_join(pass_offense, by = c("Tm" = "pass_off_Tm")) %>% 
    left_join(rush_offense, by = c("Tm" = "rush_off_Tm")) %>% 
    left_join(kick_returns, by = c("Tm" = "return_Tm")) %>% 
    left_join(kicking, by = c("Tm" = "kick_Tm")) %>% 
    left_join(scoring, by = c("Tm" = "score_Tm")) %>% 
    left_join(conversion, by = c("Tm" = "conv_Tm")) %>% 
    left_join(drives, by = c("Tm" = "drive_Tm")) %>% 
    replace(., is.na(.), 0) 
  
  # remove percent sign
  full_data[, 2:length(full_data)] <- sapply(full_data[, 2:length(full_data)], cleanPercent)
  
  # change to numeric data
  full_data[, 2:length(full_data)] <- sapply(full_data[, 2:length(full_data)], as.numeric)
  
  # NAs are zeros on the website. Change missing to 0
  full_data <- full_data %>% 
    replace(., is.na(.), 0) 
  
  return(full_data)
}


# Helper function for the "get_data" function. Some of the tables on the website have 2 headers. 
# This will fix the tables and rename the columns.
fix_headers1 <- function(dataframe, df_name){
  headers <- colnames(dataframe)
  new_head <- dataframe[1,]
  headers <- unlist(ifelse(headers == "", new_head, paste0(headers, "_", new_head)))
  headers <- paste0(df_name, "_", headers)
  colnames(dataframe) <- headers
  dataframe <- dataframe[-1,]
  return(dataframe)
}

# Helper function for the "get_data" function. This will rename the 
# columns for the rest of the tables on the website.
fix_headers2 <- function(dataframe, df_name){
  headers <- colnames(dataframe)
  headers <- paste0(df_name, "_", headers)
  if (df_name == "pass_off") {
    headers[19] <- "pass_off_sack_yds"
  }
  colnames(dataframe) <- headers
  return(dataframe)
}
  

# Convert cities/nicknames to team abbreviations
convertTeamAbbreviation <- function(x){
  x[grep("Arizona", x, ignore.case=TRUE)] <- "ARZ"
  x[grep("Cardinals", x, ignore.case=TRUE)] <- "ARZ"
  x[grep("ARI", x, ignore.case=TRUE)] <- "ARZ"
  
  x[grep("Atlanta", x, ignore.case=TRUE)] <- "ATL"
  x[grep("Falcons", x, ignore.case=TRUE)] <- "ATL"
  
  x[grep("Baltimore", x, ignore.case=TRUE)] <- "BAL"
  x[grep("Ravens", x, ignore.case=TRUE)] <- "BAL"
  
  x[grep("Buffalo", x, ignore.case=TRUE)] <- "BUF"
  x[grep("Bills", x, ignore.case=TRUE)] <- "BUF"
  
  x[grep("Carolina", x, ignore.case=TRUE)] <- "CAR"
  x[grep("Panthers", x, ignore.case=TRUE)] <- "CAR"
  
  x[grep("Chicago", x, ignore.case=TRUE)] <- "CHI"
  x[grep("Bears", x, ignore.case=TRUE)] <- "CHI"
  
  x[grep("Cincinnati", x, ignore.case=TRUE)] <- "CIN"
  x[grep("Bengals", x, ignore.case=TRUE)] <- "CIN"
  
  x[grep("Cleveland", x, ignore.case=TRUE)] <- "CLE"
  x[grep("Browns", x, ignore.case=TRUE)] <- "CLE"
  
  x[grep("Dallas", x, ignore.case=TRUE)] <- "DAL"
  x[grep("Cowboys", x, ignore.case=TRUE)] <- "DAL"
  
  x[grep("Denver", x, ignore.case=TRUE)] <- "DEN"
  x[grep("Broncos", x, ignore.case=TRUE)] <- "DEN"
  
  x[grep("Detroit", x, ignore.case=TRUE)] <- "DET"
  x[grep("Lions", x, ignore.case=TRUE)] <- "DET"
  
  x[grep("Free", x, ignore.case=TRUE)] <- "FA"
  x[grep("Agent", x, ignore.case=TRUE)] <- "FA"
  
  x[grep("Green Bay", x, ignore.case=TRUE)] <- "GB"
  x[grep("Packers", x, ignore.case=TRUE)] <- "GB"
  
  x[grep("Houston", x, ignore.case=TRUE)] <- "HOU"
  x[grep("Texans", x, ignore.case=TRUE)] <- "HOU"
  
  x[grep("Indianapolis", x, ignore.case=TRUE)] <- "IND"
  x[grep("Colts", x, ignore.case=TRUE)] <- "IND"
  
  x[grep("Jacksonville", x, ignore.case=TRUE)] <- "JAX"
  x[grep("Jaguars", x, ignore.case=TRUE)] <- "JAX"
  
  x[grep("Kansas City", x, ignore.case=TRUE)] <- "KC"
  x[grep("Chiefs", x, ignore.case=TRUE)] <- "KC"
  
  x[grep("Miami", x, ignore.case=TRUE)] <- "MIA"
  x[grep("Dolphins", x, ignore.case=TRUE)] <- "MIA"
  
  x[grep("Minnesota", x, ignore.case=TRUE)] <- "MIN"
  x[grep("Vikings", x, ignore.case=TRUE)] <- "MIN"
  
  x[grep("New England", x, ignore.case=TRUE)] <- "NE"
  x[grep("Patriots", x, ignore.case=TRUE)] <- "NE"
  
  x[grep("New Orleans", x, ignore.case=TRUE)] <- "NO"
  x[grep("Saints", x, ignore.case=TRUE)] <- "NO"
  
  x[grep("Jets", x, ignore.case=TRUE)] <- "NYJ"
  
  x[grep("Giants", x, ignore.case=TRUE)] <- "NYG"
  
  x[grep("Oakland", x, ignore.case=TRUE)] <- "OAK"
  x[grep("Raiders", x, ignore.case=TRUE)] <- "OAK"
  
  x[grep("Philadelphia", x, ignore.case=TRUE)] <- "PHI"
  x[grep("Eagles", x, ignore.case=TRUE)] <- "PHI"
  
  x[grep("Pittsburgh", x, ignore.case=TRUE)] <- "PIT"
  x[grep("Steelers", x, ignore.case=TRUE)] <- "PIT"
  
  x[grep("San Diego", x, ignore.case=TRUE)] <- "LAC"
  x[grep("Chargers", x, ignore.case=TRUE)] <- "LAC"
  x[grep("L.A. Chargers", x, ignore.case=TRUE)] <- "LAC"
  
  
  x[grep("Saint Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("St Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("St. Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("Rams", x, ignore.case=TRUE)] <- "LAR"
  x[grep("L.A. Rams", x, ignore.case=TRUE)] <- "LAR"
  
  x[grep("San Francisco", x, ignore.case=TRUE)] <- "SF"
  x[grep("49ers", x, ignore.case=TRUE)] <- "SF"
  
  x[grep("Seattle", x, ignore.case=TRUE)] <- "SEA"
  x[grep("Seahawks", x, ignore.case=TRUE)] <- "SEA"
  
  x[grep("Tampa Bay", x, ignore.case=TRUE)] <- "TB"
  x[grep("Buccaneers", x, ignore.case=TRUE)] <- "TB"
  
  x[grep("Tennessee", x, ignore.case=TRUE)] <- "TEN"
  x[grep("Titans", x, ignore.case=TRUE)] <- "TEN"
  
  x[grep("Washington", x, ignore.case=TRUE)] <- "WAS"
  x[grep("Redskins", x, ignore.case=TRUE)] <- "WAS"
  
  return(x)
}


## gets rid of percent sign in the dataframes
cleanPercent <- function(x) {gsub("\\%", "", x)}


# Simulate offensive drive
sim_drive <- function(team = "NE"){
  
  # get stats needed
  team_stats <- monte %>% 
    filter(Tm == team)
  
  # get probabilities
  Scoring_perc <- team_stats$Scoring_perc
  Punt_perc <- team_stats$Punt_perc
  Turnover_perc <- team_stats$Turnover_perc
  TD_ratio <- team_stats$TD_ratio
  FG_ratio <- team_stats$FG_ratio
  
  # initialize score
  score <- 0
  
  # 3 possible results of drive: Team scores, Team punts, Turnover
  drive_result <- sample(size = 1, 
                         x = c("Score", "Turnover", "Punt"), 
                         prob = c(Scoring_perc, Turnover_perc, Punt_perc))
  
  # determine if score was a Touchdown or Field Goal
  if (drive_result == "Score") {
    score_type <- sample(size = 1, 
                         x = c("TD", "FG"), 
                         prob = c(TD_ratio, FG_ratio))
    
    
    # Get points
    if (score_type == "TD") {
      
      # kick the extra point
      extra_point <- sample(size = 1, 
                            x = c("Make", "Miss"), 
                            prob = c(0.935, 0.065)) # league average make extra point is 93.5%
      
      
      score <- ifelse(extra_point == "Make", 7, 6)
      
    } else {
      # if the score wasn't a touchedown then it was a field goal
      score <- 3
    }
  }
  return(score)
}


# function to simulate game
sim_game <- function(home_team = "NE", away_team = "BUF"){
  ### This function will simulation a full NFL game and return the score of each team
  
  # get number of drives for each team needed
  home_drives <- monte %>% 
    filter(Tm == home_team) %>% 
    pull(Num_Drives)
  
  # get number of drives for each team needed
  away_drives <- monte %>% 
    filter(Tm == away_team) %>% 
    pull(Num_Drives)
  
  # initialize scores
  home_score <- 0
  away_score <- 0
  
  # simulate and compute score for each home team drive in the game
  home_score <- sum(replicate(home_drives, sim_drive(home_team)))
  
  # simulate and compute score for each away team drive in the game
  away_score <- sum(replicate(away_drives, sim_drive(away_team)))
  
  # gather simulations together in a dataframe
  result <- data.frame(home_team = home_team, away_team = away_team, 
                       home_score = home_score, away_score = away_score)
  
  # return results
  return(result)
}



## GGPLOT THEME to plot monte carlo results
theme_538 <- function(base_size = 12, font = "Impact") {
  
  # Text setting
  txt <- element_text(size = base_size + 2, colour = "black", face = "plain")
  bold_txt <- element_text(
    size = base_size + 2, colour = "black",
    family = "Impact", face = "bold"
  )
  large_txt <- element_text(size = base_size + 4, color = "black", face = "bold")
  
  
  theme_minimal(base_size = base_size, base_family = font) +
    theme(
      # Legend Settings
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      
      # Backgrounds
      strip.background = element_blank(),
      strip.text = large_txt,
      plot.background = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      
      # Axis & Titles
      text = txt,
      axis.text = txt,
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = bold_txt,
      plot.title = large_txt,
      
      # Panel
      panel.grid = element_line(colour = NULL),
      panel.grid.major = element_line(colour = "#D2D2D2"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()
    )
}



####################################
###   12 tables on the website   ###
####################################

#' 1. AFC Standings
#' 2. NFC Standings
#' 3. AFC Playoff Standings
#' 4. NFC Playoff Standings
#' 5. Team Offense
#' 6. Passing Offense
#' 7. Rushing Offense
#' 8. Kick & Punt Returns
#' 9. Kicking & Punting
#' 10. Scoring Offense
#' 11. Conversions
#' 12. Drive Averages

# website with data
url <- "https://www.pro-football-reference.com/years/2019/"

# Scrape all data
all_data <- get_data(url)


# get remaining NFL schedule
nfl_weeks <- scrape_game_ids(2019, weeks = c(16,17))


# get only useful stats for the simulation
monte <- all_data %>% 
  select(Tm, `tm_off_Sc%`, `tm_off_TO%`, tm_off_Passing_TD, 
         tm_off_Rushing_TD, `drive_#Dr`, kick_Scoring_FGM) %>% 
  mutate(total_TD = tm_off_Passing_TD + tm_off_Rushing_TD,
         Punt_perc = (100 - `tm_off_Sc%` - `tm_off_TO%`)/100,
         TD_ratio = round(total_TD / (total_TD + kick_Scoring_FGM), 3),
         FG_ratio = round(kick_Scoring_FGM / (total_TD + kick_Scoring_FGM), 3),
         Num_Drives = round(`drive_#Dr`/14),
         Tm = convertTeamAbbreviation(Tm)) %>% 
  rename(Scoring_perc = `tm_off_Sc%`,
         Turnover_perc = `tm_off_TO%`) %>% 
  mutate(Scoring_perc = Scoring_perc/100,
         Turnover_perc = Turnover_perc/100) %>% 
  select(-c(tm_off_Passing_TD, tm_off_Rushing_TD, `drive_#Dr`, total_TD, kick_Scoring_FGM))


# fix LA 
nfl_weeks <- nfl_weeks %>% 
  mutate(home_team = ifelse(home_team == "LA", "LAR", home_team),
         away_team = ifelse(away_team == "LA", "LAR", away_team),
         home_team = ifelse(home_team == "ARI", "ARZ", home_team),
         away_team = ifelse(away_team == "ARI", "ARZ", away_team))




###############################
#####                     #####
##### Run the Simulations ##### ------------------------------------------------------------------------
#####                     #####
###############################


### SET UP SIM
set.seed(123)
sim_results <- data.frame()
i <- 1


######################
num_sims <- 20000  ### Change to shorten the sim time if needed...
######################


# start progress bar
pb <- txtProgressBar(min = 0, max = 32, style = 3)
for (j in 1:nrow(nfl_weeks)) {
  
  # get matchup
  home <- nfl_weeks$home_team[j]
  away <- nfl_weeks$away_team[j]
  
  while (i <= num_sims) {
    
    # simulate a single game
    game_result <- sim_game(home_team = home, away_team = away)
    
    # get score differential and attach game id and week
    game_result <- game_result %>%
      mutate(Home_line = away_score - home_score,
             game_id = nfl_weeks$game_id[j],
             week = nfl_weeks$week[j])
    
    # tag game result with the simulation number
    game_result$Sim <- i
    
    # combine current simulation with previous simulations
    sim_results <- rbind(sim_results, game_result)
    
    # update sim number
    i <- i + 1 
  }
  
  # reset the sim number for the next team
  i <- 1
  
  # combine current simulations for team with previous simulations for teams
  #sim_results <- rbind(sim_results, tmp)
  
  # update pregress bar 
  setTxtProgressBar(pb, j)
  print(paste0("  --  Finished with 20,000 simulations of ", home, " vs ", away, "!"))
}
# end progress bar
close(pb)




###############################
#####     Results of      ##### ------------------------------------------------------------------------
#####   the Simulations   #####
###############################



# Add flags for win, loss, and tie
sim_results <- sim_results %>% 
  mutate(Home_win = if_else(home_score > away_score, 1, 0),
         Away_win = if_else(away_score > home_score, 1, 0),
         Tie = if_else(away_score == home_score, 1, 0))


## simulation results team
sim_results_team <- sim_results %>% 
  group_by(game_id, week) %>% 
  summarise(home_team = last(home_team),
            away_team = last(away_team),
            home_wins = sum(Home_win),
            away_wins = sum(Away_win),
            ties = sum(Tie),
            home_win_prob = round(sum(home_wins) / n(),4),
            away_win_prob = round(sum(away_wins) / n(),4),
            tie_prob = round(ties / n(),4),
            home_score_med = median(home_score),
            away_score_med = median(away_score),
            home_score_avg = round(mean(home_score),2),
            away_score_avg = round(mean(away_score),2),
            home_score_sd = sd(home_score),
            away_score_sd = sd(away_score),
            home_line_med = median(Home_line),
            home_line_avg = round(mean(Home_line),2),
            home_line_sd = sd(Home_line)) %>% 
  select(1:4, 8:10, everything())

# get team colors for plots
nfl_colors <- teamcolors %>% 
  filter(league == "nfl") %>% 
  mutate(name = convertTeamAbbreviation(name))

# split colors for home and away
home_colors <- nfl_colors
away_colors <- nfl_colors

# rename columns home and away team colors
names <- colnames(nfl_colors)
home_names <- paste0("home_", names)
away_names <- paste0("away_", names)
colnames(home_colors) <- home_names
colnames(away_colors) <- away_names

## combine datasets
sim_plot_data <- sim_results %>% 
  left_join(sim_results_team, by = c("home_team", "away_team", "game_id", "week")) %>% 
  left_join(home_colors, by = c("home_team" = "home_name")) %>% 
  left_join(away_colors, by = c("away_team" = "away_name")) %>% 
  mutate(label = paste0("Week ", week, ":  ", away_team, "  @  ", home_team))




###############################
#####                     #####
#####     Plot Results    ##### ------------------------------------------------------------------------
#####                     #####
###############################


### Break the games out in groups of 4 to plot. 
games <- sort(unique(sim_plot_data$label))
games1 <- games[1:4]
games2 <- games[5:8]
games3 <- games[9:12]
games4 <- games[13:16]
games5 <- games[17:20]
games6 <- games[21:24]
games7 <- games[25:28]
games8 <- games[29:32]



## Plot simulations in groups of 4 games because of the size
games1_plots <- lapply(games1,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })


games2_plots <- lapply(games2,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })

games3_plots <- lapply(games3,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })

games4_plots <- lapply(games4,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })


games5_plots <- lapply(games5,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })

games6_plots <- lapply(games6,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })


games7_plots <- lapply(games7,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })

games8_plots <- lapply(games8,
                       function(game_label) {
                         
                         # Pull out the teams in the division
                         new_plot <- sim_plot_data %>% 
                           filter(label == game_label)
                         
                         # pull out data to add as text to plots
                         home_team <- new_plot$home_team[[1]]
                         away_team <- new_plot$away_team[[1]]
                         home_avg <- new_plot$home_score_avg[[1]]
                         away_avg <- new_plot$away_score_avg[[1]]
                         game_line <- new_plot$home_line_avg[[1]]
                         home_win <- new_plot$home_win_prob[[1]]
                         
                         # plot games where the home team is favored
                         if (home_avg >= away_avg) {
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1 , 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                           
                         } else { 
                           
                           # plot games where the away team is favored
                           new_plot %>% 
                             ggplot(aes(x = home_score)) +
                             geom_histogram(aes(color = last(home_primary), 
                                                fill = last(home_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             scale_color_identity() +
                             scale_fill_identity() +
                             geom_histogram(aes(x = away_score, 
                                                color = last(away_primary), 
                                                fill = last(away_primary)), 
                                            binwidth = 1, 
                                            alpha = 0.8) + 
                             geom_vline(aes(xintercept = median(away_score_avg), 
                                            color = last(away_primary))) + 
                             geom_vline(aes(xintercept = median(home_score_avg), 
                                            color = last(home_primary))) + 
                             annotate("label", 
                                      x = home_avg - 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 1, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(home_team, ": ", home_avg)) +
                             annotate("label", 
                                      x = away_avg + 0.5, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 4, 
                                      family = "Impact",
                                      label = paste0(away_team, ": ", away_avg)) +  
                             annotate("label", 
                                      x = 38, 
                                      y = nrow(new_plot)*0.085, 
                                      hjust = 0, 
                                      vjust = 1, 
                                      size = 5, 
                                      fontface = "bold", 
                                      family = "Impact",
                                      label = paste0("Home Line: ", 
                                                     game_line, 
                                                     "\nHome Win Probability: ", 
                                                     home_win*100, "%")) +
                             labs(x = "Score",
                                  y = "Count") +
                             facet_wrap(~label) +
                             theme_538() +
                             theme(
                               strip.text = element_text(hjust = 0)
                             )
                         }
                       })


# Display each 2x2 grid of plots
plot_grid(plotlist = games1_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games2_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games3_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games4_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games5_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games6_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games7_plots, ncol = 2, align = "hv")
plot_grid(plotlist = games8_plots, ncol = 2, align = "hv")






###############################
#####                     #####
#####    Table Results    ##### ------------------------------------------------------------------------
#####                     #####
###############################



## TABLE
# create table
results_table <- sim_results_team %>% 
  ungroup() %>% 
  mutate(week = paste0("Week ", week),
         week = factor(week, levels = c("Week 17", "Week 16"))) %>% 
  arrange(desc(home_win_prob)) %>%
  select(home_team, away_team, home_line_avg, home_win_prob, away_win_prob, tie_prob, 
         home_score_avg, home_score_med, away_score_avg, away_score_med, week) %>% 
  group_by(week) %>% 
  gt()

# fix style
results_table %>% 
  fmt_percent(columns = vars(home_win_prob, away_win_prob, tie_prob), decimals = 1) %>%
  tab_header(
    title = "Monte Carlo Simulation Win Probability",
    subtitle = "20,000 simulations"
  ) %>% 
  tab_spanner(
    label = "Win Probabilities",
    columns = vars(home_win_prob, away_win_prob, tie_prob)
  ) %>% 
  tab_spanner(
    label = "Median Scores",
    columns = vars(home_score_med, away_score_med)
  ) %>% 
  tab_spanner(
    label = "Avg. Scores",
    columns = vars(home_score_avg, away_score_avg)
  ) %>% 
  cols_label(
    home_team = "Home Team",
    away_team = "Away Team",
    home_line_avg = "Home Line",
    home_win_prob = "Home Win %",
    away_win_prob = "Away Win %",
    tie_prob = "Tie %",
    home_score_avg = "Avg. Home Score",
    home_score_med = "Med. Home Score",
    away_score_avg = "Avg. Away Score",
    away_score_med = "Med. Away Score"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_source_note(
    source_note = "Table: Jason Lee  |  Source Data: Pro Football Reference"
  )





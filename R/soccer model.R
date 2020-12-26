


## Soccer simulation

df <- read.csv("http://www.football-data.co.uk/mmz4281/1516/E0.csv", stringsAsFactors = FALSE)
sMatch <- paste(df$HomeTeam, df$AwayTeam, sep = " - ")
sTeams <- unique(c(df$HomeTeam, df$AwayTeam)) %>% sort


tmp1 <- df %>% 
  group_by(HomeTeam) %>%
  summarise(P = length(FTR),
            Pts = sum((FTHG > FTAG) * 3 + (FTHG == FTAG) * 1),
            GS = sum(FTHG),
            GC = sum(FTAG)) %>%
  ungroup()

tmp2 <- df %>% 
  group_by(AwayTeam) %>%
  summarise(P = length(FTR),
            Pts = sum((FTHG < FTAG) * 3 + (FTHG == FTAG) * 1),
            GS = sum(FTAG),
            GC = sum(FTHG)) %>%
  ungroup()

df.team.stats <- data.frame(Team = sTeams,
                            Points = tmp1$Pts + tmp2$Pts,
                            GD = (tmp1$GS + tmp2$GS) - (tmp1$GC + tmp2$GC),
                            TGS = (tmp1$GS + tmp2$GS)/(tmp1$P + tmp2$P),
                            TGC = (tmp1$GC + tmp2$GC)/(tmp1$P + tmp2$P), stringsAsFactors = FALSE)




df.new <- expand.grid(HomeTeam = sTeams, AwayTeam = sTeams, stringsAsFactors = FALSE) %>%
  filter(HomeTeam != AwayTeam) %>%
  # mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
  # filter(!(Match %in% sMatch)) #%>%
  # select(-Match) %>%
  mutate(HG = mean(df$FTHG),
         AG = mean(df$FTAG),
         TG = (mean(df$FTHG) + mean(df$FTAG))/2) %>%
  right_join(subset(df.team.stats, select = -c(Points, GD)),  by = c("HomeTeam" = "Team")) %>%
  right_join(subset(df.team.stats, select = -c(Points, GD)), by = c("AwayTeam" = "Team")) %>%
  setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG", 
             "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
  mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
         ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>%
  ungroup()



iSim <- 1000
n <- length(sTeams)

df.all <- data.frame(Team = rep(sTeams, iSim),
                     SimNo = rep(1:iSim, each = n),
                     Pts = rep(NA, n * iSim),
                     GD = rep(NA, n * iSim),
                     Rank = rep(NA, n * iSim))

library(utils)
?winProgressBar
pb <- txtProgressBar(min = 0, max = iSim, initial = 0, char = "=",
               width = NA, title = "Running Simulation", label = "Simulating....0% done", style = 1, file = "")



set.seed(1234)
for (i in 1:iSim){
  
  tmp <- df.new %>% 
    mutate(x1 = rpois(nrow(df.new), lambda = df.new$ExpHG), 
           x2 = rpois(nrow(df.new), lambda = df.new$ExpAG), 
           HPts = 3 * (x1 > x2) + 1 * (x1 == x2),
           APts = 3 * (x1 < x2) + 1 * (x1 == x2))
  
  res <- df.team.stats %>% select(Points, GD) + 
    tmp %>% group_by(HomeTeam) %>% summarise(Pts = sum(HPts),
                                             GD = sum(x1) - sum(x2)) %>% select(Pts, GD) + 
    tmp %>% group_by(AwayTeam) %>% summarise(Pts = sum(APts),
                                             GD = sum(x2) - sum(x1)) %>% select(Pts, GD) 
  
  df.all[(n*(i-1) + 1):(n*i), c("Pts", "GD")] <- res
  
  res$PGD <- res$Points + (res$GD - min(res$GD) + 1) / max((res$GD - min(res$GD) + 1) + 1)
  df.all[(n*(i-1) + 1):(n*i), c("Rank")] <- rank(-res$PGD, ties.method = "random")  
  
  info <- sprintf("Simulating ... %d%% done", round((i/iSim)*100))
  setTxtProgressBar(pb, i, label = info)  
}


df.all %>% 
  filter(Rank == 1) %>% 
  select(Team) %>% 
  table/iSim

# top 3 rankings
df.all %>% 
  group_by(Team) %>% 
  summarise(winner = (sum(Rank == 1)/iSim)*100,
            second = (sum(Rank == 2)/iSim)*100,
            third = (sum(Rank == 3)/iSim)*100) %>% 
  arrange(desc(winner), desc(second), desc(third))

# all rankings
table(df.all$Team, df.all$Rank)/iSim
  
  
  
#### FOR FOOTBALL NOW



tmp1 <- df %>% 
  group_by(HomeTeam) %>%
  summarise(P = length(FTR),
            Pts = sum((FTHG > FTAG) * 3 + (FTHG == FTAG) * 1),
            GS = sum(FTHG),
            GC = sum(FTAG)) %>%
  ungroup()

tmp2 <- df %>% 
  group_by(AwayTeam) %>%
  summarise(P = length(FTR),
            Pts = sum((FTHG < FTAG) * 3 + (FTHG == FTAG) * 1),
            GS = sum(FTAG),
            GC = sum(FTHG)) %>%
  ungroup()

df.team.stats <- data.frame(Team = sTeams,
                            Points = tmp1$Pts + tmp2$Pts,
                            GD = (tmp1$GS + tmp2$GS) - (tmp1$GC + tmp2$GC),
                            TGS = (tmp1$GS + tmp2$GS)/(tmp1$P + tmp2$P),
                            TGC = (tmp1$GC + tmp2$GC)/(tmp1$P + tmp2$P), stringsAsFactors = FALSE)




df.new <- expand.grid(HomeTeam = sTeams, AwayTeam = sTeams, stringsAsFactors = FALSE) %>%
  filter(HomeTeam != AwayTeam) %>%
  # mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
  # filter(!(Match %in% sMatch)) #%>%
  # select(-Match) %>%
  mutate(HG = mean(df$FTHG),
         AG = mean(df$FTAG),
         TG = (mean(df$FTHG) + mean(df$FTAG))/2) %>%
  right_join(subset(df.team.stats, select = -c(Points, GD)),  by = c("HomeTeam" = "Team")) %>%
  right_join(subset(df.team.stats, select = -c(Points, GD)), by = c("AwayTeam" = "Team")) %>%
  setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG", 
             "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
  mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
         ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>%
  ungroup()



iSim <- 1000
n <- length(sTeams)

df.all <- data.frame(Team = rep(sTeams, iSim),
                     SimNo = rep(1:iSim, each = n),
                     Pts = rep(NA, n * iSim),
                     GD = rep(NA, n * iSim),
                     Rank = rep(NA, n * iSim))



set.seed(1234)
for (i in 1:iSim){
  
  tmp <- df.new %>% 
    mutate(x1 = rpois(nrow(df.new), lambda = df.new$ExpHG), 
           x2 = rpois(nrow(df.new), lambda = df.new$ExpAG), 
           HPts = 3 * (x1 > x2) + 1 * (x1 == x2),
           APts = 3 * (x1 < x2) + 1 * (x1 == x2))
  
  res <- df.team.stats %>% select(Points, GD) + 
    tmp %>% group_by(HomeTeam) %>% summarise(Pts = sum(HPts),
                                             GD = sum(x1) - sum(x2)) %>% select(Pts, GD) + 
    tmp %>% group_by(AwayTeam) %>% summarise(Pts = sum(APts),
                                             GD = sum(x2) - sum(x1)) %>% select(Pts, GD) 
  
  df.all[(n*(i-1) + 1):(n*i), c("Pts", "GD")] <- res
  
  res$PGD <- res$Points + (res$GD - min(res$GD) + 1) / max((res$GD - min(res$GD) + 1) + 1)
  df.all[(n*(i-1) + 1):(n*i), c("Rank")] <- rank(-res$PGD, ties.method = "random")  
  
  info <- sprintf("Simulating ... %d%% done", round((i/iSim)*100))
  setTxtProgressBar(pb, i, label = info)  
}


df.all %>% 
  filter(Rank == 1) %>% 
  select(Team) %>% 
  table/iSim

df.all %>% 
  group_by(Team) %>% 
  summarise(winner = (sum(Rank == 1)/iSim)*100,
            second = (sum(Rank == 2)/iSim)*100,
            third = (sum(Rank == 3)/iSim)*100) %>% 
  arrange(desc(winner), desc(second), desc(third))





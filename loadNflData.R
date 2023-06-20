library(nflfastR)

#Downloads NFL data from 2000-2022
pbp <- nflfastR::load_pbp(2000:2022)
dim(pbp)
head(pbp)

#Subsets data
x <- pbp[c("play_id", "game_id", "season", "season_type", "week", "defteam", "posteam", "yardline_100", "game_seconds_remaining", "down", "ydstogo", "goal_to_go", "play_type", "shotgun", "no_huddle", "score_differential", "wp", "epa", "cp", "temp", "wind", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "qb_kneel")]
dim(x)
head(x)
write.csv(x, "/home/empty/Desktop/nflRunPass/nflData.csv")

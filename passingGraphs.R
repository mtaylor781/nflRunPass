library(magick)
library(nflplotR)
library(magrittr)
library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

#Load NFL data from years 2000-2022 form nflfastR, subset and clean data
pbp <- nflfastR::load_pbp(2000:2022)
passYear <- pbp[c("play_id", "season", "season_type", "posteam", "defteam", "play_type")]
passYear <- passYear %>%
  mutate(play_type = replace(play_type, play_type == 'pass', 1))
passYear <- passYear %>%
  mutate(play_type = replace(play_type, play_type == 'run', 0))
passYear <- passYear[passYear$play_type < 2,]
passYear <- passYear[!is.na(passYear$play_type),]
passYear <- transform(passYear, play_type = as.numeric(play_type))
data <- passYear %>%
  dplyr::filter(season_type == "REG")%>%
  dplyr::filter(!is.na(posteam))


# Create graph of Passing % by year
aggPassPercent <- data %>% group_by(season) %>% 
  summarise(passPercent=mean(play_type),
            .groups = 'drop')

aggPassPercentDf <- aggPassPercent %>% as.data.frame()

ggplot(aggPassPercentDf, aes(season, passPercent)) + 
  geom_line(color = "blue") +
  labs(x="Season", y="Passing %")


#2022
#Filter by year, group by team and calculate & of plays are pass
data2022 <- data[data$season == 2022,]

offense2022 <- data2022 %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_pass = mean(play_type, na.rm = TRUE))
defense2022 <- data2022 %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_pass = mean(play_type, na.rm = TRUE))

#Create graph
offense2022 %>% #http://127.0.0.1:41839/graphics/b362d123-9dae-4687-93a1-2f9d65108ed0.png
  dplyr::inner_join(defense2022, by = "team") %>%
  ggplot2::ggplot(aes(x = off_pass, y = def_pass)) +
  nflplotR::geom_mean_lines(aes(h_var = off_pass, v_var = def_pass)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  ggplot2::labs(
    x = "Offensive Passing %",
    y = "Opposing Team Passing %",
    caption = "Data: @nflfastR",
    title = "2022 NFL Passing Percentages"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"))


#2021
#Filter by year, group by team and calculate & of plays are pass
data2021 <- data[data$season == 2021,]

offense2021 <- data2021 %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_pass = mean(play_type, na.rm = TRUE))
defense2021 <- data2021 %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_pass = mean(play_type, na.rm = TRUE))

#Create graph
offense2021 %>%
  dplyr::inner_join(defense2021, by = "team") %>%
  ggplot2::ggplot(aes(x = off_pass, y = def_pass)) +
  nflplotR::geom_mean_lines(aes(h_var = off_pass, v_var = def_pass)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  ggplot2::labs(
    x = "Offensive Passing %",
    y = "Opposing Team Passing %",
    caption = "Data: @nflfastR",
    title = "2021 NFL Passing Percentages"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"))


#2001
#Filter by year, group by team and calculate & of plays are pass
data2001 <- data[data$season == 2001,]

offense2001 <- data2001 %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_pass = mean(play_type, na.rm = TRUE))
defense2001 <- data2001 %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_pass = mean(play_type, na.rm = TRUE))

#Create graph
offense2001 %>%
  dplyr::inner_join(defense2001, by = "team") %>%
  ggplot2::ggplot(aes(x = off_pass, y = def_pass)) +
  nflplotR::geom_mean_lines(aes(h_var = off_pass, v_var = def_pass)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  ggplot2::labs(
    x = "Offensive Passing %",
    y = "Opposing Team Passing %",
    caption = "Data: @nflfastR",
    title = "2001 NFL Passing Percentages"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"))

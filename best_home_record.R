library(cfbfastR)
library(tidyverse)
library(ggplot2)
library(glue)
library(rvest)
# we want "American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt"
game_info <- cfbd_game_info(year = 2024)

# Care about, Go5 and if undefeated at home. Interested in home_conference = Go5
go5 <- game_info %>%
  filter(home_conference %in% c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt"), 
         completed == TRUE)

u_week <- unique(go5$week)
games_filtered <- data.frame()
for (i in u_week){
  week_games <- go5[go5$week == i & go5$home_points < go5$away_points, ]
  games_filtered <- rbind(games_filtered, week_games)
}

losers <- games_filtered$home_team %>% unique()

winners <- go5 %>% filter(!home_team %in% losers)

winners$home_team %>% unique()

# Best Home Record --------------------------------------------------------
# year, teamId, team, conference, home_games, home_wins, home_losses
# conference abr: CUSA, MAC, MWC, SBC, AAC
yr <- 2020:2024

aac <- data.frame()
for (i in yr) {
aac_yr <- cfbd_game_records(year = i, conference = "AAC") %>%
  select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
aac <- rbind(aac, aac_yr)
}

cusa <- data.frame()
for(i in yr){
  cusa_yr <- cfbd_game_records(year = i, conference = "CUSA") %>% 
    select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
  cusa <- rbind(cusa, cusa_yr)
}

mac <- data.frame()
for (i in yr){
  mac_yr <- cfbd_game_records(year = i, conference = "MAC") %>%
    select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
  mac <- rbind(mac, mac_yr)
}

mwc <- data.frame()
for(i in yr){
  mwc_yr <- cfbd_game_records(year = i, conference = "MWC") %>%
    select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
  mwc <- rbind(mwc, mwc_yr)
}

sbc <- data.frame()
for(i in yr){
  sbc_yr <- cfbd_game_records(year = i, conference = "SBC") %>%
    select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
  sbc <- rbind(sbc, sbc_yr)
}


army_2020 <- cfbd_game_records(year = 2020, team = "Army") %>% 
  select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
army_2021 <- cfbd_game_records(year = 2021, team = "Army") %>% 
  select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
army_2022 <- cfbd_game_records(year = 2022, team = "Army") %>% 
  select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
army_2023 <- cfbd_game_records(year = 2023, team = "Army") %>% 
  select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")

army_df <- rbind(army_2020, army_2021, army_2022, army_2023)
go5_df <- rbind(go5_df, army_df)

win_rec <- go5_df %>% 
  group_by(team) %>%
  summarize(total_home_games = sum(home_games),
            total_home_wins = sum(home_wins),
            win_pct = total_home_wins/total_home_games) %>%
  arrange(desc(win_pct))

# current go5 teams
# 64 group of five teams
url <- "https://herosports.com/fbs-group-of-five-conferences-football-teams-g5-cpcp/#:~:text=The%20Group%20of%20Five%20refers,Belt%20are%20the%20G5%20conferences."
webpage <- read_html(url)
go5_names <- webpage %>%
  html_node("table") %>%
  html_table() %>%
  rename("School" = "X1")
  
go5_df %>% select(team) %>% unique()

# Texas Teams at Home -----------------------------------------------------
library(gt)
library(glue)
#baylor, tcu, texas a&m, texas state, utep
teams <- cfbd_team_info(year = 2024)
teams <- teams %>%
  filter(state == "TX")

tex_school <- teams$school
# cfbd_game_records(year = 2020, team = "UTSA")

tex <- data.frame()
for (i in yr){
  for (j in tex_school){
  df_tx <- cfbd_game_records(year = i, team = j) %>%
  select("year", "team", "total_games", "total_wins", "total_losses", "home_games", "home_wins", "home_losses")
  tex <- rbind(tex, df_tx)
 }
}



tex_df <- tex %>%
  group_by(team) %>%
  summarize(sum_home_wins = sum(home_wins),
            sum_tot_games = sum(home_games),
            win_pct = round((sum_home_wins/sum_tot_games) * 100, digits = 2)) %>%
  arrange(desc(win_pct)) %>%
  mutate(win_pct = paste0(win_pct, "%"))


utsa_number_one <-
tex_df %>%
  gt() %>%
  tab_header(title = "How Texas Teams Stack Up at Home") %>%
  cols_label(
    team = "School",
    sum_home_wins = "Total Home Wins",
    sum_tot_games = "Totoal Home Games",
    win_pct = "Win Percent"
  ) %>%
  tab_source_note(source_note = md("**Table:** @valen9_9 | **Data:** cfbfastR")) %>%
  gt_theme_538(table.width = px(550))

gtsave(utsa_number_one, filename = "tab_1.png")



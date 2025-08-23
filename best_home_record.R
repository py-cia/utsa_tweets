library(cfbfastR)
library(tidyverse)
library(ggplot2)
library(gt)
library(glue)
# Texas Teams at Home -----------------------------------------------------

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




library(tidyverse)
library(RSelenium)
library(netstat)

# UTSA jumped to FBS in 2012. Analysis will be from 2012-2024

# Selenium Troubleshoot ---------------------------------------------------

# checks location of chromedriver
chrome(retcommand = T, verbose = F, check = F)
# lists chrome version
binman::list_versions("chromedriver")
# check chrom for testing and download the current user version. paste where other chrome drivers which can be found using the chrome function

# Scraping ----------------------------------------------------------------

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "138.0.7204.94",
                             verbose = F,
                             port = free_port(),
                             phantomver = NULL)

remDr <- rs_driver_object$client


# Load fbs href
fbs_df <- read.csv("C:\\Users\\valen\\OneDrive\\Documents\\R_twitter\\fbs_df.csv")
utsa <- fbs_df %>%
  filter(School == "UTSA")

remDr$open()
remDr$navigate(utsa$ref[1])

# creating a function
utsa_data_v1 <- function(x){
  
  remDr$navigate(paste0("https://goutsa.com/sports/football/schedule/season/", x))
  
  Sys.sleep(3)
  
  utsa_box <- remDr$findElements(using = "class name", value = "schedule-event__content")
  
  weekday <- lapply(utsa_box, function(x) x$findChildElements(using = "xpath", ".//time[contains(@class, 'month')]")) %>%
    unlist() %>%
    lapply(function(x) x$getElementText()) %>%
    unlist()
  
  date <- lapply(utsa_box, function(x) x$findChildElements(using = "xpath", ".//span[contains(@class, 'day')]")) %>%
    unlist() %>%
    lapply(function(x) x$getElementText()) %>%
    unlist()
  
  opponent <- lapply(utsa_box, function(x) x$findChildElements(using = "xpath", ".//strong[@class = 'schedule-event-teams__name']")) %>%
    unlist() %>%
    lapply(function(x) x$getElementText()) %>%
    unlist()
  
  location <- lapply(utsa_box, function(x) x$findChildElements(using = "class name", value = "schedule-event-location")) %>%
    unlist() %>%
    lapply(function(x) x$getElementText()) %>%
    unlist()
  
  score <- lapply(utsa_box, function(x) x$findChildElements(using = "class name", value = "schedule-event-item-result__label")) %>%
    unlist() %>%
    lapply(function(x) x$getElementText()) %>%
    unlist() %>%
    str_extract(pattern = "\\d+-\\d+")
  
  outcome <- lapply(utsa_box, function(x) x$findChildElements(using = "class name", value = "schedule-event-item-result__label")) %>%
    unlist() %>%
    lapply(function(x) x$getElementText()) %>%
    unlist() %>%
    str_extract(pattern = regex("Win|Loss", ignore_case = T))
  
  school <- rep("UTSA", times = length(utsa_box))
  
  year = x
    
  utsa_df <- cbind(weekday, date, year, school, opponent, location, score, outcome) %>%
    as.data.frame()
  return(utsa_df)
}

# I can use a for loop or map function, but I use each function manually to see if there are any problems getting the data

# utsa_df_2011 <- utsa_data_v1(x = 2011)

utsa_df_2012 <- utsa_data_v1(x = 2012)

utsa_df_2013 <- utsa_data_v1(x = 2013)

utsa_df_2014 <- utsa_data_v1(x = 2014)

utsa_df_2015 <- utsa_data_v1(x = 2015)

utsa_df_2016 <- utsa_data_v1(x = 2016)

utsa_df_2017 <- utsa_data_v1(x = 2017)

utsa_df_2018 <- utsa_data_v1(x = 2018)

utsa_df_2019 <- utsa_data_v1(x = 2019)

utsa_df_2020 <- utsa_data_v1(x = 2020)

utsa_df_2021 <- utsa_data_v1(x = 2021)

utsa_df_2022 <- utsa_data_v1(x = 2022)

utsa_df_2023 <- utsa_data_v1(x = 2023)

utsa_df_2024 <- utsa_data_v1(x = 2024)

utsa_df <- rbind(utsa_df_2012,utsa_df_2013, utsa_df_2014, utsa_df_2015, utsa_df_2016, utsa_df_2017, utsa_df_2018, utsa_df_2019,
                 utsa_df_2020, utsa_df_2021, utsa_df_2022, utsa_df_2023, utsa_df_2024)

# Data Cleaning -----------------------------------------------------------

utsa_df$home_away <- ifelse(grepl("alamodome|san antonio", utsa_df$location, ignore.case = T), "home", "away")

utsa_df_v1 <- separate(utsa_df, col = score, into = c("utsa_score", "opponent_score"), sep = "-", remove = FALSE)

utsa_df_v1$opponent <- ifelse(utsa_df_v1$opponent == "Army West Point", "Army", utsa_df_v1$opponent)

utsa_df_v1$opponent <- ifelse(utsa_df_v1$opponent == "Texas St.", "Texas State", utsa_df_v1$opponent)

utsa_df_v1$opponent <- ifelse(utsa_df_v1$opponent == "Southern Mississippi", "Southern Miss", utsa_df_v1$opponent)

utsa_df_v1$opponent <- ifelse(utsa_df_v1$opponent == "WKU", "Western Kentucky", utsa_df_v1$opponent)

utsa_df_v2 <- utsa_df_v1[!is.na(utsa_df_v1$score), ]

utsa_df_v3 <- utsa_df_v2 %>%
  unite(col = "game_day", c("date", "year"), sep = ", ", remove = F)

utsa_df_v3$game_day <- mdy(utsa_df_v3$game_day)

write.csv(utsa_df_v3, "C:/Users/valen/OneDrive/Documents/R_twitter/utsa_df_v3.csv", row.names = FALSE)

# Visualization -----------------------------------------------------------
library(gt)
library(gtExtras)

# Game Streaks
best_streak <- utsa_df_v3 %>%
  mutate(streak_id = with(rle(outcome), rep(seq_along(lengths), lengths)),
         streak_type = outcome) %>%
  group_by(streak_id, streak_type) %>%
  reframe(
    start_date = min(game_day),
    end_date = max(game_day),
    streak_length = n()
  ) %>%
  arrange(desc(streak_length))

# Game streaks by opponent
utsa_streaks <- utsa_df_v3 %>%
  group_by(opponent) %>%
  reframe(
    streak_id = with(rle(outcome), rep(seq_along(lengths), lengths)),
    streak_type = outcome,
    game_day = game_day
  ) %>%
  group_by(opponent, streak_id, streak_type) %>%
  reframe(
    streak_length = n(),
    start_date = min(game_day),
    end_date = max(game_day))

utsa_streaks_sub <- utsa_streaks %>%
  filter(opponent %in% c("North Texas", "Rice", "Louisiana Tech", "UTEP", "Southern Miss", "UAB", "Texas State", "Army", "Florida Atlantic", "Middle Tennessee"), streak_type == "Win") %>%
  slice_max(streak_length, n = 1, by = opponent) %>%
  arrange(desc(streak_length)) %>%
  rename('Longest Streak' = streak_length,
         'Start Date' = start_date,
         'End Date' = end_date,
         "Opponent" = opponent) %>%
  select(-streak_id, - streak_type)

# Find last match-up
utsa_last_matchup <- utsa_df_v3 %>%
  filter(opponent %in% c("North Texas", "Rice", "Louisiana Tech", "UTEP", "Southern Miss", "UAB", "Texas State", "Army", "Florida Atlantic", "Middle Tennessee")) %>%
  group_by(opponent) %>%
  reframe('Last Matchup' = max(game_day),
          "Outcome" = last(outcome)) %>%
  mutate(Outcome = str_sub(Outcome, start = 1, end = 1),
         'Last Matchup' = str_c(Outcome, " ", `Last Matchup`)) %>%
  select(-Outcome)

# Current Streak
utsa_current <- utsa_streaks %>% 
  filter(opponent %in% c("North Texas", "Rice", "Louisiana Tech", "UTEP", "Southern Miss", "UAB", "Texas State", "Army", "Florida Atlantic", "Middle Tennessee")) %>%
  group_by(opponent) %>%
  reframe(
    current_streak = streak_length[streak_id == max(streak_id)],
    streak_type = streak_type[streak_id = max(streak_id)]
  ) %>%
  mutate(current_streak = ifelse(streak_type == "Loss", -current_streak, current_streak))

# Most frequent opponents
most_frq_opp <- utsa_df_v3 %>%
  group_by(opponent) %>%
  reframe(
    Wins = sum(outcome == "Win"),
    Games_Played = n(),
    Win_Percent = Wins/Games_Played
  ) %>%
  arrange(desc(Games_Played))

most_frq_opp_v1 <- most_frq_opp %>% 
  select(Opponent = opponent, Wins, 'Games Played' = Games_Played, 'Win %' = Win_Percent) %>%
  arrange(desc('Games Played'))

utsa_orange <- "#f15A22"
utsa_blue <- "#0c2340"

# AC image sources:
uab <- "https://theamerican.org/images/2025/7/29/UAB.png?width=50"
north_texas <- "https://theamerican.org/images/logos/North-Texas.png?width=50"
rice <- "https://theamerican.org/images/logos/R-Sidearm200.png?width=50"
army <- "https://theamerican.org/images/logos/army_logo_R.png?width=50"
fau <- "https://theamerican.org/images/2023/6/28/Florida_Atlantic.png?width=50"
utep <- "https://conferenceusa.com/images/2020/6/23/UTEP_primary.jpg?width=72&height=72&mode=max"
mt <- "https://conferenceusa.com/images/logos/Middle-Tenn.png?width=72&height=72&mode=max"
la_tech <- "https://conferenceusa.com/images/logos/LA-Tech.png?width=72&height=72&mode=max"
tx_st <- "https://sunbeltsports.org/images/logos/u1.png?width=88&height=88&mode=crop&anchor=center&scale=both"
sm <- "https://sunbeltsports.org/images/logos/Southern-Miss.png?width=88&height=88&mode=crop&anchor=center&scale=both"

most_frq_opp_v2 <- most_frq_opp_v1 %>%
  head(10) %>%
  mutate(logo_url = c(north_texas, rice, la_tech, utep, sm, uab, tx_st, army, fau, mt))

most_frq_opp_v3 <- most_frq_opp_v2 %>%
  left_join(utsa_streaks_sub, by = "Opponent") %>%
  select(-`Start Date`, -`End Date`) %>%
  left_join(utsa_current, by = c("Opponent" = "opponent")) %>%
  select(-streak_type) %>%
  left_join(utsa_last_matchup, by = c("Opponent" = "opponent")) 
  
utsa_tbl <- most_frq_opp_v3 %>%
  gt() %>%
  tab_header(
    title = md("**UTSA vs. Frequent Foes (2012-2024)**")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = utsa_blue),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  opt_row_striping() %>%
  fmt_percent(columns = 'Win %', decimals = 1) %>%
  gt_img_rows(columns = logo_url, height = 22) %>%
  cols_move_to_start(columns = logo_url) %>%
  cols_label(logo_url = "",
             current_streak = "Current Streak") %>%
  tab_options(
    table.border.bottom.style = "none",
    data_row.padding = px(6),
    table.border.top.style = "none"
  ) %>%
  data_color(
    columns = c(current_streak),
    colors = scales::col_bin(
      palette = c("red", "lightgray", "green"),
      domain = c(-2, 0, 8),
      bins = c(-Inf, -1, 0, Inf)
    )
  )

# table
utsa_tbl






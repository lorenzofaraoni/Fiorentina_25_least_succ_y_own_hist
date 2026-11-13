library(dplyr)
library(tidyr)
library(stringr)
library(readr)

raw_standings <- readRDS("data/raw_standings.rds")
raw_cups      <- readRDS("data/raw_cups.rds")
raw_market    <- readRDS("data/raw_market.rds")


clean_standings <- raw_standings |>
  select(
    season    = Season,
    league    = League.1,
    tier      = League.2,
    wins      = League.Level,
    draws     = W,
    losses    = D,
    goals_str = L,
    goal_diff = Goals,
    points    = X...,
    rank      = Points,
    manager   = Rank
  ) |>
  #2-digit season format (e.g. "01/02") to exclude pre-1930s ("1928/29")
  filter(str_detect(season, "^\\d{2}/\\d{2}$")) |>
  mutate(
    start_year = as.numeric(paste0("20", str_sub(season, 1, 2)))
  ) |>
  filter(start_year >= 2001, start_year <= 2025) |>
  arrange(start_year) |>
  mutate(
    across(c(wins, draws, losses, goal_diff, points, rank), as.numeric),
    matches         = wins + draws + losses,
    goals_for       = as.numeric(str_extract(goals_str, "^\\d+")),
    goals_against   = as.numeric(str_extract(goals_str, "\\d+$")),
    
    #adjusted for 2006/07 Calciopoli -15 penalty to reflect true on-pitch performance
    penalty_pts     = if_else(season == "06/07", 15, 0),
    points_on_pitch = points + penalty_pts,
    ppg_on_pitch    = round(points_on_pitch / matches, 2),
    points_norm_38  = round(ppg_on_pitch * 38, 1),
    
    is_serie_a      = if_else(league == "Serie A", "Serie A", "Serie B / C2"),
    
    #managerial continuity (consecutive seasons with the same manager)
    manager_changed = if_else(row_number() == 1, TRUE, manager != lag(manager)),
    manager_stint   = cumsum(manager_changed)
  ) |>
  group_by(manager_stint) |>
  mutate(manager_tenure = row_number()) |>
  ungroup() |>
  select(-goals_str, -manager_stint)


clean_cups <- raw_cups |>
  mutate(
    competition = if_else(
      Season == "" & !is.na(Opponent) & Round.achieved == Opponent,
      Round.achieved,
      NA_character_
    )
  ) |>
  fill(competition, .direction = "down") |>
  filter(str_detect(Season, "^\\d{2}/\\d{2}$")) |>
  mutate(
    competition = str_replace_all(competition, "Italy Cup", "Coppa Italia"),
    round_clean = str_trim(Round.achieved)
  ) |>
  distinct(Season, competition, round_clean) |>
  mutate(
    round_pts = case_when(
      str_detect(round_clean, regex("Winner", ignore_case = TRUE))        ~ 5,
      str_detect(round_clean, regex("^Final$", ignore_case = TRUE))       ~ 4,
      str_detect(round_clean, regex("Semi-Final", ignore_case = TRUE))    ~ 3,
      str_detect(round_clean, regex("Quarter-Final", ignore_case = TRUE)) ~ 2,
      str_detect(round_clean, regex("Round of 16", ignore_case = TRUE))   ~ 1,
      TRUE ~ 0 #arbitrary decision to exclude preliminary rounds from the weighted cup score
    ),
    comp_weight = case_when(
      str_detect(competition, regex("Champions League", ignore_case = TRUE))                ~ 2.0,
      str_detect(competition, regex("Europa League|UEFA-Cup|UEFA Cup", ignore_case = TRUE)) ~ 1.5,
      str_detect(competition, regex("Conference League", ignore_case = TRUE))               ~ 1.2,
      TRUE ~ 1.0
    ),
    weighted_cup_pts = round_pts * comp_weight
  ) |>
  group_by(season = Season) |>
  summarise(
    cup_score    = round(sum(weighted_cup_pts, na.rm = TRUE), 2),
    cups_played  = n_distinct(competition),
    best_cup_run = paste(paste0(competition, ": ", round_clean), collapse = " | "),
    .groups      = "drop"
  )


parse_tm_fee <- function(fee_str) {
  fee_clean <- str_to_lower(str_trim(fee_str))
  val <- suppressWarnings(parse_number(fee_clean))
  case_when(
    is.na(val) ~ 0,
    str_detect(fee_clean, "m") ~ val,
    str_detect(fee_clean, "k") ~ val / 1000,
    TRUE ~ 0
  )
}

clean_market <- raw_market |>
  filter(
    Players != "",
    Players != Transfer.sum,
    !str_detect(Players, "^€|^Total")
  ) |>
  mutate(
    type             = str_extract(Season_Box, "^(Arrivals|Departures)"),
    season           = str_extract(Season_Box, "\\d{2}/\\d{2}"),
    fee_m            = parse_tm_fee(Transfer.sum),
    fee_lower        = str_to_lower(Transfer.sum),
    is_incoming_loan = type == "Arrivals" & str_detect(fee_lower, "loan") & !str_detect(fee_lower, "end of loan")
  ) |>
  filter(!is.na(type), !is.na(season)) |>
  group_by(season) |>
  summarise(
    n_arrivals         = sum(type == "Arrivals"),
    n_departures       = sum(type == "Departures"),
    total_turnover     = n_arrivals + n_departures,
    n_loans_in         = sum(is_incoming_loan, na.rm = TRUE),
    loan_arrival_pct   = if_else(n_arrivals > 0, round(n_loans_in / n_arrivals * 100, 1), 0),
    expenditure_m      = round(sum(fee_m[type == "Arrivals"], na.rm = TRUE), 2),
    income_m           = round(sum(fee_m[type == "Departures"], na.rm = TRUE), 2),
    transfer_balance_m = round(income_m - expenditure_m, 2),
    top_sale_fee_m     = round(max(c(0, fee_m[type == "Departures"]), na.rm = TRUE), 2),
    top_sale_player    = if (top_sale_fee_m > 0) {
      Players[type == "Departures"][which.max(fee_m[type == "Departures"])]
    } else {
      "No relevant sales"
    },
    .groups = "drop"
  )


df_fiorentina <- clean_standings |>
  left_join(clean_cups, by = "season") |>
  left_join(clean_market, by = "season") |>
  mutate(
    cup_score    = replace_na(cup_score, 0),
    cups_played  = replace_na(cups_played, 0),
    best_cup_run = replace_na(best_cup_run, "No cup competitions played"),
    prev_transfer_balance_m = lag(transfer_balance_m),
    prev_top_sale_fee_m     = lag(top_sale_fee_m),
    prev_turnover           = lag(total_turnover)
  )

saveRDS(df_fiorentina, "data/fiorentina_25y.rds")
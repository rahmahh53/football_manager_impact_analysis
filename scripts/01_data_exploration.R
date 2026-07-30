# ============================================================
# Purpose:
#   Load the football manager dataset, standardize data types,
#   run quality checks, and save validation summaries.
# ============================================================

library(dplyr)
library(readr)
library(here)

input_path <- here("data", "raw_data", "new_epl_team_match_with_managers_windows.csv")
validation_output_path <- here("outputs", "tables", "validation_summary.csv")
incomplete_output_path <- here("outputs", "tables", "incomplete_matches.csv")
season_output_path <- here("outputs", "tables", "match_seasons.csv")

required_columns <- c("Date", "Season", "Team", "Opponent", "HomeFlag", "GF", "GA", "Manager", "ManagerStart", "ManagerEnd", "MatchNumMgr", "Window1", "Window2", "Window3")

raw_data <- read_csv(input_path, col_types = cols(Date = col_date(format = "%Y-%m-%d"), ManagerStart = col_date(format = "%Y-%m-%d"), ManagerEnd = col_date(format = "%Y-%m-%d"), .default = col_guess()), show_col_types = FALSE)

dim(raw_data)
names(raw_data)
str(raw_data)
summary(raw_data)

raw_data <- raw_data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent), AwayTeam = if_else(HomeFlag == 0, Team, Opponent), MatchID = paste(Season, Date, HomeTeam, AwayTeam, sep = "_"))

missing_columns <- setdiff(required_columns, names(raw_data))

if (length(missing_columns) > 0) {stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))}

missing_summary <- raw_data %>% summarise(across(everything(), ~sum(is.na(.))))
missing_summary

exact_duplicates <- raw_data %>% filter(duplicated(.))
nrow(exact_duplicates)

duplicate_team_matches <- raw_data %>% count(Season, Date, Team, Opponent, HomeFlag, name="row_count") %>% filter(row_count > 1)
duplicate_team_matches

match_check <- raw_data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent), AwayTeam = if_else(HomeFlag == 0, Team, Opponent)) %>% count(Season, Date, HomeTeam, AwayTeam, name = "rows_per_match")
match_check %>% count(rows_per_match)
incomplete_matches <- match_check %>% filter(rows_per_match != 2)
incomplete_matches

match_consistency <- raw_data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent),
                                     AwayTeam = if_else(HomeFlag == 0, Team, Opponent),
                                     HomeGoals = if_else(HomeFlag == 1, GF, GA),
                                     AwayGoals = if_else(HomeFlag == 0, GF, GA)) %>% group_by(Season, Date, HomeTeam, AwayTeam) %>% summarise(row_count = n(), 
                                                                                                                                              distinct_home_scores = n_distinct(HomeGoals),
                                                                                                                                              distinct_away_scores = n_distinct(AwayGoals),
                                                                                                                                              .groups="drop")
inconsistent_matches <- match_consistency %>% filter(row_count != 2 | distinct_home_scores != 1 | distinct_away_scores != 1)
inconsistent_matches

incomplete_detail <- raw_data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent),
                                     AwayTeam = if_else(HomeFlag == 0, Team, Opponent)) %>% group_by(Season, Date, HomeTeam, AwayTeam) %>% filter(n() == 1) %>% ungroup() %>% mutate(present_side = if_else(HomeFlag == 1, "home", "away"),
                                                                                                                                                                                     missing_side = if_else(HomeFlag == 1, "away", "home"),
                                                                                                                                                                                     missing_team = if_else(HomeFlag == 1, AwayTeam, HomeTeam)) %>% select(Season, Date, HomeTeam, AwayTeam, present_team = Team, present_side, missing_team, missing_side, present_manager = Manager)
incomplete_detail %>% count(missing_team, sort=TRUE)
incomplete_detail %>% count(Season, sort=TRUE)
incomplete_detail %>% count(missing_side)

season_match_counts <- raw_data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent),
                                       AwayTeam = if_else(HomeFlag == 0, Team, Opponent)) %>% distinct(Season, Date, HomeTeam, AwayTeam) %>% count(Season, name = "unique_matches")
season_match_counts

raw_data %>% count(Window1, Window2, Window3) %>% arrange(Window1, Window2, Window3)
raw_data %>% summarise(Window1_Window2_overlap = sum(Window1 == 1 & Window2 == 1), Window1_Window3_overlap = sum(Window1 == 1 & Window3 == 1), Window2_Window3_overlap = sum(Window2 == 1 & Window3 == 1))
raw_data %>% filter(Window1 == 1 | Window2 == 1| Window3 == 1) %>% select(Date, Season, Team, Opponent, Manager, ManagerStart, ManagerEnd, MatchNumMgr, Window1, Window2, Window3) %>% arrange(Team, Date) %>% head(30)
window_logic_errors <- raw_data %>% filter(Window1 != as.integer(MatchNumMgr >= 1 & MatchNumMgr <= 3)| Window2 != as.integer(MatchNumMgr >= 4 & MatchNumMgr <= 6)| Window3 != as.integer(MatchNumMgr >= 7 & MatchNumMgr <= 10))
nrow(window_logic_errors)

manager_date_violations <- raw_data %>% filter(Date < ManagerStart | Date > ManagerEnd)
nrow(manager_date_violations)

validation_summary <- tibble(check = c("total_rows", "total_columns", "unique_matches", "missing_values", "exact_duplicates", "duplicate_team_matches", "incomplete_match_pairs", "window_logic_errors", "manager_date_violations"), value = c(nrow(raw_data), ncol(raw_data), n_distinct(raw_data$MatchID), sum(is.na(raw_data)), nrow(exact_duplicates), nrow(duplicate_team_matches), nrow(incomplete_matches), nrow(window_logic_errors), nrow(manager_date_violations)))
write_csv(validation_summary, validation_output_path)
write_csv(incomplete_detail, incomplete_output_path)
write_csv(season_match_counts, season_output_path)

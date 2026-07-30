data <- read.csv("/data/raw_data/epl_team_match_with_managers_windows.csv")

dim(data)
names(data)
str(data)
summary(data)

library(dplyr)
library(lubridate)

data <- data %>% mutate(Date=mdy(Date), ManagerStart = mdy(ManagerStart), ManagerEnd = mdy(ManagerEnd))
str(data[c("Date", "ManagerStart", "ManagerEnd")])
colSums(is.na(data[c("Date", "ManagerStart", "ManagerEnd")]))

missing_summary <- data %>% summarise(across(everything(), ~sum(is.na(.))))
missing_summary

exact_duplicates <- data %>% filter(duplicated(.))
nrow(exact_duplicates)

duplicate_team_matches <- data %>% count(Season, Date, Team, Opponent, HomeFlag, name="row_count") %>% filter(row_count > 1)
duplicate_team_matches

match_check <- data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent), AwayTeam = if_else(HomeFlag == 0, Team, Opponent)) %>% count(Season, Date, HomeTeam, AwayTeam, name = "rows_per_match")
match_check %>% count(rows_per_match)
incomplete_matches <- match_check %>% filter(rows_per_match != 2)
incomplete_matches

match_consistency <- data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent),
                                     AwayTeam = if_else(HomeFlag == 0, Team, Opponent),
                                     HomeGoals = if_else(HomeFlag == 1, GF, GA),
                                     AwayGoals = if_else(HomeFlag == 0, GF, GA)) %>% group_by(Season, Date, HomeTeam, AwayTeam) %>% summarise(row_count = n(), 
                                                                                                                                              distinct_home_scores = n_distinct(HomeGoals),
                                                                                                                                              distinct_away_scores = n_distinct(AwayGoals),
                                                                                                                                              .groups="drop")
inconsistent_matches <- match_consistency %>% filter(row_count != 2 | distinct_home_scores != 1 | distinct_away_scores != 1)
inconsistent_matches

incomplete_detail <- data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent),
                                     AwayTeam = if_else(HomeFlag == 0, Team, Opponent)) %>% group_by(Season, Date, HomeTeam, AwayTeam) %>% filter(n() == 1) %>% ungroup() %>% mutate(present_side = if_else(HomeFlag == 1, "home", "away"),
                                                                                                                                                                                     missing_side = if_else(HomeFlag == 1, "away", "home"),
                                                                                                                                                                                     missing_team = if_else(HomeFlag == 1, AwayTeam, HomeTeam)) %>% select(Season, Date, HomeTeam, AwayTeam, present_team = Team, present_side, missing_team, missing_side, present_manager = Manager)
incomplete_detail %>% count(missing_team, sort=TRUE)
incomplete_detail %>% count(Season, sort=TRUE)
incomplete_detail %>% count(missing_side)

season_match_counts <- data %>% mutate(HomeTeam = if_else(HomeFlag == 1, Team, Opponent),
                                       AwayTeam = if_else(HomeFlag == 0, Team, Opponent)) %>% distinct(Season, Date, HomeTeam, AwayTeam) %>% count(Season, name = "unique_matches")
season_match_counts

data <- data %>% mutate(Points = case_when(GF > GA ~ 3, GF == GA ~ 1, GF < GA ~ 0),
                        GoalDifference = GF - GA, Win = as.integer(GF > GA), Draw = as.integer(GF == GA), Loss = as.integer(GF < GA))
data %>% summarise(minimum_points = min(Points), maximum_points = max(Points), total_missing_points = sum(is.na(Points)))
data %>% count(Points)

data %>% count(Window1, Window2, Window3) %>% arrange(Window1, Window2, Window3)
data %>% summarise(Window1_Window2_overlap = sum(Window1 == 1 & Window2 == 1), Window1_Window3_overlap = sum(Window1 == 1 & Window3 == 1), Window2_Window3_overlap = sum(Window2 == 1 & Window3 == 1))
data %>% filter(Window1 == 1 | Window2 == 1| Window3 == 1) %>% select(Date, Season, Team, Opponent, Manager, ManagerStart, ManagerEnd, MatchNumMgr, Window1, Window2, Window3) %>% arrange(Team, Date) %>% head(30)
window_logic_errors <- data %>% filter(Window1 != as.integer(MatchNumMgr >= 1 & MatchNumMgr <= 3)| Window2 != as.integer(MatchNumMgr >= 4 & MatchNumMgr <= 6)| Window3 != as.integer(MatchNumMgr >= 7 & MatchNumMgr <= 10))
nrow(window_logic_errors)

manager_date_violations <- data %>% filter(Date < ManagerStart | Date > ManagerEnd)
nrow(manager_date_violations)

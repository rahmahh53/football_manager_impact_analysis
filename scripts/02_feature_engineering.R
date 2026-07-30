library(dplyr)
library(readr)
library(here)

source(here("scripts", "01_data_exploration.R"))

analysis_data <- raw_data %>% mutate(Points = case_when(GF > GA ~ 3L, GF == GA ~ 1L, GF < GA ~ 0L), GoalDifference = GF - GA, Win = as.integer(GF > GA), Draw = as.integer(GF == GA), Loss = as.integer(GF < GA))
analysis_data %>% summarise(minimum_points = min(Points), maximum_points = max(Points), total_missing_points = sum(is.na(Points)))

analysis_data <- analysis_data %>%  mutate(
    ManagerPeriod = case_when(
      MatchNumMgr >= 1 & MatchNumMgr <= 3 ~ "Matches 1-3",
      MatchNumMgr >= 4 & MatchNumMgr <= 6 ~ "Matches 4-6",
      MatchNumMgr >= 7 & MatchNumMgr <= 10 ~ "Matches 7-10",
      MatchNumMgr > 10 ~ "Established"))

analysis_data <- analysis_data %>% mutate(ManagerPeriod = factor(ManagerPeriod, levels = c("Established", "Matches 1-3", "Matches 4-6", "Matches 7-10")))
match_row_counts <- analysis_data %>% count(MatchID, name = "rows_per_match")
analysis_data <- analysis_data %>% left_join(match_row_counts, by = "MatchID") %>% mutate(CompleteMatchPair = rows_per_match == 2)

stopifnot(
  all(analysis_data$Points %in% c(0, 1, 3)),
  all(analysis_data$Win %in% c(0, 1)),
  all(analysis_data$Draw %in% c(0, 1)),
  all(analysis_data$Loss %in% c(0, 1)),
  all(analysis_data$GF >= 0),
  all(analysis_data$GA >= 0),
  !any(is.na(analysis_data$ManagerPeriod))
)

output_path <- here("data", "processed_data", "manager_analysis_data.csv")

write_csv(analysis_data, output_path)

message("Processed analysis dataset saved to: ", output_path)

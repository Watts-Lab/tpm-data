library(jsonlite)
library(pls)
library(doParallel)
library(foreach)
library(tidyverse)

seed = 1

# Mark's Data Cleaning
#### -----------------------------------------------------------------------------------
path = "../Wave 1 data"
load_CSVs <- function(pattern) {
  data.frame(path = list.files(path, pattern, recursive = TRUE, full.names = TRUE)) |>
    mutate(data = map(path, \(f)read_csv(f) |> mutate(across(
      matches("(data.score|duplicateCellID)"), as.character # This is dealing with inconsistencies in stages output formats. It works, for now!
    ))),
    .keep = "none") |>
    unnest(data) |>
    distinct()
}

rounds <- load_CSVs("rounds.csv")
stages <- load_CSVs("stages.csv")

raw_score_data <-
  player_conditions |>
  select(data.playerIds, playerId, playerCount, unitsIndex, unitsSeed) |>
  ungroup() |>
  na.omit() |>
  left_join(player_stages) |>
  left_join(stages |> select(stageId = `_id`, displayName, data.stageLength, data.defaultStageLength)) |> 
  left_join(task_instances) |>
  left_join(offline_scoring) |>
  mutate(task = sub(" Round.*", "", displayName)) |>
  filter(!grepl(" Intro", displayName), !grepl(" Practice", displayName)) |>
  mutate(data.score = case_when(!is.na(score) ~ as.character(score),
                                TRUE ~ data.score)) |>
  filter(!is.na(data.score), data.score != "offline") |>
  mutate(playerCount = ordered(playerCount),
         data.score = as.numeric(data.score)) |>
  group_by(task, complexity) |>
  mutate(
    unit = unitsSeed,
    score = 100 * data.score / max(data.score),
    score = if_else(score < 0, 0, score),
    raw_duration_min = data.stageLength / 60000,
    default_duration_min = first(data.defaultStageLength) / 60000,
    speed = (1 + data.defaultStageLength - if_else(data.stageLength > data.defaultStageLength, data.defaultStageLength, data.stageLength)) / (1 + data.defaultStageLength) * 100,
    speed = if_else(speed < 0, 0, speed),
    efficiency = score * speed, 
  ) |>
  select(unit,
         playerCount,
         task,
         complexity,
         score,
         speed,
         efficiency,
         raw_duration_min,
         default_duration_min,
         stageId) |>
  unique() |>
  na.omit()

#### -----------------------------------------------------------------------------------
# Emily's Data Cleaning

stages %>% write_csv('multi_task_stages.csv')

# Rounds, Chats, and DV's
rounds_with_chat <- rounds %>% filter(!is.na(data.A))
rounds_with_chat <- rounds_with_chat[,colSums(is.na(rounds_with_chat))<nrow(rounds_with_chat)]
# Merge with outcome data
merged_data <- rounds_with_chat %>%
  mutate(stageIds = strsplit(stageIds, ",")) %>%
  tidyr::unnest(cols = stageIds) %>%
  left_join(raw_score_data, by = c("stageIds" = "stageId")) %>%
  filter(!is.na(score)) %>%
  select(-c(data.bonusRate, data.assigned, data.similarityList))
merged_data %>% write_csv("multi_task_data_with_dv_by_rounds.csv")

# Individual Participant Information
panel_data <- read_csv("https://raw.githubusercontent.com/Watts-Lab/panel/main/clean_data/individuals.csv?token=GHSAT0AAAAAACLLXRSYYCKNMMARLSLLBKZ4ZNUF75A")
results_by_user_with_panel_info <- results_by_user %>%
  left_join(panel_data, by = c("id" = "WorkerId"))
results_by_user_with_panel_info %>% write_csv('results_by_user_detailed.csv')


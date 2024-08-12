library(soles)
library(dplyr)
library(DBI)
library(fst)
library(tidyr)
library(readr)
library(jsonlite)
library(janitor)


# Bring in llm predictions and tidy

predictions_df <- read_csv("llm_annotations_df_all.csv") %>%
  clean_names()

rownames(predictions_df) <- NULL

# Interventions tables
interventions_df_small <- predictions_df %>%
  select(uid, intervention) %>%
  filter(!uid %in% intervention_df_human$uid)

interventions_df_bubble <- interventions_df_small %>%
  separate_rows(intervention, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

interventions_df <- rbind(interventions_df_bubble, intervention_df_human)
dataframes_for_app[["interventions_df"]] <- interventions_df
interventions_df_small <- rbind(interventions_df_small, intervention_df_human_small)
dataframes_for_app[["interventions_df_small"]] <- interventions_df_small

# Intervention providers tables
intervention_provider_df_small <- predictions_df %>%
  select(uid, intervention_provider)  %>%
  filter(!uid %in% intervention_provider_df_human$uid)

intervention_provider_df_bubble <- intervention_provider_df_small %>%
  separate_rows(intervention_provider, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

intervention_provider_df <- rbind(intervention_provider_df_bubble, intervention_provider_df_human)
dataframes_for_app[["intervention_provider_df"]] <- intervention_provider_df
interventions_df_small <- rbind(intervention_provider_df_small, intervention_provider_df_human_small)
dataframes_for_app[["intervention_provider_df_small"]] <- intervention_provider_df_small

# Discipline tables
discipline_df_small <- predictions_df %>%
  select(uid, discipline) %>%
  filter(!uid %in% discipline_df_human$uid)

discipline_df_bubble <- discipline_df_small %>%
  separate_rows(discipline, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

discipline_df <- rbind(discipline_df_bubble, discipline_df_human)
dataframes_for_app[["discipline_df"]] <- discipline_df
discipline_df_small <- rbind(discipline_df_small, discipline_df_human_small)
dataframes_for_app[["discipline_df_small"]] <- discipline_df_small

# Target population tables
target_population_df_small <- predictions_df %>%
  select(uid, target_population) %>%
  filter(!uid %in% target_population_df_human$uid)

target_population_df_bubble <- target_population_df_small %>%
  separate_rows(target_population, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

target_population_df <- rbind(target_population_df_bubble, target_population_df_human)
dataframes_for_app[["target_population_df"]] <- target_population_df
target_population_df_small <- rbind(target_population_df_small, target_population_df_human_small)
dataframes_for_app[["target_population_df_small"]] <- target_population_df_small

# Research stage tables
research_stage_df_small <- predictions_df %>%
  select(uid, research_stage) %>%
  filter(!uid %in% research_stage_df_human$uid)

research_stage_df_bubble <- research_stage_df_small %>%
  separate_rows(research_stage, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

research_stage_df <- rbind(research_stage_df_bubble, research_stage_df_human)
dataframes_for_app[["research_stage_df"]] <- research_stage_df
research_stage_df_small <- rbind(research_stage_df_small, research_stage_df_human_small)
dataframes_for_app[["research_stage_df_small"]] <- research_stage_df_small

# Outcome measures tables
outcome_measures_df_small <- predictions_df %>%
  select(uid, outcome_measures) %>%
  filter(!uid %in% outcome_measures_df_human$uid)

outcome_measures_df_bubble <- outcome_measures_df_small %>%
  separate_rows(outcome_measures, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

outcome_measures_df <- rbind(outcome_measures_df_bubble, outcome_measures_df_human)
dataframes_for_app[["outcome_measures_df"]] <- outcome_measures_df
outcome_measures_df_small <- rbind(outcome_measures_df_small, outcome_measures_df_human_small)
dataframes_for_app[["outcome_measures_df_small"]] <- outcome_measures_df_small

# Target pop location tables
target_pop_location_df_small <- predictions_df %>%
  select(uid, target_population_location)  %>%
  rename(location_pop=target_population_location) %>%
  filter(!uid %in% location_pop_df_human$uid)

target_pop_location_df_bubble <- target_pop_location_df_small %>%
  separate_rows(location_pop, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1")

target_pop_location_df <- rbind(target_pop_location_df_bubble,location_pop_df_human)
dataframes_for_app[["target_pop_location_df"]] <- target_pop_location_df
research_stage_df_small <- rbind(target_pop_location_df_small, location_pop_df_human_small)
dataframes_for_app[["target_pop_location_df_small"]] <- target_pop_location_df_small




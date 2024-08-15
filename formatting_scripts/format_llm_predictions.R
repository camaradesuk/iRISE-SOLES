library(soles)
library(dplyr)
library(DBI)
library(fst)
library(tidyr)
library(readr)
library(jsonlite)
library(janitor)


# Bring in llm predictions and tidy
predictions_df <- read_csv("llm_annotations_df_full_modified.csv") %>%
  clean_names()

predictions_df$outcome_measures <- gsub("Pre-registration", "Preregistration", predictions_df$outcome_measures)
predictions_df$intervention <- gsub("Pre-registration", "Preregistration", predictions_df$intervention)

rownames(predictions_df) <- NULL

all_annotations_small <- predictions_df

dataframes_for_app[["all_annotations_small"]] <- all_annotations_small


# Interventions tables
interventions_df_small <- predictions_df %>%
  select(uid, intervention) %>%
  filter(!uid %in% intervention_df_human$uid)

interventions_df_bubble <- interventions_df_small %>%
  separate_rows(intervention, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(intervention_df_human) %>% 
  mutate(intervention = trimws(intervention))
  

interventions_df <- interventions_df_bubble %>%
  rename(name = intervention)
dataframes_for_app[["interventions_df"]] <- interventions_df


# Intervention providers tables
intervention_provider_df_small <- predictions_df %>%
  select(uid, intervention_provider)  %>%
  filter(!uid %in% intervention_provider_df_human$uid)

intervention_provider_df_bubble <- intervention_provider_df_small %>%
  separate_rows(intervention_provider, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(intervention_provider_df_human) %>% 
  mutate(intervention_provider = trimws(intervention_provider))

intervention_provider_df <- intervention_provider_df_bubble %>%
  rename(name = intervention_provider)
dataframes_for_app[["intervention_provider_df"]] <- intervention_provider_df


# Discipline tables
discipline_df_small <- predictions_df %>%
  select(uid, discipline) %>%
  filter(!uid %in% discipline_df_human$uid)

discipline_df_bubble <- discipline_df_small %>%
  separate_rows(discipline, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(discipline_df_human) %>% 
  mutate(discipline = trimws(discipline)) 


discipline_df <- discipline_df_bubble %>%
  rename(name = discipline)

dataframes_for_app[["discipline_df"]] <- discipline_df


# Target population tables
target_population_df_small <- predictions_df %>%
  select(uid, target_population) %>%
  filter(!uid %in% target_population_df_human$uid)

target_population_df_bubble <- target_population_df_small %>%
  separate_rows(target_population, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(target_population_df_human) %>% 
  mutate(target_population = trimws(target_population))

target_population_df <- target_population_df_bubble %>%
  rename(name = target_population)

dataframes_for_app[["target_population_df"]] <- target_population_df


# Research stage tables
research_stage_df_small <- predictions_df %>%
  select(uid, research_stage) %>%
  filter(!uid %in% research_stage_df_human$uid)

research_stage_df_bubble <- research_stage_df_small %>%
  separate_rows(research_stage, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(research_stage_df_human) %>% 
  mutate(research_stage = trimws(research_stage))

research_stage_df <- research_stage_df_bubble %>%
  rename(name = research_stage)

dataframes_for_app[["research_stage_df"]] <- research_stage_df


# Outcome measures tables
outcome_measures_df_small <- predictions_df %>%
  select(uid, outcome_measures) %>%
  filter(!uid %in% outcome_measures_df_human$uid)

outcome_measures_df_bubble <- outcome_measures_df_small %>%
  separate_rows(outcome_measures, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(outcome_measures_df_human) %>% 
  mutate(outcome_measures = trimws(outcome_measures))

outcome_measures_df <- outcome_measures_df_bubble %>%
  rename(name = outcome_measures)

dataframes_for_app[["outcome_measures_df"]] <- outcome_measures_df


# Target pop location tables
target_pop_location_df_small <- predictions_df %>%
  select(uid, target_population_location)  %>%
  rename(location_pop=target_population_location) %>%
  filter(!uid %in% location_pop_df_human$uid)

target_pop_location_df_bubble <- target_pop_location_df_small %>%
  separate_rows(location_pop, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>%
  rbind(location_pop_df_human) %>% 
  mutate(location_pop = trimws(location_pop))

target_pop_location_df <- target_pop_location_df_bubble %>%
  rename(name = location_pop)
dataframes_for_app[["target_pop_location_df"]] <- target_pop_location_df

# Combine all human and LLM annotations
all_annotations_small <- predictions_df %>%
  filter(!uid %in% all_human_annotations_small$uid) %>%
  rbind(all_human_annotations_small)

dataframes_for_app[["all_annotations_small"]] <- all_annotations_small

all_annotations <- included_small %>%
  select(uid) %>%
  inner_join(interventions_df_bubble, by = "uid") %>%
  inner_join(intervention_provider_df_bubble, by = "uid") %>%
  inner_join(target_population_df_bubble, by ="uid") %>%
  inner_join(target_pop_location_df_bubble, by ="uid") %>%
  inner_join(discipline_df_bubble, by ="uid") %>%
  inner_join(research_stage_df_bubble, by = "uid") %>%
  inner_join(outcome_measures_df_bubble, by = "uid") %>%
  select(-starts_with("method.")) %>%
  mutate(across(where(is.character), str_trim))

dataframes_for_app[["all_annotations"]] <- all_annotations


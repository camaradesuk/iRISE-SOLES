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
  clean_names() %>% 
  mutate(discipline = ifelse(discipline == "Education", "Social sciences", discipline))

rownames(predictions_df) <- NULL

# Interventions tables
interventions_df_small <- predictions_df %>%
  select(uid, intervention) %>%
  filter(!uid %in% intervention_df_human$uid)

interventions_df_bubble <- interventions_df_small %>%
  separate_rows(intervention, sep = ";\\s+") %>%
  mutate(method = "gpt-4o-3s-v1") %>% 
  rbind(intervention_df_human)

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
  rbind(intervention_provider_df_human)

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
  rbind(discipline_df_human)

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
  rbind(target_population_df_human)

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
  rbind(research_stage_df_human)

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
  rbind(outcome_measures_df_human)

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
  rbind(location_pop_df_human)

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




# Create dataframes with only suggested categories
intervention_categories <- c("TRIPOD guidelines", "SRWR guidelines", "COREQ guidelines", "PRISMA guidelines", "STROBE guidelines", "SPIRIT guidelines", "AGREE guidelines", "SRQR guidelines", "ARRIVE guidelines", "ICMJE guidelines", "SQUIRE guidelines", "CHEERS guidelines", "CARE guidelines", "STARD guidelines", "CONSORT guidelines", "TOP guidelines", "MDAR guidelines", "Other reporting guidelines, checklists, or standards", "Data sharing policy/guideline", "Materials sharing policy/guideline", "Code sharing policy/guideline", "Reporting quality checks / feedback", "Computational reproducibility checks / feedback", "Code quality checks / feedback", "Data quality checks / feedback", "Other quality checks / feedback", "Trial registration", "Analysis plans", "Pre-registration", "Protocol registration", "Registered reports", "Other peer review process", "Workflow standardisation", "Data access policies / agreements", "Open science plans", "Citation standards", "Open access publication", "Online sharing platform", "Open Science Framework", "Zenodo", "GitHub", "Centralised sharing platform", "Open science infrastructure", "Open code badges", "Open materials badges", "Open data badges", "Pre-registration badges", "Other open science badges", "Electronic lab notebooks", "Version control system", "Documentation system", "Experimental design", "Statistical method", "CReDiT taxonomy", "ORCID iD", "Data management training", "Systematic review training", "Pre-registration training", "Statistical training", "Reproducible code/analysis training", "Training community", "Mentoring / role models", "Results-free peer review", "Open peer review", "Unspecified")
provider_categories <- c("Government", "Funder", "Publisher / journal", "Institution", "Students", "Researchers / Researcher Collaboration", "Learned societies", "Research support staff", "Organisation", "Unspecified")
outcome_categories <- c("Type-I error reduction", "Type-II error reduction", "Effect size estimation", "Transparency of funding", "Transparency of interests", "Transparency of contributions", "Data availability and re-use", "Materials availability and re-use", "Code / analysis availability and re-use", "Reporting quality", "Transparency of evaluation", "Reporting bias", "Publication bias", "Computational reproducibility", "Unspecified")
target_categories <- c("Government", "Funder", "Publisher / journal", "Institution", "Students", "Researchers / Researcher Collaboration", "Learned societies", "Research support staff", "Organisation", "Unspecified")

all_annotations_restricted <- all_annotations %>%
  filter(intervention %in% intervention_categories,
         outcome_measures %in% outcome_categories,
         intervention_provider %in% provider_categories,
         target_population %in% target_categories) %>% 
  distinct()

dataframes_for_app[["all_annotations_restricted"]] <- all_annotations_restricted


all_annotations_interventions_restricted <- all_annotations %>%
  select(uid, intervention) %>% 
  filter(intervention %in% intervention_categories) %>% 
  distinct() %>% 
  group_by(uid) %>%
  summarise(
    intervention = paste(intervention, collapse = "; "),
    .groups = 'drop'
  )

all_annotations_intervention_provider_restricted <- all_annotations %>%
  select(uid, intervention_provider) %>% 
  filter(intervention_provider %in% provider_categories) %>% 
  distinct() %>% 
  group_by(uid) %>%
  summarise(
    intervention_provider = paste(intervention_provider, collapse = "; "),
    .groups = 'drop'
  )

all_annotations_outcomes_restricted <- all_annotations %>%
  select(uid, outcome_measures) %>% 
  filter(outcome_measures %in% outcome_categories) %>% 
  distinct() %>% 
  group_by(uid) %>%
  summarise(
    outcome_measures = paste(outcome_measures, collapse = "; "),
    .groups = 'drop'
  )

all_annotations_target_population_restricted <- all_annotations %>%
  select(uid, target_population) %>% 
  filter(target_population %in% target_categories) %>% 
  distinct() %>% 
  group_by(uid) %>%
  summarise(
    target_population = paste(target_population, collapse = "; "),
    .groups = 'drop'
  )

all_annotations_small_restricted <- all_annotations_small %>% 
  select(uid, discipline, research_stage, target_population_location) %>% 
  left_join(all_annotations_interventions_restricted, by= "uid") %>% 
  left_join(all_annotations_outcomes_restricted, by = "uid") %>% 
  left_join(all_annotations_intervention_provider_restricted, by = "uid") %>% 
  left_join(all_annotations_target_population_restricted, by = "uid")

dataframes_for_app[["all_annotations_small_restricted"]] <- all_annotations_small_restricted



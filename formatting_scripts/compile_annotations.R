library(tidyr)
library(dplyr)
library(soles)
library(janitor)
library(DBI)


# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))


annotated_data <- read.csv("annotation_data/Annotation_data_-_2024_08_05_-_Long_format_-_1059b82e-b4bb-42b2-af3d-78888c23473d_-_Investigators_Unblinded.csv") %>%
  clean_names() %>%
  filter(question_id %in% c("53f9bf22-4f0d-40c5-85c4-4bb227b782e3", "ce632263-f636-436a-97e8-e525f4019c48", "3a33700f-fcaa-467c-af7b-af20206bf738", "bf3d790d-8868-4745-9e42-0b63b93b436b",
                            "883eafba-d784-44ce-b0e5-7e99db451980", "5f62cc81-eda8-4863-8a1b-88ddfd3a1311", "261f8697-6539-4a06-adc9-9d0c60cf8c41", "ebb70d23-a0c8-443c-9a0f-2d9bb94f2e20",
                            "146fd515-dfb3-4410-b981-ceacf99bb43e", "c9407615-5901-4a14-952d-d88d8a895b09", "f8fe8bb2-5abc-4107-87ca-f85470b50593", "b9495f6b-60a8-40c9-9214-cdd31296f175",
                            "60119758-e3e3-4443-9d2a-385bf20a25f9", "baa4c492-9980-4876-8bd1-d25844c139bb", "6de3827e-88aa-4189-ae6f-cd97b001d1f5", "e3a34dcf-3a5a-4b2a-bd4b-7a0a369bdccb",
                            "4ef35ea9-c3f8-47e8-9295-1eed2dec6b59", "89d2afbf-b16e-44ef-8b29-9f06da4fd7f5", "f0ae17a8-f6f8-4ea2-bb28-1e78d6a492f5", "cad6c230-df70-490b-8b50-0fa4615a0a9f",
                            "b7c6ce9a-3ebd-4060-b396-d86a7d4e9b51", "acfbccc0-af40-4eef-a219-ef5991f67fda", "e2923c42-e304-4813-9b92-e501eb3f6b2d", "916076be-e5c9-4ac7-bd58-84c22a767874"))


annotated_studies_small <- annotated_data %>%
  select(study_id, title, abstract, custom_id, investigator_id, question, answer) %>%
  pivot_wider(names_from = question, values_from = answer) %>%
  clean_names() %>%
  filter(are_you_reconciling_these_annotations == "True") %>%
  filter(should_this_article_be_included == "True") %>%
  rename(intervention = intervention_evaluated,
         discipline = target_discipline,
         population_evaluated = population_s_evaluated,
         research_stage = stage_of_research,
         outcome_measures = relevant_outcomes,
         location_pop = target_population_location,
         edi_impications_discussed = do_the_authors_discuss_the_edi_implications_of_the_intervention,
         edi_impact_measured = do_authors_measure_the_impact_of_the_intervention_on_specific_aspects_of_edi) %>%
  mutate(do_authors_measure_the_costs_of_applying_this_intervention = ifelse(is.na(do_authors_measure_the_costs_of_applying_this_intervention), "", do_authors_measure_the_costs_of_applying_this_intervention)) %>%
  mutate(do_authors_measure_the_costs_of_applying_this_intervention_2 = ifelse(is.na(do_authors_measure_the_costs_of_applying_this_intervention_2), "", do_authors_measure_the_costs_of_applying_this_intervention_2)) %>%
  mutate(cost_of_intervention_measured = case_when(
    do_authors_measure_the_costs_of_applying_this_intervention != "" ~ do_authors_measure_the_costs_of_applying_this_intervention,
    do_authors_measure_the_costs_of_applying_this_intervention_2 != "" ~ do_authors_measure_the_costs_of_applying_this_intervention_2,
    TRUE ~ "False"
  )) %>%
  mutate(target_population = ifelse(is.na(target_population), population_evaluated, target_population)) %>%
  select(uid = custom_id, abstract, evidence_type = type_of_evidence, intervention, intervention_provider, discipline, target_population, research_stage, outcome_measures, location_pop, edi_impications_discussed, edi_impact_measured, cost_of_intervention_measured) %>%
  mutate(discipline = ifelse(discipline == "Other (leave a comment)", "Unspecified", discipline)) %>%
  mutate(across(everything(), ~if_else(is.na(.), "Unspecified", .)))


annotated_studies_small$outcome_measures <- gsub("Effect size estimation", "Type I/II error reduction",annotated_studies_small$outcome_measures)
annotated_studies_small$outcome_measures <- gsub("Type-I error reduction", "Type I/II error reduction",annotated_studies_small$outcome_measures)
annotated_studies_small$outcome_measures <- gsub("Type-II error reduction", "Type I/II error reduction",annotated_studies_small$outcome_measures)
annotated_studies_small$outcome_measures <- gsub("Type I/II error reduction; Type I/II error reduction", "Type I/II error reduction",annotated_studies_small$outcome_measures)
annotated_studies_small$outcome_measures <- gsub("Type I/II error reduction; Type I/II error reduction", "Type I/II error reduction",annotated_studies_small$outcome_measures)
annotated_studies_small$outcome_measures <- gsub("Pre-registration", "Preregistration", annotated_studies_small$outcome_measures)
annotated_studies_small$intervention <- gsub("Pre-registration", "Preregistration", annotated_studies_small$intervention)

annotated_studies <- annotated_studies_small %>%
  separate_rows(research_stage, sep = ";") %>%
  separate_rows(discipline, sep = ";") %>%
  separate_rows(intervention, sep = ";") %>%
  separate_rows(intervention_provider, sep = ";") %>%
  separate_rows(outcome_measures, sep = ";") %>%
  separate_rows(location_pop, sep = ";") %>%
  separate_rows(target_population, sep = ";") %>%
  select(uid, intervention, intervention_provider, target_population, location_pop, discipline, research_stage, outcome_measures)

# Interventions tables
intervention_df_human_small <- annotated_studies_small %>%
  select(uid, intervention)

intervention_df_human <- intervention_df_human_small %>%
  separate_rows(intervention, sep = ";") %>%
  mutate(method = "human")

# Intervention Provider tables
intervention_provider_df_human_small <- annotated_studies_small %>%
  select(uid, intervention_provider)

intervention_provider_df_human <- intervention_provider_df_human_small %>%
  separate_rows(intervention_provider, sep = ";") %>%
  mutate(method = "human")

# Target Population tables
target_population_df_human_small <- annotated_studies_small %>%
  select(uid, target_population)

target_population_df_human <- target_population_df_human_small %>%
  separate_rows(target_population, sep = ";") %>%
  mutate(method = "human")

# Discipline tables
discipline_df_human_small <- annotated_studies_small %>%
  select(uid, discipline)

discipline_df_human <- discipline_df_human_small %>%
  separate_rows(discipline, sep = ";") %>%
  mutate(method = "human")

# Research Stage tables
research_stage_df_human_small <- annotated_studies_small %>%
  select(uid, research_stage)

research_stage_df_human <- research_stage_df_human_small %>%
  separate_rows(research_stage, sep = ";") %>%
  mutate(method = "human")

# Location Population tables
location_pop_df_human_small <- annotated_studies_small %>%
  select(uid, location_pop)

location_pop_df_human <- location_pop_df_human_small %>%
  separate_rows(location_pop, sep = ";") %>%
  mutate(method = "human")

# Outcome Measures tables
outcome_measures_df_human_small <- annotated_studies_small %>%
  select(uid, outcome_measures)

outcome_measures_df_human <- outcome_measures_df_human_small %>%
  separate_rows(outcome_measures, sep = ";") %>%
  mutate(method = "human")

# All human annotations
all_human_annotations_small <- annotated_studies_small %>%
  select(uid, intervention, intervention_provider, target_population, target_population_location = location_pop,
         discipline, research_stage, outcome_measures)

all_human_annotations <- annotated_studies %>%
  select(uid, intervention, intervention_provider, target_population, target_population_location = location_pop,
         discipline, research_stage, outcome_measures)


#write.csv(reconciled_studies, "iRISE_syrf_annotations_300524.csv")

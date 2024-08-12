library(tidyr)
library(dplyr)
library(soles)
library(janitor)
library(DBI)

setwd("/home/scsmith/projects/iRISE_new/")

# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

study_class <- dbReadTable(con, "study_classification")

# annotated_data <- read.csv("Annotation_data_-_2024_07_17_-_Long_format_-_1059b82e-b4bb-42b2-af3d-78888c23473d_-_Investigators_Unblinded.csv") %>% 
#   clean_names() %>% 
#   filter(question_id %in% c("53f9bf22-4f0d-40c5-85c4-4bb227b782e3", "ce632263-f636-436a-97e8-e525f4019c48", "3a33700f-fcaa-467c-af7b-af20206bf738", "bf3d790d-8868-4745-9e42-0b63b93b436b",
#                             "883eafba-d784-44ce-b0e5-7e99db451980", "5f62cc81-eda8-4863-8a1b-88ddfd3a1311", "261f8697-6539-4a06-adc9-9d0c60cf8c41", "ebb70d23-a0c8-443c-9a0f-2d9bb94f2e20",
#                             "146fd515-dfb3-4410-b981-ceacf99bb43e", "c9407615-5901-4a14-952d-d88d8a895b09", "f8fe8bb2-5abc-4107-87ca-f85470b50593", "b9495f6b-60a8-40c9-9214-cdd31296f175",
#                             "60119758-e3e3-4443-9d2a-385bf20a25f9", "baa4c492-9980-4876-8bd1-d25844c139bb", "6de3827e-88aa-4189-ae6f-cd97b001d1f5", "e3a34dcf-3a5a-4b2a-bd4b-7a0a369bdccb",
#                             "4ef35ea9-c3f8-47e8-9295-1eed2dec6b59", "89d2afbf-b16e-44ef-8b29-9f06da4fd7f5", "f0ae17a8-f6f8-4ea2-bb28-1e78d6a492f5", "cad6c230-df70-490b-8b50-0fa4615a0a9f",
#                             "b7c6ce9a-3ebd-4060-b396-d86a7d4e9b51", "acfbccc0-af40-4eef-a219-ef5991f67fda", "e2923c42-e304-4813-9b92-e501eb3f6b2d", "916076be-e5c9-4ac7-bd58-84c22a767874"))

annotated_data <- read.csv("annotation_data/Annotation_data_-_2024_07_30_-_Long_format_-_1059b82e-b4bb-42b2-af3d-78888c23473d_-_Investigators_Unblinded.csv") %>% 
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
  #filter(reconciled_annotations == "True") %>% 
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
  #mutate(intervention_provider = ifelse(is.na(intervention_provider), og_intervention_provider, intervention_provider)) %>% 
  mutate(do_authors_measure_the_costs_of_applying_this_intervention = ifelse(is.na(do_authors_measure_the_costs_of_applying_this_intervention), "", do_authors_measure_the_costs_of_applying_this_intervention)) %>% 
  mutate(do_authors_measure_the_costs_of_applying_this_intervention_2 = ifelse(is.na(do_authors_measure_the_costs_of_applying_this_intervention_2), "", do_authors_measure_the_costs_of_applying_this_intervention_2)) %>% 
  mutate(cost_of_intervention_measured = case_when(
    do_authors_measure_the_costs_of_applying_this_intervention != "" ~ do_authors_measure_the_costs_of_applying_this_intervention,
    do_authors_measure_the_costs_of_applying_this_intervention_2 != "" ~ do_authors_measure_the_costs_of_applying_this_intervention_2,
    TRUE ~ "False"
  )) %>% 
  mutate(target_population = ifelse(is.na(target_population), population_evaluated, target_population)) %>% 
  mutate(type_of_evidence = ifelse(type_of_evidence == "Controlled observational study measuring the impact of an intervention ", "Controlled observational study measuring the impact of an intervention", type_of_evidence)) %>% 
  select(uid = custom_id, title, abstract, evidence_type = type_of_evidence, intervention, intervention_provider, discipline, target_pop = target_population, research_stage, outcomes = outcome_measures, location_pop, edi_impications_discussed, edi_impact_measured, cost_of_intervention_measured) %>% 
  mutate(across(everything(), ~if_else(is.na(.), "Unspecified", .))) %>% 
  mutate(tiab = paste(title, abstract, sep = ". ")) %>% 
  filter(!uid == "medline-17156458")

reconciled_35 <- read.csv("reconciled_35.csv") 

updated_reconciled <- annotated_studies_small %>% 
  filter(uid %in% reconciled_35$uid) %>% 
  filter(!uid == "medline-35653153") %>% 
  mutate(evidence_type_aim = ifelse(evidence_type == "Other relevant evidence", "Aim 2", "Aim 1"))
  
  

write.csv(updated_reconciled, "iRISE_updated_reconcile_300724.csv")



## Create training data for fine-tune -----

# Format the initial 10 studies
training_10 <- read.csv("training_10_final_tiab.csv") %>% 
  mutate(evidence_type = ifelse(evidence_type == "Other relevant evidence ", "Other relevant evidence", evidence_type)) %>% 
  mutate(evidence_type_aim = ifelse(evidence_type == "Other relevant evidence", "Aim 2", "Aim 1")) %>% 
  select(uid, title, abstract, tiab, evidence_type_aim)

# Use the annotated studies from the set of 35 for initial test/development
new_annotations <- annotated_studies_small %>%
  filter(uid %in% reconciled_35$uid) %>%
  mutate(evidence_type_aim = ifelse(evidence_type == "Other relevant evidence", "Aim 2", "Aim 1")) %>% 
  select(uid, title, abstract, tiab, evidence_type_aim)

# Bind these 2 sets of studies for training
older_training_data <- rbind(training_10, new_annotations) %>% 
  distinct()

# Get an extra 10 studies to add for training, giving us 54 in total
remaining_train <- annotated_studies_small %>% 
  filter(!uid %in% training_10$uid) %>% 
  filter(!uid %in% new_annotations$uid) %>% 
  sample_n(10) %>% 
  mutate(evidence_type_aim = ifelse(evidence_type == "Other relevant evidence", "Aim 2", "Aim 1")) %>% 
  select(uid, title, abstract, tiab, evidence_type_aim) %>% 
  rbind(older_training_data) %>% 
  distinct()

study_type_remaining_train <- annotated_studies_small %>% 
  filter(uid %in% remaining_train$uid)

# Evidence type proportions for training set
train_count <- remaining_train %>% 
  count(evidence_type_aim)

# Take all of the studies which are left over for testing, totalling 50
remaining_test <- annotated_studies_small %>% 
  filter(!uid %in% remaining_train$uid) %>% 
  mutate(evidence_type_aim = ifelse(evidence_type == "Other relevant evidence", "Aim 2", "Aim 1")) %>% 
  mutate(uid = tolower(uid))

study_type_remaining_test <- remaining_test %>% 
  mutate(study_type = ifelse(evidence_type == "Controlled observational study measuring the impact of an intervention", "Observational study", evidence_type)) %>% 
  mutate(study_type = ifelse(evidence_type == "Controlled experiment measuring the impact of an intervention", "Experiment", evidence_type))

  
# Evidence type proportions for test set
test_count <- remaining_test %>% 
  count(evidence_type_aim)

# write.csv(remaining_train, "training_for_evidence_type_ft.csv")
# write.csv(remaining_test, "test_set_for_evidence_type.csv")


# Retrieve the test set pdfs for method extractions -----
test_papers <- dbReadTable(con, "unique_citations") %>% 
  filter(uid %in% remaining_test$uid) %>% 
  select(uid, doi, title)

# Bring in path, rename files based on uid
full_texts <- dbReadTable(con, "full_texts") %>% 
  filter(doi %in% test_papers$doi) %>% 
  left_join(test_papers, by = "doi") %>% 
  filter(status == "found") %>% 
  mutate(
    file_extension = tools::file_ext(path),
    new_uid = paste(uid, file_extension, sep = ".")) %>% 
  mutate(path = stringr::str_replace_all(path, "full_texts/", ""))

file.copy(from  = full_texts$path, to = "test_pdfs/")

# Function for renaming files
rename_files <- function(old_name, new_name, directory) {
  old_filepath <- file.path(directory, old_name)
  new_filepath <- file.path(directory, new_name)
  
  if (file.exists(old_filepath)) {
    if (file.rename(old_filepath, new_filepath)) {
      message("Successfully renamed ", old_name, " to ", new_name)
    } else {
      message("Failed to rename ", old_name)
    }
  } else {
    message("File does not exist: ", old_filepath)
  }
}

# Apply the renaming function to each row in the DataFrame
apply(full_texts, 1, function(x) rename_files(x['path'], x['new_uid'], "test_pdfs"))

pdfs_to_be_dl <- test_papers %>% 
  filter(!uid %in% full_texts$uid)


# incorrect_evidence_type <- annotated_studies_small %>% 
#   filter(uid %in% c("medline-32639955", "scopus-2-s2.0-85098325783", "medline-31322285", "medline-31038358", "scopus-2-s2.0-85042159426", "scopus-2-s2.0-85049801491", "medline-26131374"))
# 
# write.csv(incorrect_evidence_type, "incorrect_evidence_type.csv")

latest_incorrect_evidence_type <- annotated_studies_small %>% 
  filter(uid %in% c("scopus-2-s2.0-85042159426", "medline-36884270", "medline-31038358", "medline-31322285"))

incorrect_predictions_test <- annotated_studies_small %>% 
  filter(uid %in% c("medline-37463773", "medline-29082866", "medline-35852964", "scopus-2-s2.0-85097053825", "scopus-2-s2.0-85140462031"))
write.csv(incorrect_predictions_test, "incorrect_evidence_type_test.csv")


# # Interventions tables
# intervention_df_small <- annotated_studies_small %>% 
#   select(uid, intervention) 
# 
# intervention_df <- intervention_df_small %>% 
#   separate_rows(intervention, sep = ";")



# annotated_studies <- annotated_studies_small %>% 
#   separate_rows(intervention, sep = ";") %>% 
#   separate_rows(intervention_provider, sep = ";") %>% 
#   separate_rows(discipline, sep = ";") %>% 
#   separate_rows(target_population, sep = ";") %>% 
#   separate_rows(research_stage, sep = ";") %>% 
#   separate_rows(outcome_measures, sep = ";") %>% 
#   separate_rows(target_pop_location, sep = ";") %>% 
#   separate_rows(type_of_evidence, sep = ";") %>% 
#   filter(!discipline == "Other (leave a comment)") %>% 
#   select(uid, intervention, intervention_provider, population_evaluated, discipline, target_population, research_stage, outcome_measures, target_pop_location, type_of_evidence, edi_impications_discussed, edi_impact_measured, cost_of_intervention_measured)
# 
# 


# possible_interventions <- annotated_studies_small %>% 
#   distinct(intervention) %>% 
#   separate_rows(intervention, sep = ";\\s*") %>% 
#   distinct() %>% 
#   mutate(intervention = ifelse(intervention == "ICMJE recommendations", "ICMJE guidelines", intervention)) %>% 
#   pull(intervention)
# 
# interventions_1 <- c(
#   "Protocol registration", "Trial registration",
#   "Other reporting guidelines, checklists, or standards", "Data sharing policy/guideline",
#   "Statistical method", "CONSORT guidelines",
#   "Workflow standardisation", "Open data badges",
#   "Other peer review process", "PROCESS guidelines",
#   "Open science infrastructure", "CONSORT-AI guidelines",
#   "Code sharing policy/guideline", "Open access publication",
#   "Pre-registration badges", "Open materials badges",
#   "ICMJE guidelines", "Data sharing statements",
#   "Open science plans", "PRISMA guidelines",
#   "GoodReports.org", "Reporting quality checks / feedback",
#   "Penelope.ai", "Code quality checks / feedback",
#   "Online sharing platform", "OpenStats",
#   "Data curation tool", "Pre-registration",
#   "Computational reproducibility checks / feedback", "Data management training",
#   "Centralised sharing platform", "ARRIVE guidelines",
#   "BRISQ guidelines", "Other peer review processes",
#   "STROBE guidelines", "TREND guidelines",
#   "STREGA guidelines", "REMARK guidelines",
#   "ChEMBL", "Other quality checks / feedback",
#   "Registered reports", "Other open science badges",
#   "Open code badges", "TIDieR checklist",
#   "COREQ guidelines", "Systematic review training",
#   "Reproducible code/analysis training", "Materials sharing policy/guideline",
#   "Open Data Commons for Spinal Cord Injury (ODC-SCI)", "CONSERVE",
#   "MIAPE-Quant", "Results-free peer review",
#   "Mentoring / role models", "Wide-Open",
#   "TRIPOD guidelines", "Radiomics Quality Score",
#   "Checklist for Artificial Intelligence in Medical Imaging", "CARE guidelines"
# )
# 
# interventions_2 = c("TRIPOD guidelines", "SRWR guidelines", "COREQ guidelines", "PRISMA guidelines", "STROBE guidelines", "SPIRIT guidelines", "AGREE guidelines", "SRQR guidelines", "ARRIVE guidelines", "ICMJE guidelines", "SQUIRE guidelines", "CHEERS guidelines", "CARE guidelines", "STARD guidelines", "CONSORT guidelines", "TOP guidelines", "MDAR guidelines", "Other reporting guidelines, checklists, or standards", "Data sharing policy/guideline", "Materials sharing policy/guideline", "Code sharing policy/guideline", "Reporting quality checks / feedback", "Computational reproducibility checks / feedback", "Code quality checks / feedback", "Data quality checks / feedback", "Other quality checks / feedback", "Trial registration", "Analysis plans", "Pre-registration", "Protocol registration", "Registered reports", "Other peer review process", "Workflow standardisation", "Data access policies / agreements", "Open science plans", "Citation standards", "Open access publication", "Online sharing platform", "Open Science Framework", "Zenodo", "GitHub", "Centralised sharing platform", "Open science infrastructure", "Open code badges", "Open materials badges", "Open data badges", "Pre-registration badges", "Other open science badges", "Electronic lab notebooks", "Version control system", "Documentation system", "Experimental design", "Statistical method", "CReDiT taxonomy", "ORCID iD", "Data management training", "Systematic review training", "Pre-registration training", "Statistical training", "Reproducible code/analysis training", "Training community", "Mentoring / role models", "Results-free peer review", "Open peer review")
# missing_in_2 <- setdiff(interventions_1, interventions_2)


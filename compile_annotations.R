library(tidyr)
library(dplyr)
library(soles)
library(janitor)

setwd("/home/scsmith/projects/iRISE_new/")

# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))


annotated_data <- read.csv("Annotation_data_-_2024_05_31_-_Long_format_-_1059b82e-b4bb-42b2-af3d-78888c23473d_-_Investigators_Unblinded.csv") %>% 
  clean_names()


annotated_studies_small <- annotated_data %>% 
  select(study_id, custom_id,investigator_id, question, answer) %>% 
  pivot_wider(names_from = question, values_from = answer) %>% 
  clean_names() %>% 
  filter(reconciled_annotations == "True") %>% 
  rename(intervention = intervention_s_evaluated,
         discipline = target_discipline,
         research_stage = intervention_research_stage,
         outcome_measures = relevant_outcomes,
         method_of_delivery = method_of_delivery_of_intervention) %>% 
  select(-should_this_article_be_included) %>% 
  select(uid = custom_id, intervention, intervention_provider, target_population, discipline, method_of_delivery, research_stage, outcome_measures)


annotated_studies <- annotated_studies_small %>% 
  separate_rows(research_stage, sep = ";") %>% 
  separate_rows(discipline, sep = ";") %>% 
  separate_rows(intervention, sep = ";") %>% 
  separate_rows(intervention_provider, sep = ";") %>% 
  separate_rows(outcome_measures, sep = ";") %>% 
  separate_rows(method_of_delivery, sep = ";") %>% 
  filter(!discipline == "Other (leave a comment)") %>% 
  select(uid, intervention, intervention_provider, target_population, discipline, method_of_delivery, research_stage, outcome_measures)
  

#write.csv(reconciled_studies, "iRISE_syrf_annotations_300524.csv")  

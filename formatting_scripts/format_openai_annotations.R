library(soles)
library(dplyr)
library(DBI)
library(fst)
library(tidyr)
library(readr)
library(jsonlite)
library(janitor)


# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

# Bring in llm predictions and tidy
df <- read.csv("llm_files/tiabme_gpt4-turbo_2.csv",row.names = NULL)

df <- df[,-1]

rownames(df) <- NULL

gpt4_predict <- df %>% 
  select(uid, prediction) %>% 
  rowwise() %>% 
  mutate(prediction = list(fromJSON(prediction))) %>% 
  unnest_wider(prediction) %>% 
  clean_names()

# Create interventions dataframe
interventions <- gpt4_predict %>% 
  select(uid, intervention) %>% 
  separate_rows(intervention, sep = ";") %>%
  mutate(intervention = gsub("\\(.*?\\)", "", intervention)) %>% 
  mutate(intervention = trimws(intervention)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(interventions, "db_only_deploy_app/fst_files/interventions_df.fst")

# Create intervention provider dataframe
intervention_provider <- gpt4_predict %>% 
  select(uid, intervention_provider) %>% 
  separate_rows(intervention_provider, sep = ";") %>% 
  mutate(intervention_provider = gsub("\\(.*?\\)", "", intervention_provider)) %>% 
  mutate(intervention_provider = trimws(intervention_provider)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(intervention_provider, "db_only_deploy_app/fst_files/intervention_provider_df.fst")

# Create method of delivery dataframe
method_of_delivery <- gpt4_predict %>% 
  select(uid, method_of_delivery = method) %>% 
  separate_rows(method_of_delivery, sep = ";") %>%
  mutate(method_of_delivery = gsub("\\(.*?\\)", "", method_of_delivery)) %>% 
  mutate(method_of_delivery = trimws(method_of_delivery)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(method_of_delivery, "db_only_deploy_app/fst_files/method_of_delivery_df.fst")


# Create target population dataframe
target_population <- gpt4_predict %>% 
  select(uid, target_population) %>% 
  separate_rows(target_population, sep = ";") %>% 
  mutate(target_population = gsub("\\(.*?\\)", "", target_population)) %>% 
  mutate(target_population = trimws(target_population)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(target_population, "db_only_deploy_app/fst_files/target_population_df.fst")


# Create discipline dataframe
discipline <- gpt4_predict %>% 
  select(uid, discipline) %>% 
  separate_rows(discipline, sep = ";") %>% 
  mutate(discipline = gsub("\\(.*?\\)", "", discipline)) %>% 
  mutate(discipline = trimws(discipline)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(discipline, "db_only_deploy_app/fst_files/discipline_df.fst")


# Create research stage dataframe
research_stage <- gpt4_predict %>% 
  select(uid, research_stage) %>% 
  separate_rows(research_stage, sep = ";") %>%
  mutate(research_stage = gsub("\\(.*?\\)", "", research_stage)) %>% 
  mutate(research_stage = trimws(research_stage)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(research_stage, "db_only_deploy_app/fst_files/research_stage_df.fst")


# Create outcomes stage dataframe
outcome_measures <- gpt4_predict %>% 
  select(uid, outcome_measures) %>%
  separate_rows(outcome_measures, sep = ";") %>%
  mutate(outcome_measures = gsub("\\(.*?\\)", "", outcome_measures)) %>% 
  mutate(outcome_measures = trimws(outcome_measures)) %>% 
  mutate(method = "gpt4-turbo")

write.fst(outcome_measures, "db_only_deploy_app/fst_files/outcomes_df.fst")


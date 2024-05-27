library(lubridate)
library(soles)
library(shiny)
library(pool)
library(RPostgres)
library(shinythemes)
library(viridis)
library(viridisLite)
library(shinyalert)
library(fst)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(purrr)
library(networkD3)
library(RColorBrewer)
library(bs4Dash)
library(googlesheets4)
library(tools)
library(readr)
library(odbc)
library(DBI)
library(fresh)
library(htmlwidgets)
library(lubridate)
library(stringr)
library(readr)
library(jsonlite)
library(tidyr)
library(blastula)
library(glue)
library(parallel)
library(janitor)

setwd("/home/scsmith/projects/iRISE_new/")

# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

source("dummy_data_create.R")

# Create tables for app and write to fst -----
dataframes_for_app <- list()

# Create unique_citations table
unique_citations <- tbl(con, "unique_citations") %>% 
  select(date, uid, title, journal, year, doi, uid, url, author, abstract, keywords)

# Create large included studies table with metadata from unique_citations
citations_for_dl <- tbl(con, "study_classification")  %>%
  select(uid, decision) %>%
  filter(decision == "include") %>%
  left_join(tbl(con, "unique_citations"), by = "uid") %>%
  #select(-decision) %>% 
  collect() %>% 
  mutate(year = as.numeric(year))

dataframes_for_app[["citations_for_dl"]] <- citations_for_dl

# Fix dodgy years
# unique_years <- dbReadTable(con, "unique_citations") %>%
#   mutate(year = ifelse(uid == "medline-28336799", "2016", year),
#          year = ifelse(uid == "medline-28855368", "2017", year),
#          year = ifelse(uid == "medline-30082302", "2018", year),
#          year = ifelse(uid == "medline-28336792", "2016", year),
#          year = ifelse(uid == "medline-28336800", "2016", year),
#          year = ifelse(uid == "medline-27092246", "2016", year),
#          year = ifelse(uid == "medline-27534954", "2016", year)
#   )


# Create included tbl
included_with_metadata <- citations_for_dl  %>%
  select(date, uid, title, journal, year, doi, uid, url, author, abstract, keywords, decision)

dataframes_for_app[["included_with_metadata"]] <- included_with_metadata 

included_small <- included_with_metadata %>% 
  select(uid, doi, year)

# Gather data for included_per_year_plot
n_included_per_year_plot_data <- unique_citations %>%
  select(uid, year) %>% 
  collect() %>% 
  mutate(is_included = ifelse(uid %in% included_with_metadata$uid, "included", "excluded")) %>%
  mutate(year = as.numeric(year)) %>%
  select(year, is_included) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!year == "")

dataframes_for_app[["n_included_per_year_plot_data"]] <- n_included_per_year_plot_data

# Arrange dates
include_by_date <- included_with_metadata %>%
  distinct() %>%
  group_by(date) %>%
  count() %>%
  mutate(date = lubridate::dmy(date)) %>%
  arrange(desc(date)) %>%
  ungroup()

dataframes_for_app[["include_by_date"]] <- include_by_date

# Create pdfs df using full texts
pdfs <- tbl(con, "full_texts") %>% 
  select(status, doi) %>%
  collect()

dataframes_for_app[["pdfs"]] <- pdfs


# Bring in open access data
open_access_tag <- tbl(con, "oa_tag") %>%
  collect()

oa_tag <- included_small %>% 
  left_join(open_access_tag, by = "doi", relationship = "many-to-many") %>% 
  filter(!is.na(is_oa)) %>%
  distinct()

dataframes_for_app[["oa_tag"]] <- oa_tag


# Bring in transparency data
open_data_tag <- tbl(con, "open_data_tag") %>%
  collect()

transparency <- included_small %>% 
  left_join(open_data_tag, by = "doi", relationship = "many-to-many") %>%
  filter(!doi == ""|is.na(doi)) %>% 
  filter(!is.na(year)) %>%
  distinct()

#transparency[is.na(transparency)] <- "unknown"

dataframes_for_app[["transparency"]] <- transparency

funder <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>% 
  distinct(doi) %>% 
  mutate(status = "found")

dataframes_for_app[["funder"]] <- funder


citations_small <- citations_for_dl %>% 
  select(doi, year)

funder_overall_count <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>% 
  distinct(doi, funder_name) %>% 
  count(funder_name, sort = T) %>% 
  slice_head(n = 100)

dataframes_for_app[["funder_overall_count"]] <- funder_overall_count

funder_transparency <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>% 
  distinct(doi, funder_name) %>% 
  left_join(oa_tag %>% select(doi, is_oa), by = "doi") %>% 
  left_join(transparency %>% select(doi, is_open_data, is_open_code), by = "doi") %>% 
  filter(!is.na(is_oa))

dataframes_for_app[["funder_transparency"]] <- funder_transparency

funder_year <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>%
  left_join(citations_small, by = "doi") %>% 
  filter(!is.na(year)) %>% 
  select(doi, funder_name, year) %>% 
  group_by(year, funder_name) %>%
  count()

dataframes_for_app[["funder_year"]] <- funder_year




funder_metadata <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>%
  filter(doi %in% citations_small$doi) %>% 
  distinct(doi, funder_name) %>%
  left_join(citations_for_dl, by = "doi") %>% 
  select(uid, doi, funder_name, year, title, author, url) %>% 
  left_join(dummy_data_for_funder, by = "uid") %>% 
  filter(!is.na(intervention))

dataframes_for_app[["funder_metadata"]] <- funder_metadata

# Bring in llm predictions and tidy
llm_predictions <- read.csv("llm_files/tiabme_gpt4-turbo_2.csv",row.names = NULL)

llm_predictions <- llm_predictions[,-1]

rownames(llm_predictions) <- NULL

gpt4_predict <- llm_predictions %>% 
  select(uid, prediction) %>% 
  rowwise() %>% 
  mutate(prediction = list(fromJSON(prediction))) %>% 
  unnest_wider(prediction) %>% 
  clean_names()

# Create interventions dataframe
interventions_df <- gpt4_predict %>% 
  select(uid, intervention) %>% 
  separate_rows(intervention, sep = ";") %>%
  mutate(intervention = gsub("\\(.*?\\)", "", intervention)) %>% 
  mutate(intervention = trimws(intervention)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["interventions_df"]] <- interventions_df

interventions_df_small <- aggregate(intervention ~ uid, interventions_df, FUN = paste, collapse = "; ")

dataframes_for_app[["interventions_df_small"]] <- interventions_df_small



# Create intervention provider dataframe
intervention_provider_df <- gpt4_predict %>% 
  select(uid, intervention_provider) %>% 
  separate_rows(intervention_provider, sep = ";") %>% 
  mutate(intervention_provider = gsub("\\(.*?\\)", "", intervention_provider)) %>% 
  mutate(intervention_provider = trimws(intervention_provider)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["intervention_provider_df"]] <- intervention_provider_df

intervention_provider_df_small <- aggregate(intervention_provider ~ uid, intervention_provider_df, FUN = paste, collapse = "; ")

dataframes_for_app[["intervention_provider_df_small"]] <- intervention_provider_df_small


# Create method of delivery dataframe
method_of_delivery_df <- gpt4_predict %>% 
  select(uid, method_of_delivery = method) %>% 
  separate_rows(method_of_delivery, sep = ";") %>%
  mutate(method_of_delivery = gsub("\\(.*?\\)", "", method_of_delivery)) %>% 
  mutate(method_of_delivery = trimws(method_of_delivery)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["method_of_delivery_df"]] <- method_of_delivery_df

method_of_delivery_df_small <- aggregate(method_of_delivery ~ uid, method_of_delivery_df, FUN = paste, collapse = "; ")

dataframes_for_app[["method_of_delivery_df_small"]] <- method_of_delivery_df_small


# Create target population dataframe
target_population_df <- gpt4_predict %>% 
  select(uid, target_population) %>% 
  separate_rows(target_population, sep = ";") %>% 
  mutate(target_population = gsub("\\(.*?\\)", "", target_population)) %>% 
  mutate(target_population = trimws(target_population)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["target_population_df"]] <- target_population_df

target_population_df_small <- aggregate(target_population ~ uid, target_population_df, FUN = paste, collapse = "; ")

dataframes_for_app[["target_population_df_small"]] <- target_population_df_small


# Create discipline dataframe
discipline_df <- gpt4_predict %>% 
  select(uid, discipline) %>% 
  separate_rows(discipline, sep = ";") %>% 
  mutate(discipline = gsub("\\(.*?\\)", "", discipline)) %>% 
  mutate(discipline = trimws(discipline)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["discipline_df"]] <- discipline_df

discipline_df_small <- aggregate(discipline ~ uid, discipline_df, FUN = paste, collapse = "; ")

dataframes_for_app[["discipline_df_small"]] <- discipline_df_small


# Create research stage dataframe
research_stage_df <- gpt4_predict %>% 
  select(uid, research_stage) %>% 
  separate_rows(research_stage, sep = ";") %>%
  mutate(research_stage = gsub("\\(.*?\\)", "", research_stage)) %>% 
  mutate(research_stage = trimws(research_stage)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["research_stage_df"]] <- research_stage_df

research_stage_df_small <- aggregate(research_stage ~ uid, research_stage_df, FUN = paste, collapse = "; ")

dataframes_for_app[["research_stage_df_small"]] <- research_stage_df_small


# Create outcomes stage dataframe
outcomes_df <- gpt4_predict %>% 
  select(uid, outcome_measures) %>%
  separate_rows(outcome_measures, sep = ";") %>%
  mutate(outcome_measures = gsub("\\(.*?\\)", "", outcome_measures)) %>% 
  mutate(outcome_measures = trimws(outcome_measures)) %>% 
  mutate(method = "gpt4-turbo")

dataframes_for_app[["outcomes_df"]] <- outcomes_df

outcomes_df_small <- aggregate(outcome_measures ~ uid, outcomes_df, FUN = paste, collapse = "; ")

dataframes_for_app[["outcomes_df_small"]] <- outcomes_df_small

data_for_bubble <- included_small %>% 
  select(uid) %>% 
  inner_join(interventions_df, by = "uid") %>% 
  inner_join(intervention_provider_df, by = "uid") %>% 
  inner_join(target_population_df, by ="uid") %>% 
  inner_join(method_of_delivery_df, by = "uid") %>% 
  inner_join(discipline_df, by ="uid") %>% 
  inner_join(research_stage_df, by = "uid") %>% 
  inner_join(outcomes_df, by = "uid") %>% 
  select(-starts_with("method."))

dataframes_for_app[["data_for_bubble"]] <- data_for_bubble


data_for_bubble_small <- included_small %>% 
  select(uid) %>% 
  inner_join(interventions_df_small, by = "uid") %>% 
  inner_join(intervention_provider_df_small, by = "uid") %>% 
  inner_join(target_population_df_small, by ="uid") %>% 
  inner_join(method_of_delivery_df_small, by = "uid") %>% 
  inner_join(discipline_df_small, by ="uid") %>% 
  inner_join(research_stage_df_small, by = "uid") %>% 
  inner_join(outcomes_df_small, by = "uid") %>% 
  select(-starts_with("method."))

dataframes_for_app[["data_for_bubble_small"]] <- data_for_bubble_small

dummy_data_title_case <- dummy_data_for_bubble %>% 
  rename_with(~ stringr::str_replace_all(stringr::str_to_title(.), pattern = "_", replacement = " ")) %>% 
  rename("uid" = "Uid")

dataframes_for_app[["dummy_data_title_case"]] <- dummy_data_title_case

dataframes_for_app[["dummy_data_for_bubble"]] <- dummy_data_for_bubble

#dummy_bubble <- read.fst("dummy_deploy_app/fst_files/data_for_bubble.fst")

pico <- data.frame(uid = character())

dataframes_for_app[["pico"]] <- pico

# Create folder for fst_files if it does not exist
fst_files_written <- 0
if (!file.exists("deploy_app/fst_files")) {
  dir.create("deploy_app/fst_files")
}

# Write all of the dataframes required to fst files
for (name in names(dataframes_for_app)) {
  dataframe <- dataframes_for_app[[name]]
  write_fst(dataframe, paste0("deploy_app/fst_files/", name, ".fst"))
  fst_files_written <- fst_files_written + 1
}


# Redeploy the app
app_deploy <- try({
  rsconnect::deployApp(
    appDir = "deploy_app",
    appFiles = c("app.R",
                 "irise_modules.R",
                 "fst_files/",
                 "www/"),
    account = "camarades",
    appName  = "irise-soles",
    logLevel = "verbose",
    launch.browser = F, 
    forceUpdate = T)
})

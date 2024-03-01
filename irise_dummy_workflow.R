library(lubridate)
library(rwoslite)
library(rwos)
library(soles)
library(shiny)
library(pool)
library(RPostgres)
library(shinythemes)
library(viridis)
library(viridisLite)
library(shiny)
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
library(curl)

setwd("/home/scsmith/projects/iRISE_new/")

# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))



# If the workflow is complete then create dataframes for app and save as fst files
# Send automated email with full report from workflow and log file attached
dataframes_for_app <- list()

# Create unique_citations table
unique_citations <- tbl(con, "unique_citations") %>% 
  select(date, uid, title, journal, year, doi, uid, url, author, abstract, keywords)

# Create smaller lzy_tbl table for future joins
unique_citations_doi <- unique_citations %>%
  select(doi, uid, year, date, url)

# For citations download
unique_citations_for_dl <- tbl(con, "unique_citations") %>%
  collect()

# unique_citations_for_db <- unique_citations_for_dl %>% 
#   mutate(year = as.numeric(year)) %>% 
#   filter(!(is.na(year) | year == ""))
# 
# glimpse(unique_citations_for_dl)
# 
# dataframes_for_app[["unique_citations_for_db"]] <- unique_citations_for_db

dataframes_for_app[["unique_citations_for_dl"]] <- unique_citations_for_dl


# Create smaller table for future joins
unique_citations_doi_df <- unique_citations_for_dl %>%
  select(doi, uid, year, date, url) %>% 
  distinct()

# From machine screen table - get counts of included studies per date and load all included studies
conf_abstracts <- tbl(con, "article_type") %>%
  filter(ptype %in% c("conference-abstract",  "proceedings-article"),
         !doi == "")

# Create included tbl
included <- tbl(con, "study_classification")  %>%
  select(uid, decision) %>% 
  left_join(unique_citations_doi, by = "uid") %>%
  select(-year, -date, -url) %>%
  filter(decision == "include") %>%  
  anti_join(conf_abstracts, by = "doi") %>%
  select(-doi)

# Create included with meta table
included_with_metadata <- unique_citations %>%
  inner_join(included, by = "uid") %>%
  collect() %>% 
  mutate(year = as.numeric(year))

dataframes_for_app[["included_with_metadata"]] <- included_with_metadata

# Gather data for included_per_year_plot
n_included_per_year_plot_data <- unique_citations_doi_df %>%
  mutate(is_included = ifelse(uid %in% included_with_metadata$uid, "included", "excluded")) %>% 
  select(year, is_included) %>%
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

# Create open access table
oa_tag <- tbl(con, "oa_tag") %>%
  left_join(unique_citations_doi, by = "doi") %>%
  filter(!is.na(is_oa)) %>%
  collect() %>% 
  filter(uid %in% included_with_metadata$uid) %>% 
  filter(!year == "")

dataframes_for_app[["oa_tag"]] <- oa_tag

# Create transparency table with open data
transparency <- tbl(con, "open_data_tag") %>%
  left_join(unique_citations_doi, by="doi") %>%
  collect() %>%   
  filter(uid %in% included_with_metadata$uid) %>% 
  filter(!year == "")

transparency[is.na(transparency)] <- "unknown"

dataframes_for_app[["transparency"]] <- transparency


# Bring in dictionary and tagged table for joining to create pico ontology full
names <- tbl(con, "pico_dictionary") %>%
  select(id, name) %>%  
  collect() 

pico_tagged <- tbl(con, "pico_tag") %>%
  select(-strings) %>% 
  arrange(regex_id) %>% 
  collect()

# Included studies uids - used to full join with tagged elements to create "Unknown"
# pico tags when the studies are yet to be tagged
included_with_metadata_uid <- included_with_metadata %>% 
  select(uid)

pico_ontology_full <- tbl(con, "pico_ontology") %>%
  collect() %>% 
  arrange(regex_id) %>%
  left_join(names, by = c("regex_id" = "id")) %>%
  inner_join(pico_tagged, relationship = "many-to-many", by = c("regex_id")) %>%
  left_join(unique_citations_doi_df, by = "uid") %>%
  mutate(year = ifelse(year == "" | is.na(year), "Unknown", year),
         frequency = as.numeric(frequency)) %>%
  filter(!year == "Unknown") %>%
  distinct()

# Create interventions table from pico_ontology_full
interventions_df <- pico_ontology_full %>%
  filter(type %in% c("intervention_method"),
         method %in% c("dummy method"),
         uid %in% included_with_metadata$uid) %>%
  #filter(!(method == "fulltext_regex" & frequency < 3)) %>%
  select(-doi, -date, -url) %>%
  distinct() %>% 
  full_join(included_with_metadata_uid, by = "uid", relationship = "many-to-many") %>% 
  mutate(name = ifelse(is.na(name), "Unknown Intervention", name),
         regex_id = ifelse(name == "Unknown Intervention", 9999993, name),
         main_category = ifelse(is.na(main_category), "Unknown", main_category)) %>% 
           filter(!is.na(year))

interventions_df["name"] <- as.data.frame(sapply(interventions_df["name"], toTitleCase))

dataframes_for_app[["interventions_df"]] <- interventions_df

interventions_df_small <- interventions_df %>%
  select(name, uid) %>%
  filter(!name == "Unknown") %>%
  rename(intervention = name) %>%
  distinct()

interventions_df_small <- aggregate(intervention ~ uid, interventions_df_small, FUN = paste, collapse = "; ")

dataframes_for_app[["interventions_df_small"]] <- interventions_df_small

# Create interventions table from pico_ontology_full
outcomes_df <- pico_ontology_full %>%
  filter(type %in% c("target_outcome"),
         method %in% c("dummy method"),
         uid %in% included_with_metadata$uid) %>%
  #filter(!(method == "fulltext_regex" & frequency < 3)) %>%
  select(-doi, -date, -url, -frequency, -year) %>%
  distinct() %>% 
  full_join(included_with_metadata, by = "uid", relationship = "many-to-many") %>% 
  mutate(name = ifelse(is.na(name), "Unknown Outcome", name),
         regex_id = ifelse(name == "Unknown Outcome", 9999995, name),
         main_category = ifelse(is.na(main_category), "Unknown", main_category)) %>% 
  select(regex_id, main_category, sub_category1, sub_category2, type, name, uid, year)


outcomes_df["name"] <- as.data.frame(sapply(outcomes_df["name"], toTitleCase))

dataframes_for_app[["outcomes_df"]] <- outcomes_df

outcomes_df_small <- outcomes_df %>%
  select(name, uid) %>%
  filter(!name == "Unknown") %>%
  rename(outcome = name) %>%
  distinct()

outcomes_df_small <- aggregate(outcome ~ uid, outcomes_df_small, FUN = paste, collapse = "; ")

dataframes_for_app[["outcomes_df_small"]] <- outcomes_df_small


int_out_df <- interventions_df %>%
  select(uid, name) %>%
  rename(intervention = name) %>% 
  left_join(outcomes_df, by = "uid") %>% 
  select(uid, intervention, outcome = name, year) %>% 
  filter(!is.na(year))

dataframes_for_app[["int_out_df"]] <- int_out_df



# Create interventions table from pico_ontology_full
population_df <- pico_ontology_full %>%
  filter(type %in% c("population"),
         method %in% c("dummy method"),
         #name %in% c("government", "funder", "publisher", "journal", "student", "researcher", "support staff", "unknown population", "Unknown Population"),
         uid %in% included_with_metadata$uid) %>%
  #filter(!(method == "fulltext_regex" & frequency < 3)) %>%
  select(-doi, -date, -url) %>%
  distinct() %>% 
  full_join(included_with_metadata_uid, by = "uid", relationship = "many-to-many") %>% 
  mutate(name = ifelse(is.na(name), "Unknown Population", name),
         regex_id = ifelse(name == "Unknown Population", 9999996, name),
         main_category = ifelse(is.na(main_category), "Unknown", main_category)
  ) 

population_df["name"] <- as.data.frame(sapply(population_df["name"], toTitleCase))
population_df["main_category"] <- as.data.frame(sapply(population_df["main_category"], toTitleCase))

dataframes_for_app[["population_df"]] <- population_df

population_small_df <- population_df %>%
  select(main_category, uid) %>%
  filter(!main_category == "Unknown") %>%
  rename(population = main_category) %>%
  distinct()

population_small_df <- aggregate(population ~ uid, population_small_df, FUN = paste, collapse = "; ")

dataframes_for_app[["population_small_df"]] <- population_small_df


author_location_df <- pico_ontology_full %>%
  filter(type %in% c("country"),
         method == "author_country_affiliation",
         uid %in% included_with_metadata$uid) %>%
  # filter(!(method == "fulltext_regex" & frequency < 3)) %>%
  select(-method, -frequency) %>% 
  select(uid, main_category, name) %>%
  distinct() %>% 
  full_join(included_with_metadata_uid, by = "uid", relationship = "many-to-many") %>% 
  mutate(name = ifelse(is.na(name), "Unknown Country", name),
         regex_id = ifelse(name == "Unknown Country", 9999997, name),
         main_category = ifelse(is.na(main_category), "Unknown", main_category))

dataframes_for_app[["author_location_df"]] <- author_location_df

author_location_small <- author_location_df %>%
  select(name, main_category, uid) %>%
  filter(!name == "Unknown Country") %>%
  rename(author_aff_country = name,
         author_aff_continent = main_category) %>%
  distinct()

author_location_country_small <- aggregate(author_aff_country ~ uid, author_location_small, FUN = paste, collapse = "; ")
author_location_continent_small <- aggregate(author_aff_continent ~ uid, author_location_small, FUN = paste, collapse = "; ")

author_location_combined_small <- author_location_country_small %>% 
  left_join(author_location_continent_small, by = "uid")

dataframes_for_app[["author_location_combined_small"]] <- author_location_combined_small

author_location_for_bubble <- author_location_continent_small %>% 
  mutate(author_aff_continent = sapply(strsplit(as.character(author_aff_continent), ";"), `[`, 1)) 
  

# Gather the tables in their current state
discipline_df <- tbl(con, "discipline_tag") %>%
  #filter(uid %in% included_with_metadata$uid) %>%
  select(uid, main_discipline) %>%
  rename("name" = "main_discipline") %>% 
  distinct() %>%
  collect() %>% 
  full_join(included_with_metadata_uid, by = "uid") %>%
  mutate(name = ifelse((is.na(name) | name == "Unknown"), "Unknown Discipline", name))

dataframes_for_app[["discipline_df"]] <- discipline_df

discipline_df_small <- discipline_df %>%
  select(name, uid) %>%
  filter(!name == "Unknown Discipline") %>%
  rename(discipline = name) %>%
  distinct()

discipline_df_small <- aggregate(discipline ~ uid, discipline_df_small, FUN = paste, collapse = "; ")

dataframes_for_app[["discipline_df_small"]] <- discipline_df_small

funder_tag_df <- tbl(con, "funder_grant_tag") %>%
  filter(!funder_name == "Unknown") %>% 
  collect() %>% 
  select(uid, name = funder_name) %>% 
  mutate(name = sapply(strsplit(as.character(name), ";"), `[`, 1)) %>% 
  full_join(included_with_metadata_uid, by = "uid") %>%
  mutate(name = ifelse((is.na(name) | name == "Unknown"), "Unknown Funder", name))

dataframes_for_app[["funder_tag_df"]] <- funder_tag_df

funder_tag_df_small <- funder_tag_df %>%
  select(name, uid) %>%
  filter(!name == "Unknown Funder") %>%
  rename(funder = name) %>%
  distinct()

funder_tag_df_small <- aggregate(funder ~ uid, funder_tag_df_small, FUN = paste, collapse = "; ")

dataframes_for_app[["funder_tag_df_small"]] <- funder_tag_df_small

data_for_bubble <- included_with_metadata_uid %>% 
  left_join(population_df, by = "uid") %>% 
  select(uid, pop_inter = main_category, pop_target = name) %>% 
  left_join(interventions_df[, c("uid","name")], by ="uid") %>%
  rename(intervention = name) %>% 
  left_join(outcomes_df[, c("uid", "name")], by = "uid") %>%
  rename(outcome = name) %>% 
  left_join(discipline_df[, c("uid", "name")], by = "uid") %>% 
  rename(discipline = name) %>% 
  left_join(author_location_for_bubble[, c("uid", "author_aff_continent")], by ="uid") %>%  
  left_join(funder_tag_df, by = "uid") %>% 
  rename(funder = name)
  
dataframes_for_app[["data_for_bubble"]] <- data_for_bubble

# Create pico df using aggregated data
pico <- author_location_country_small %>% 
  full_join(population_small_df, by = "uid") %>% 
  full_join(interventions_df_small, by = "uid") %>% 
  full_join(discipline_df_small, by = "uid") %>% 
  full_join(funder_tag_df_small, by = "uid")

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
                 "irise_dummy_modules.R",
                 "sunburst.R",
                 "fst_files/",
                 "www/"),
    account = "camarades",
    appName  = "dev-irise-soles",
    logLevel = "verbose",
    launch.browser = F, 
    forceUpdate = T)
})

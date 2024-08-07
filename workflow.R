library(sf)
library(rmapshaper)
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

# Connect to db
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

workflow <- try({
  
  source("search_strategy.R")
  
  # Update retrieved citations with new search results, checking the uid doesn't already exist in the table
  try(new_citations <- check_if_retrieved(con, combined_result))
  
  # Find additional DOI's
  try(new_citations <- get_missing_dois(new_citations))
  
  # Find additional abstracts
  try(new_citations <- get_missing_abstracts(new_citations))
  
  # Remove duplicate citations
  try(new_citations_unique <- get_new_unique(con, new_citations))
  
  # Pull date for email update
  try(date_update <- new_citations_unique %>%
        distinct(date) %>%
        mutate(date = dmy(date)) %>%
        pull())
  
  
  # Append new citations to unique citations table
  #try(dbWriteTable(con, "unique_citations", new_citations_unique, append = TRUE))
  
  # Screening and Machine Learning
  #source("functions/get_screening_decisions.R")
  
  try(screening_decisions <- get_screening_decisions(con, review_id = "iRISE-SOLES-screening"))
  
  
  try(unscreened_set <- get_studies_to_screen(con,
                                              classify_NA = TRUE,
                                              project_name = "iRISE-SOLES",
                                              classifier_name = "irise"))
  
  try(run_ml(con, project_name = "iRISE-SOLES",
             classifier_name = "irise",
             screening_decisions,
             unscreened_set))
  
  try(screened_studies <- tbl(con, "study_classification") %>%
        collect() %>%
        filter(date == Sys.Date()))
  
  try(included_studies <- screened_studies %>% 
        filter(date == Sys.Date(),
               decision == "include"))
  
  
  # Tagging -----
  # Retrieve full texts + count update
  try(pre_text_found <- tbl(con, "full_texts") %>%
        collect() %>% 
        filter(status == "found") %>%
        nrow())
  
  try(get_ft(con, path = "full_texts"))
  
  try(post_text_found <- tbl(con, "full_texts") %>%
        collect() %>% 
        filter(status == "found") %>%
        nrow())
  
  # Open access status tagging
  pre_open_access <- tbl(con, "oa_tag") %>%
    select(doi) %>% 
    collect()
  
  # Funder tagging
  pre_funder_tag <- dbReadTable(con, "funder_grant_tag") %>% 
    distinct(doi)
  
  # Number of papers or institutions tagged??
  pre_institute_tag <- dbReadTable(con, "institution_tag") %>% 
    distinct(doi)
  
  try(get_openalex_metadata(con))
  
  post_open_access <- tbl(con, "oa_tag") %>%
    select(doi) %>% 
    collect()
  
  post_funder_tag <- dbReadTable(con, "funder_grant_tag") %>% 
    distinct(doi)
  
  post_institute_tag <- dbReadTable(con, "institution_tag") %>% 
    distinct(doi)
  
  
  # Open data and code availability
  pre_ods_tag <- tbl(con, "open_data_tag") %>%
    select(doi) %>% 
    collect()
  
  try(ods_tag(con, path = "full_texts/"))
  
  post_ods_tag <- tbl(con, "open_data_tag") %>%
    select(doi) %>% 
    collect()
  
  
})

current_time <- Sys.time()

try({
  if (inherits(workflow, "try-error")) {
    
    workflow_error <- as.character(workflow)
    
    try(my_email  <-
          compose_email(
            body = md(glue(
              "
## iRISE-SOLES Automated Workflow Update

Task Incomplete at {try(current_time)}

Full Workflow Incomplete due to error: {try(workflow_error)}

"
            ))
          ) %>% add_attachment(file = "workflow.log",
                               filename = "workflow.log"
          ))
    
    smtp_send(my_email,
              from = "soles.updates@gmail.com",
              to = c("ssmith49@ed.ac.uk", "kaitlyn.hair@ed.ac.uk"),
              subject = "iRISE SOLES Update",
              credentials = creds_file("/home/scsmith/soles_updates_gmail"))
  } else {

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
  select(uid, doi, year, title)

# Gather data for included_per_year_plot
n_included_per_year_plot_data <- unique_citations %>%
  select(uid, year) %>% 
  collect() %>% 
  mutate(is_included = ifelse(uid %in% included_with_metadata$uid, "included", "excluded")) %>%
  mutate(year = as.numeric(year)) %>%
  select(year, is_included) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!year == "") %>% 
  filter(is_included == "included")

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

#source("dummy_data_create.R")
source("formatting_scripts/compile_annotations.R")

#source("formatting_scripts/format_llm_predictions.R")

data_for_bubble_small <- included_small %>% 
  select(uid) %>% 
  inner_join(annotated_studies_small, by = "uid")

dataframes_for_app[["data_for_bubble_small"]] <- data_for_bubble_small

data_for_bubble <- included_small %>% 
  select(uid) %>% 
  inner_join(intervention_df, by = "uid") %>% 
  inner_join(intervention_provider_df, by = "uid") %>% 
  inner_join(target_population_df, by ="uid") %>% 
  inner_join(location_pop_df, by ="uid") %>% 
  inner_join(discipline_df, by ="uid") %>% 
  inner_join(research_stage_df, by = "uid") %>% 
  inner_join(outcome_measures_df, by = "uid") %>% 
  select(-starts_with("method."))

# data_for_bubble <- included_small %>% 
#   select(uid) %>% 
#   inner_join(interventions_df, by = "uid") %>% 
#   inner_join(intervention_provider_df, by = "uid") %>% 
#   inner_join(target_population_df, by ="uid") %>% 
#   inner_join(target_pop_location_df, by ="uid") %>% 
#   inner_join(discipline_df, by ="uid") %>% 
#   inner_join(research_stage_df, by = "uid") %>% 
#   inner_join(outcome_measures_df, by = "uid") %>% 
#   select(-starts_with("method."))
# 
# data_for_bubble_small_test <- included_small %>% 
#   select(uid) %>% 
#   inner_join(predictions_df, by = "uid") %>% 
#   separate_rows(intervention, sep = ";") %>% 
#   separate_rows(intervention_provider, sep = ";") %>% 
#   separate_rows(target_population, sep = ";") %>% 
#   separate_rows(target_population_location, sep = ";") %>% 
#   separate_rows(discipline, sep = ";") %>% 
#   separate_rows(research_stage, sep = ";") %>% 
#   separate_rows(outcome_measures, sep = ";")
  

dataframes_for_app[["data_for_bubble"]] <- data_for_bubble

dataframes_for_app[["annotated_studies"]] <- annotated_studies
dataframes_for_app[["annotated_studies_small"]] <- annotated_studies_small


# Create Funder tables
funder <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>% 
  distinct(doi) %>% 
  mutate(status = "found")

dataframes_for_app[["funder"]] <- funder


citations_small <- citations_for_dl %>% 
  select(doi, year)

# Take the top 100 funders
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
  left_join(citations_for_dl, by = "doi") %>% 
  select(uid, doi, funder_name, year, title, author, url) %>% 
  left_join(data_for_bubble, by = "uid") %>%
  #left_join(dummy_data_for_funder, by = "uid") %>% 
  #filter(!is.na(intervention)) %>%
  distinct()

dataframes_for_app[["funder_metadata"]] <- funder_metadata

funder_metadata_small <- dbReadTable(con, "funder_grant_tag") %>% 
  filter(!str_starts(funder_name, "https")) %>% 
  filter(!funder_name == "Unknown") %>%
  filter(doi %in% citations_small$doi) %>% 
  left_join(citations_for_dl, by = "doi") %>% 
  select(uid, doi, funder_name, year, title, author, url) %>% 
  left_join(data_for_bubble_small, by = "uid") %>%
  #left_join(dummy_data_for_funder, by = "uid") %>% 
  #filter(!is.na(intervention)) %>%
  distinct()

dataframes_for_app[["funder_metadata_small"]] <- funder_metadata_small


## REMINDER, make institution type titlecase in workflow (change it in function?)
inst <- dbReadTable(con, "institution_tag") %>% 
  mutate(type = toTitleCase(type))

pico_country <- dbReadTable(con,"pico_ontology") %>% 
  filter(type == "country") %>% 
  dplyr::select(country = name, continent = main_category, sub_category2)

# ror_dummy_data <- dbReadTable(con, "institution_location") %>% 
#   left_join(dbReadTable(con, "institution_tag"), by = "doi") %>% 
#   left_join(pico_country, by = c("institution_country_code" = "sub_category2")) %>%  
#   left_join(included_with_metadata, by = "doi") %>% 
#   left_join(dummy_data_for_map, by = "uid") %>% 
#   distinct() %>%   
#   group_by(name) %>% 
#   mutate(number_pub = n()) %>% 
#   ungroup() %>% 
#   filter(!name == "Unknown") %>% 
#   mutate(lat = latitude,
#          long = longitude)
# 
# dataframes_for_app[["ror_dummy_data"]] <- ror_dummy_data

ror_data <- dbReadTable(con, "institution_location") %>% 
  left_join(dbReadTable(con, "institution_tag"), by = "doi") %>% 
  left_join(pico_country, by = c("institution_country_code" = "sub_category2")) %>%  
  left_join(included_with_metadata, by = "doi") %>% 
  left_join(data_for_bubble, by = "uid") %>% 
  distinct() %>%   
  group_by(name) %>% 
  mutate(number_pub = n_distinct(uid)) %>% 
  ungroup() %>% 
  filter(!name == "Unknown") %>% 
  mutate(lat = latitude,
         long = longitude) %>% 
  mutate(outcome_measures = ifelse(is.na(outcome_measures), "Unknown", outcome_measures)) %>% 
  mutate(discipline = ifelse(is.na(discipline), "Unknown", discipline)) 

dataframes_for_app[["ror_data"]] <- ror_data

ror_data_small <- dbReadTable(con, "institution_location") %>% 
  left_join(dbReadTable(con, "institution_tag"), by = "doi") %>% 
  left_join(pico_country, by = c("institution_country_code" = "sub_category2")) %>%  
  left_join(included_with_metadata, by = "doi") %>% 
  left_join(data_for_bubble_small, by = "uid") %>% 
  distinct() %>%   
  group_by(name) %>% 
  mutate(number_pub = n_distinct(uid)) %>% 
  ungroup() %>% 
  filter(!name == "Unknown") %>% 
  mutate(lat = latitude,
         long = longitude) %>% 
  mutate(outcome_measures = ifelse(is.na(outcome_measures), "Unknown", outcome_measures)) %>% 
  mutate(discipline = ifelse(is.na(discipline), "Unknown", discipline))

dataframes_for_app[["ror_data_small"]] <- ror_data_small

#pico <- data.frame(uid = character())

pico <- data_for_bubble_small %>% 
  select(uid, intervention, discipline, outcome_measures)



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
                 "www/",
                 "helpfiles/"),
    account = "camarades",
    appName  = "irise-soles",
    logLevel = "verbose",
    launch.browser = F, 
    forceUpdate = T)
})

try({  
  if (inherits(app_deploy, "try-error")) {
    app_deployment <- "Failed"
    
  } else {
    app_deployment <- "Successful"
  }
})

try(my_email  <-
      compose_email(
        body = md(glue(
          "
## iRISE-SOLES Automated Workflow Update

Task Complete at {try(current_time)}

### __Search Results__

Number of distinct pubmed citations found: {try(nrow(pubmed))}

Number of distinct scopus citations found: {try(nrow(scopus))}

Number of distinct citations found total: {try(nrow(combined_result))}

### __New Citations__

Number of new citations retrieved: {try(nrow(new_citations))}

Number of new unique citations added: {try(nrow(new_citations_unique))}

### __Screening__

Number of citations screened: {try(nrow(screened_studies))}

Number of new included studies: {try(nrow(included_studies))}

### __Get Full Texts__

Number of full texts retrieved: {try(post_text_found - pre_text_found)}

### __Pico Tagging__

Number of studies tagged by Outcome using full text: {try(nrow(post_outcome_tag) - nrow(pre_outcome_tag))}

Number of studies tagged by Intervention using full text: {try(nrow(post_intervention_tag) - nrow(pre_intervention_tag))}

Number of studies tagged by Model using full text: {try(nrow(post_model_tag) - nrow(pre_model_tag))}

Number of studies tagged by Species using full text: {try(nrow(post_species_tag) - nrow(pre_species_tag))}

### __Open Access Tagging__

Number of studies tagged for Open Access: {try(nrow(post_open_access) - nrow(pre_open_access))}

### __Open Data Tagging__

Number of studies tagged for Open Data and Code Availability: {try(nrow(post_ods_tag) - nrow(pre_ods_tag))}

### __Write Data__

Number of new fst files created: {try(fst_files_written)}

### __Deploy App__

Re-deployment of app: {try(app_deployment)}

"
        ))
        
      ) %>% add_attachment(file = "workflow.log",
                           filename = "workflow.log"
      ))

smtp_send(my_email,
          from = "soles.updates@gmail.com",
          to = c("ssmith49@ed.ac.uk", "kaitlyn.hair@ed.ac.uk"),
          subject = "iRISE SOLES Update",
          credentials = creds_file("/home/scsmith/soles_updates_gmail"))


  }
})

# If error occurs during previous step then send email stating "Task Incomplete" with workflow attached
my_email  <-
  compose_email(
    body = md(glue(
      "
## iRISE SOLES Automated Workflow Update

Task Incomplete

"
    ))
  ) %>% add_attachment(file = "workflow.log",
                       filename = "workflow.log"
  )

dbDisconnect(con)
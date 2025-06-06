#library(tidyr)
library(dplyr)
library(soles)
library(sf)
library(rmapshaper)
library(lubridate)
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
#library(jsonlite)
library(blastula)
library(glue)
library(parallel)
library(janitor)

# Set working directory
setwd("/home/scsmith/projects/iRISE-SOLES/")

# Connect to db
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))


workflow <- try({

  # Search databases and bring in results
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
  try(dbWriteTable(con, "unique_citations", new_citations_unique, append = TRUE))
  
  screening_decisions <- read_csv("screening/validation/labelled_data_assigned_iteration_26-02-2025.csv") %>%
    filter(iteration == 16) %>%
    filter(!Cat == "Validate") %>%
    select(ITEM_ID, LABEL, TITLE, ABSTRACT, Cat, KEYWORDS, REVIEW_ID) %>%
    mutate(Cat = ifelse(Cat == "Calibrate", "Test", Cat)) %>% 
    mutate(Cat = ifelse(!Cat == "Test", "Train", Cat))
  

  try(unscreened_set <- get_studies_to_screen(con,
                                              classify_NA = TRUE,
                                              project_name = "iRISE-SOLES",
                                              classifier_name = "irise"))

  try(run_ml(con, project_name = "iRISE-SOLES",
             classifier_name = "irise",
             screening_decisions,
             unscreened_set))
  
  try(screened_studies <- dbReadTable(con, "study_classification") %>%
        filter(date == Sys.Date()))
  
  try(included_studies <- screened_studies %>%
        filter(date == Sys.Date(),
               decision == "include"))
  
  ## Update Grey Literature ----
  # Search databases and bring in results
  source("grey_lit_search_strategy.R")
  
  # Update retrieved citations with new search results, checking the uid doesn't already exist in the table
  try(grey_new_citations <- check_if_retrieved(con, combined_grey_lit))
  
  # Find additional DOI's
  try(grey_new_citations <- get_missing_dois(grey_new_citations))
  
  # Find additional abstracts
  try(grey_new_citations <- get_missing_abstracts(grey_new_citations))
  
  # Bring in functions to find new unique grey literature
  source("functions/grey_lit_get_unique.R")
  
  # Remove duplicate citations
  try(grey_new_citations_unique <- grey_lit_get_new_unique(con, grey_new_citations))
  
  # Pull date for email update
  try(date_update <- grey_new_citations_unique %>%
        distinct(date) %>%
        mutate(date = dmy(date)) %>%
        pull())
  
  # Append new citations to unique citations table
  try(dbWriteTable(con, "grey_lit_unique_citations", grey_new_citations_unique, append = TRUE))
  
  # Bring in functions for grey literature screening
  source("functions/screen_grey_lit.R")
  
  try(unscreened_set <- grey_lit_get_studies_to_screen(con,
                                              classify_NA = TRUE,
                                              project_name = "iRISE-SOLES",
                                              classifier_name = "irise"))
  
  try(grey_lit_run_ml(con, project_name = "iRISE-SOLES",
             classifier_name = "irise",
             screening_decisions,
             unscreened_set))

  try(grey_lit_screened_studies <- tbl(con, "grey_lit_study_classification") %>%
        collect() %>%
        filter(date == Sys.Date()))

  try(grey_lit_included_studies <- grey_lit_screened_studies %>%
        filter(date == Sys.Date(),
               decision == "include"))


  # Tagging -----
  # Retrieve full texts + count update
  try(pre_text_found <- tbl(con, "full_texts") %>%
        collect() %>%
        filter(status == "found") %>%
        nrow())

  try(get_ft(con, path = "full_texts"))
  try(get_ft(con, path = "full_texts"))
  try(get_ft(con, path = "full_texts", check_failed = TRUE))
  

  try(post_text_found <- tbl(con, "full_texts") %>%
        collect() %>%
        filter(status == "found") %>%
        nrow())
  
  # Convert pdfs to txt using autoannotation within pico_tag
  complete_pico_tag(con, 
                    tag_method = "fulltext",
                    tag_type = "sex")

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

# Create grey literature table
grey_lit <- tbl(con, "grey_lit_study_classification") %>% 
  select(uid, decision) %>% 
  filter(decision == "include") %>% 
  left_join(tbl(con, "grey_lit_unique_citations"), by = "uid") %>%
  mutate(year = as.numeric(year)) %>%
  collect() %>%
  select(-decision) %>% 
  mutate(ptype = ifelse(grepl("pprn:", uid), "Preprint", ptype))

dataframes_for_app[["grey_lit"]] <- grey_lit

# Create large included studies table with metadata from unique_citations
citations_for_dl <- tbl(con, "study_classification")  %>%
  select(uid, decision) %>%
  filter(decision == "include") %>%
  left_join(tbl(con, "unique_citations"), by = "uid") %>%
  mutate(year = ifelse(year == "", NA_character_, year)) %>% 
  mutate(year = as.numeric(year)) %>%
  collect() %>% 
  filter(!uid %in% grey_lit$uid)

dataframes_for_app[["citations_for_dl"]] <- citations_for_dl

retracted_embase <- unique_citations %>% 
  filter(str_detect(title, "^Retraction|^Retracted|^Erratum: retracted:")) %>% 
  collect()

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

# fix_years <- dbReadTable(con, "unique_citations") %>% 
#   mutate(year = ifelse(uid == "embase-51668480", 2012, year)) %>% 
#   mutate(year = ifelse(uid == "embase-52948227", 2014, year)) %>% 
#   mutate(year = ifelse(uid == "unknown-1026", 1981, year)) %>% 
#   mutate(year = ifelse(uid == "unknown-1329", 2023, year))

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

#source("formatting_scripts/compile_annotations.R")
source("update_scripts/llm_update_script.R")
source("formatting_scripts/format_all_annotations.R")


# Create Funder tables
funder <- dbReadTable(con, "funder_grant_tag") %>%
  filter(!stringr::str_starts(funder_name, "https")) %>%
  filter(!funder_name == "Unknown") %>%
  distinct(doi) %>%
  mutate(status = "found")

dataframes_for_app[["funder"]] <- funder

citations_small <- citations_for_dl %>%
  select(doi, year)

# Take the top 100 funders
funder_overall_count <- dbReadTable(con, "funder_grant_tag") %>%
  filter(!stringr::str_starts(funder_name, "https")) %>%
  filter(!funder_name == "Unknown") %>%
  distinct(doi, funder_name) %>%
  count(funder_name, sort = T) %>%
  slice_head(n = 100)

dataframes_for_app[["funder_overall_count"]] <- funder_overall_count

funder_transparency <- dbReadTable(con, "funder_grant_tag") %>%
  filter(!stringr::str_starts(funder_name, "https")) %>%
  filter(!funder_name == "Unknown") %>%
  distinct(doi, funder_name) %>%
  left_join(oa_tag %>% select(doi, is_oa), by = "doi") %>%
  left_join(transparency %>% select(doi, is_open_data, is_open_code), by = "doi") %>%
  filter(!is.na(is_oa))

dataframes_for_app[["funder_transparency"]] <- funder_transparency

funder_year <- dbReadTable(con, "funder_grant_tag") %>%
  filter(!stringr::str_starts(funder_name, "https")) %>%
  filter(!funder_name == "Unknown") %>%
  left_join(citations_small, by = "doi") %>%
  filter(!is.na(year)) %>%
  select(doi, funder_name, year) %>%
  group_by(year, funder_name) %>%
  count()

dataframes_for_app[["funder_year"]] <- funder_year

funder_metadata <- dbReadTable(con, "funder_grant_tag") %>%
  filter(!stringr::str_starts(funder_name, "https")) %>%
  filter(!funder_name == "Unknown") %>%
  filter(doi %in% citations_small$doi) %>%
  left_join(citations_for_dl, by = "doi") %>%
  select(uid, doi, funder_name, year, title, author, url) %>%
  left_join(all_annotations, by = "uid") %>%
  distinct()

dataframes_for_app[["funder_metadata"]] <- funder_metadata

funder_metadata_small <- dbReadTable(con, "funder_grant_tag") %>%
  filter(!stringr::str_starts(funder_name, "https")) %>%
  filter(!funder_name == "Unknown") %>%
  filter(doi %in% citations_small$doi) %>%
  left_join(citations_for_dl, by = "doi") %>%
  select(uid, doi, funder_name, year, title, author, url) %>%
  left_join(all_annotations_small, by = "uid") %>%
  distinct()

dataframes_for_app[["funder_metadata_small"]] <- funder_metadata_small

## REMINDER, make institution type titlecase in workflow (change it in function?)
inst <- dbReadTable(con, "institution_tag") %>%
  mutate(type = tools::toTitleCase(type))

pico_country <- dbReadTable(con,"pico_ontology") %>%
  filter(type == "country") %>%
  dplyr::select(country = name, continent = main_category, sub_category2)

## Once ROR coords table is full use this instead of location, join by ror!

ror_data <- dbReadTable(con, "ror_coords") %>%
  left_join(inst, by = "ror") %>%
  left_join(pico_country, by = c("institution_country_code" = "sub_category2")) %>%
  filter(doi %in% included_with_metadata$doi) %>%
  left_join(included_with_metadata, by = "doi") %>%
  select(-method) %>%
  left_join(all_annotations, by = "uid") %>%
  distinct() %>%
  group_by(name) %>%
  mutate(number_pub = n_distinct(uid)) %>%
  ungroup() %>%
  filter(!name == "Unknown") %>% 
  distinct() %>%
  select(longitude, latitude, doi, uid, inst_name = name, type, country, continent, discipline, outcome_measures, number_pub) %>% 
  arrange(doi)

dataframes_for_app[["ror_data"]] <- ror_data


ror_data_small <- dbReadTable(con, "ror_coords") %>%
  left_join(inst, by = "ror") %>%
  left_join(pico_country, by = c("institution_country_code" = "sub_category2")) %>%
  filter(doi %in% included_with_metadata$doi) %>%
  left_join(included_with_metadata, by = "doi") %>%
  #select(-method) %>% 
  left_join(all_annotations_small, by = "uid") %>%
  distinct() %>%
  group_by(name) %>%
  mutate(number_pub = n_distinct(uid)) %>%
  ungroup() %>%
  filter(!name == "Unknown") %>% 
  select(title, journal, year, author, longitude, latitude, doi, uid, name, type, country, continent, discipline, outcome_measures, number_pub) 

dataframes_for_app[["ror_data_small"]] <- ror_data_small

pico <- all_annotations_small %>%
  select(uid, intervention, discipline, outcome_measures, intervention_provider)

dataframes_for_app[["pico"]] <- pico

grey_lit_pico <- grey_lit %>% 
  select(uid, doi, name = ptype) %>% 
  mutate(name = toTitleCase(name)) %>% 
  mutate(name = ifelse(is.na(name)|name == "", "Unknown", name))

dataframes_for_app[["grey_lit_pico"]] <- grey_lit_pico

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

### __Grey Literature__

Number of citations screened: {try(nrow(grey_lit_screened_studies))}

Number of new included studies: {try(nrow(grey_lit_included_studies))}

### __Get Full Texts__

Number of full texts retrieved: {try(post_text_found - pre_text_found)}

### __Open Access Tagging__

Number of studies tagged for Open Access: {try(nrow(post_open_access) - nrow(pre_open_access))}

### __Open Data Tagging__

Number of studies tagged for Open Data and Code Availability: {try(nrow(post_ods_tag) - nrow(pre_ods_tag))}

### __Evidence Type__

Run GPT-4o-mini on evidence type: {try(evidence_type_message)}

Number of Aim1/Controlled studies found: {try(nrow(evidence_type_predictions_new %>% filter(type == 'controlled')))}

Number of Aim2/Uncontrolled studies found: {try(nrow(evidence_type_predictions_new %>% filter(type == 'uncontrolled')))}

### __LLM Annotation__

Run GPT-4o on Aim1/Controlled studies for full annotation: {try(annotation_result_message)}

Number of Aim1/Controlled studies annotated: {try(nrow(combined_df))}

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

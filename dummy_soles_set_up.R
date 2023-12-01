library(dplyr)
library(janitor)
library(soles)
library(readxl)
library(tidyr)
library(stringr)
library(rcrossref)
library(DBI)

# Import and dedup search ----
# read in search
search <- fst::read.fst("second_search_modified.fst")

# make sure no dup uids
search <- search %>%
  unique() %>%
  group_by(uid) %>%
  slice_head() %>%
  ungroup()

# dedup using asysd
all_unique <- soles::dedup_first_search(search) 
all_unique <- all_unique$unique

# Filter search for relevant studies ----
pattern <- "open science|reproducibility"
likely_relevant <- all_unique[grepl(pattern, all_unique$keywords, ignore.case = TRUE), ]
not_relevant <- likely_relevant[grepl("measure*", likely_relevant$keywords, ignore.case = TRUE), ]
likely_relevant <- likely_relevant %>%
  filter(!title %in% not_relevant$title)

# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

# Write tables ----
study_classification <- all_unique %>%
  select(uid) %>%
  mutate(decision = ifelse(uid %in% likely_relevant$uid, "include", "exclude")) %>%
  mutate(name = "irise-relevant") %>%
  mutate(score = 0, date = "011223", cid="999", type = "human")

all <- search %>% select(uid, date)

dbWriteTable(con, "retrieved_citations", all)
dbWriteTable(con, "unique_citations", all_unique)
dbWriteTable(con, "study_classification", study_classification)

get_missing_citation_discipline_and_funder(con, n = 4000)

# This function populates the "discipline_tag" and "funder_grant_tag" tables concurrently.
# This function requires 2 tables to be created in the db.
# "citation_discipline" - uid, doi, (main_discipline, level, score) == NA
# "funder_grant_tag" - uid, doi, (funder_name, funder_details, grants_award_id) == NA

get_missing_citation_discipline_and_funder <- function(con, n = as.numeric()){

# if table doesn't exist, create it
  if (!dbExistsTable(con, "funder_grant_tag")) {
    
   discipline <- dataframe(uid = "", 
             doi = "", main_discipline="", 
             level= "",
             score = "")
    
    funding <- dataframe(uid = "", 
              doi = "", funder_name="", 
              funder_details= "",
              grants_award_id = "")
              
    dbWriteTable(con, "funder_grant_tag", funder)  
    dbWriteTable(con, "discipline_tag", discipline) 
    
  }
  
  # Gather the tables in their current state
  citation_discipline_full <- tbl(con, "discipline_tag") %>% 
    collect()
  
  citation_funder_full <- tbl(con, "funder_grant_tag") %>% 
    collect() 
  
  included <- dbReadTable(con, "study_classification") %>% filter(decision == "include")
  dois <- tbl(con, "unique_citations") %>% select(uid, doi) %>% collect()
  
  
  # Filter for rows containing no discipline
  citations_no_discipline <- dois  %>%
    filter(uid %in% included$uid) %>%
    filter(!uid %in% citation_funder_full$uid) %>% 
    slice_sample(n = n)
  
  print(paste0(length(citations_no_discipline$uid), " papers with no discipline"))
  
  if(length(citations_no_discipline$uid) < 1) {
    message("no missing disciplines")
    return(citations)
  }
  
  # Use the doi's with no discipline (which should also have no funder data) to search OpenAlex
  
  res <- NULL
  
  # Create a dataframe with data from openAlex
  for(i in 1:length(citations_no_discipline$uid)){
    try(new <- openalexR::oa_fetch(
      identifier = NULL,
      entity = "works",
      doi = citations_no_discipline$doi[i]),silent=TRUE)
    
    if(is.data.frame(new)){
      res <- plyr::rbind.fill(res, new)
    }
  }
  
  
  if(is.null(res)){
    
    message("no additional citations with discipline found")
    return(citations)
  }
  
  
  # Take the results and transform data for discipline_tag
  res_concepts <- res %>% 
    unnest(concepts, names_sep = "_") %>% 
    select(doi, concepts_display_name, concepts_level, concepts_score) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    filter(!concepts_score == 0,
           concepts_level == 1 | concepts_level == 0) %>%
    rename("main_discipline" = "concepts_display_name",
           "level" = "concepts_level",
           "score" = "concepts_score")
  
  unique_names <- c("grants_funder", "grants_funder_display_name", "grants_award_id")
  
  # Take the results and transform data for funder_grant_tag
  res_funder <- res %>% 
    select(doi, grants) %>%
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    unnest_wider(grants, names_sep = "_", names_repair = "unique") %>%
    select(-grants_1) %>% 
    unite(funder_name, starts_with("grants_funder_display"), sep = ";", remove = TRUE) %>%
    unite(grant_id, starts_with("grants_award"), sep = ";", remove = TRUE) %>%
    unite(funder_details, starts_with("grants_funder"), sep = ";", remove = TRUE) %>%
    select(-starts_with("grants_funder_display")) %>% 
    select(-starts_with("grants_award")) %>% 
    select(-starts_with("grants_funder")) %>% 
    mutate(funder_name = ifelse(funder_name == "NA;NA;NA", "Unknown", funder_name)) %>% 
    mutate(funder_details = ifelse(funder_details == "NA;NA;NA", NA, funder_details)) %>% 
    mutate(grant_id = ifelse(grant_id == "NA;NA;NA", NA, grant_id)) %>% 
    select(doi, funder_name, funder_details, grant_id) %>% 
    rename("grants_award_id" = "grant_id")
  
  # Join back to no discipline table adding "Unknown" to NA or empty elements
  citations_with_discipline <- citations_no_discipline %>%
    left_join(res_concepts, by = "doi") %>% 
    mutate(main_discipline = ifelse(score < 0.5, "Unknown", main_discipline)) %>%
    group_by(uid, level) %>% 
    slice_max(score) %>%
    mutate(main_discipline = ifelse(is.na(main_discipline) | main_discipline == "", "Unknown", main_discipline))
  
  # Join back to no funder table adding "Unknown" to NA or empty elements
  citations_with_funder <- citations_no_discipline %>%
    left_join(res_funder, by = "doi") %>% 
    mutate(funder_name = ifelse(is.na(funder_name) | funder_name == "" | grepl("NA;", funder_name), "Unknown", funder_name)) %>%
    mutate(funder_details = ifelse(is.na(funder_details) | funder_details == "" | grepl("NA;", funder_details), "Unknown", funder_details)) %>%
    mutate(grants_award_id = ifelse(is.na(grants_award_id) | grants_award_id == "" | grepl("NA;", grants_award_id), "Unknown", grants_award_id))
  
  
  # Overwrite discipline table with new data
  dbWriteTable(con, "discipline_tag", citations_with_discipline, overwrite = TRUE)
  
  # Overwrite funder table with new data
  dbWriteTable(con, "funder_grant_tag", citations_with_funder, overwrite = TRUE)
  
  
  message(paste0(length(citations_with_funder$uid), " citations with discipline added!"))
  
  message(paste0(length(citations_with_discipline$uid), " citations with funder data added!"))
  
}

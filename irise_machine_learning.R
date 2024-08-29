library(soles)
library(dplyr)
library(DBI)
library(crayon)
library(fst)
library(tidyr)
library(devtools)
library(readr)


# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))

# Write lowercase uids to db to deal with eric issues
unique <- dbReadTable(con, "unique_citations") %>% 
  mutate(uid = tolower(uid))

#dbWriteTable(con, "unique_citations", unique, overwrite = T)

# Write the latest screening decisions to the database
syrf_decisions_to_db(con, "screening_decisions_data/Screening_data_-_2024_05_08_-_Long_format_-_1059b82e-b4bb-42b2-af3d-78888c23473d_-_Investigators_Unblinded.csv",
                     with_annotations = FALSE,
                     classifier_name = "irise"
)


study_classification <- dbReadTable(con, "study_classification")

# Check the inclusion rate
decision_count <- study_classification %>%
  count(decision)

# Check for duplicates
dup_check <- study_classification %>% 
  count(uid)

unique_citations <- dbReadTable(con, "unique_citations")

# Check all studies are present in unique citations
uid_check <- unique_citations %>% 
  filter(uid %in% study_classification$uid)

# Get screening decisions from the database
source("functions/utility_functions.R")
screening_decisions <- get_screening_decisions(con, review_id = "iRISE-soles")

# Get all unscreened studies from unique citations
source("functions/screen_studies.R")
unscreened_set <- get_studies_to_screen(con,
                                        classify_NA = TRUE,
                                        project_name = "iRISE-soles",
                                        classifier_name = "irise")

# Run Machine Learning
run_ml(con, project_name = "iRISE-soles",
       classifier_name = "irise",
       screening_decisions,
       unscreened_set)

# Run K-Fold Validation
run_k_fold(con, project_name = "iRISE-soles_kfold",
           classifier_name = "irise",
           screening_decisions_review_id = "iRISE-soles_kfold",
           fold_number = 5)

# Get studies for error correction
run_error_correction(con, 
                     k_fold_scores = "k-fold-validation/results/iRISE-soles_kfold_test_scores_all_090524.csv",
                     k_fold_performance = "k-fold-validation/results/iRISE-soles_kfold_perf_all_folds_090524.csv",
                     type = "extreme discrepancies",
                     number_to_be_re_reviewed = 250)

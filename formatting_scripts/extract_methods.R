library(soles)
library(dplyr)
library(parallel)
library(DBI)
library(fst)
library(lubridate)
library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(fs)


# Connect to db ----
con <- dbConnect(RPostgres::Postgres(),
                 dbname = Sys.getenv("irise_soles_dbname"),
                 host = Sys.getenv("irise_soles_host"),
                 port = 5432,
                 user = Sys.getenv("irise_soles_user"),
                 password = Sys.getenv("irise_soles_password"))


# Define the regular expression pattern
pattern <- "(Methods|Method|Methods and Materials|Experimental section|Methods and materials|Methodology|METHODS|Materials and methods|Materials and Methods|M ATE R I A L S A N D M E TH O DS)\\n([\\s\\S]{1,20000})"

# Suppose df is your pre-existing dataframe
included <- tbl(con, "study_classification")  %>%
  select(uid, decision) %>%
  filter(decision == "include") %>%
  left_join(tbl(con, "unique_citations"), by = "uid") %>%
  collect()

ft <- dbReadTable(con, "full_texts") %>%
  filter(status == "found")

included_texts <- included %>% 
  left_join(ft, by ="doi") %>%
  filter(status == "found") %>%
  distinct() %>% 
  select(uid, doi, title, abstract, path) 

update_to_txt_extension <- function(path) {
  # Replace anything after the last dot (if present) to .txt
  str_replace(path, "\\.[^.]*$", ".txt")
}

# Apply the function to the 'path' column to create 'txt_path'
included_texts_new_path <- included_texts %>%
  mutate(txt_path = map_chr(path, update_to_txt_extension))

check_file_exists <- function(file_path) {
  file_exists(file_path)
}

# Add 'path_exists' column to check if each 'txt_path' exists
df <- included_texts_new_path %>%
  mutate(path_exists = map_lgl(txt_path, check_file_exists)) 


# Function to extract the methods section from a file given its path
extract_methods <- function(file_path) {
  #browser()
  file_contents <- read_file(file_path)
  matches <- str_match(file_contents, pattern)
  
  if (!is.na(matches[3])) {
    return(matches[3])  # return the captured group which is the methods section
  } else {
    return("No Methods section found")
  }
}

# Applying the function to each path in the dataframe and creating a new column
included_texts_new <- df %>%
  mutate(methods = map_chr(path, extract_methods))

included_methods_found <- included_texts_new %>% 
  select(uid, methods)

included_with_methods <- included %>%
  left_join(included_methods_found, by = "uid") %>% 
  select(uid, doi, title, abstract, methods) %>% 
  mutate(methods = ifelse(is.na(methods), "No Methods section found", methods))


evidence_type_predictions <- read.csv("all_included_predictions_evidence_type_gpt_4o_mini_0shot_050824.csv") %>% 
  clean_names() %>% 
  filter(type_of_evidence == "Aim 1")

evidence_type1_methods <- included_with_methods %>% 
  filter(uid %in% evidence_type_predictions$uid)

write_csv(evidence_type1_methods, "iRISE_evidence_type1_methods.csv")



# Optionally, write the updated dataframe back to a CSV file
write_csv(included_with_methods, "iRISE_included_with_methods.csv")


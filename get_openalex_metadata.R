#' Extract concepts, funder, citation count, institution and open access data from OpenAlex.
#'
#' This function retrieves meta-data.
#'
#' @param con connection to db
#' @param n number of citations to retrieve metadata for
#'
#' @import DBI
#' @import dplyr
#' @import dbplyr
#' @import plyr
#' @import tidyr
#' @import stringr
#' @import openalexR
#' @return Tables "discipline_tag", "funder_grant_tag", "institution_tag", "citation_count_tag", "oa_tag" will be updated with n number of new rows containing metadata. If no data is retrieved for these n citations, "Unknown" will be returned in the relevant columns. 
#' @export
#' @examples get_openalex_metadata(con, n = 500)

get_openalex_metadata <- function(con, n = as.numeric()){
  
  # if table doesn't exist, create it ----
  if (!dbExistsTable(con, "funder_grant_tag")) {
    
    discipline <- data.frame(uid = "", 
                            doi = "", main_discipline="", 
                            level= "",
                            score = "")
    
    funding <- data.frame(uid = "", 
                         doi = "", funder_name="", 
                         funder_details= "",
                         grants_award_id = "")
    
    dbWriteTable(con, "funder_grant_tag", funder)  

  }
  
  if (!dbExistsTable(con, "discipline_tag")) {
    
    discipline <- data.frame(uid = "", 
                             doi = "", main_discipline="", 
                             level= "",
                             score = "")
    
    dbWriteTable(con, "discipline_tag", discipline) 
    
  }
  
  if (!dbExistsTable(con, "institution_tag")) {
    
    institution <- data.frame(uid = "", 
                              doi = "", instiution_id = "", 
                              name = "",
                              ror = "",
                              country = "",
                              type = "")
    
    dbWriteTable(con, "institution_tag", institution)  
    
  }
  
  if (!dbExistsTable(con, "citation_count_tag")) {
    
    citation_count <- data.frame(doi = "", 
                                 count = as.integer(), 
                                 method = "")
    
    dbWriteTable(con, "citation_count_tag", institution)  
    
  }
  
  if (!dbExistsTable(con, "article_type")) {
    
    article <- data.frame(doi = "", 
                                 count = as.integer(), 
                                 method = "")
    
    dbWriteTable(con, "article_type", article)  
    
  }
  
  if (!dbExistsTable(con, "oa_tag")) {
    
    open_access <- data.frame(doi = "", 
                          is_oa = "", 
                          oa_status = "",
                          method = "")
    
    dbWriteTable(con, "oa_tag", article)  
    
  }
  
  # Gather the tables in their current state ----
  institution_full <- tbl(con, "institution_tag") %>% 
    collect()
  
  discipline_full <- tbl(con, "discipline_tag") %>% 
    collect()
  
  funder_full <- tbl(con, "funder_grant_tag") %>% 
    collect()
  
  citation_count_full <- tbl(con, "citation_count_tag") %>% 
    collect()
  
  open_access_full <- tbl(con, "oa_tag") %>% 
    collect()
  
  included <- dbReadTable(con, "study_classification") %>% filter(decision == "include")
  dois <- tbl(con, "unique_citations") %>% select(uid, doi) %>% collect()
  
  # Filter for rows containing no data ----
  citations_missing_data <- dois  %>%
    filter(uid %in% included$uid) %>%
    filter(!uid %in% institution_full$uid) %>%
    filter(!(is.na(doi) | doi == "")) %>% 
    slice_sample(n = n)
  
  print(paste0("Retrieving data for ", length(citations_missing_data$uid), " papers"))
  
if(length(citations_missing_data$uid) < 1) {
    message("no missing data")
    return(citations_missing_data)
  }

  
# Use the doi's with no discipline (which should also have no funder data) to search OpenAlex
  res <- NULL
  
  # Create a dataframe with data from openAlex ----
  for(i in 1:length(citations_missing_data$uid)){
    suppressWarnings({
      
    try(new <- openalexR::oa_fetch(
      identifier = NULL,
      entity = "works",
      doi = citations_missing_data$doi[i]),silent=TRUE)
    })
    if(is.data.frame(new)){
      res <- plyr::rbind.fill(res, new)
    }
  }
  
  if(is.null(res)){
    
    message("no additional citations missing data")
    return(citations)
  }
  
  
  # Create table of country names with corresponding codes ----
  country_code_names <- tbl(con, "pico_dictionary") %>%
    left_join(tbl(con, "pico_ontology"), by = c("id" = "regex_id" )) %>% 
    filter(type == "country") %>%
    select(country = name, sub_category2) %>% 
    collect()
  
  # Unnest author data, and extract institution info ----
  res_institution <- res %>% 
    unnest(author) %>%
    filter(author_position == "first") %>%
    select(doi, institution_id, name = institution_display_name, ror = institution_ror, institution_country_code, type = institution_type) %>%
    mutate(institution_country_code = toupper(institution_country_code), 
           doi = str_remove(doi, "https://doi.org/")) %>%
    left_join(country_code_names, by = c("institution_country_code" = "sub_category2")) %>% 
    select(-institution_country_code)
  
  # Take results and transform data for discipline_tag
  res_concepts <- res %>% 
    unnest(concepts, names_sep = "_") %>% 
    select(doi, concepts_display_name, concepts_level, concepts_score) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    filter(!concepts_score == 0,
           concepts_level == 1 | concepts_level == 0) %>%
    rename("main_discipline" = "concepts_display_name",
           "level" = "concepts_level",
           "score" = "concepts_score") %>% 
    mutate(main_discipline = ifelse(score < 0.4, "Unknown", main_discipline)) %>%
    filter(!main_discipline == "Unknown") %>% 
    group_by(doi) %>% 
    slice_max(score) %>% 
    ungroup()
  
  # Take results and transform data for funder_grant_tag
  res_funder <- res %>% 
    select(doi, grants) %>%
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    unnest_wider(grants, names_sep = "_", names_repair = "unique") %>%
    select(-grants_1) %>% 
    unite(funder_name, starts_with("grants_funder_display"), sep = ";", remove = TRUE) %>%
    unite(grants_award_id, starts_with("grants_award"), sep = ";", remove = TRUE) %>%
    unite(funder_details, starts_with("grants_funder"), sep = ";", remove = TRUE) %>%
    select(doi, funder_name, funder_details, grants_award_id) 

    # Transform data for citation_count_tag
  res_citation_count <- res %>% 
    select(doi, count = cited_by_count) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex") %>% 
    filter(!doi %in% citation_count_full$doi)
  
  res_oa <- res %>% 
    select(doi, is_oa, oa_status) %>% 
    mutate(doi = str_remove(doi, "https://doi.org/")) %>% 
    mutate(method = "OpenAlex") %>% 
    filter(!doi %in% open_access_full$doi)
  
  # Join back to no institution table adding "Unknowns" to NA or empty elements ----
  citations_with_institution <- citations_missing_data %>% 
    left_join(res_institution, by = "doi") %>%
    mutate(name = ifelse(!(is.na(name) | name == ""), name, "Unknown")) %>% 
    mutate(institution_id = ifelse(!(is.na(institution_id) | institution_id == ""), institution_id, "Unknown")) %>%
    mutate(ror = ifelse(!(is.na(ror) | ror == ""), ror, "Unknown")) %>% 
    mutate(country = ifelse(!(is.na(country) | country == ""), country, "Unknown")) %>% 
    mutate(type = ifelse(!(is.na(type) | type == ""), type, "Unknown")) %>% 
    filter(!doi %in% institution_full$doi)

  # Join back to no discipline table adding "Unknown" to NA or empty elements
  citations_with_discipline <- citations_missing_data %>%
    left_join(res_concepts, by = "doi") %>% 
    mutate(main_discipline = ifelse(is.na(main_discipline) | main_discipline == "", "Unknown", main_discipline)) %>% 
    filter(!doi %in% discipline_full$doi)
  
  # Join back to no funder table adding "Unknown" to NA or empty elements
  # Separate the rows to account for multiple funders on 1 citation
  citations_with_funder <- citations_missing_data %>%
    left_join(res_funder, by = "doi") %>% 
    mutate(funder_name = ifelse(is.na(funder_name) | funder_name == "" | grepl("NA;", funder_name) | grepl("\\bNA\\b", funder_name), "Unknown", funder_name)) %>%
    mutate(funder_details = ifelse(is.na(funder_details) | funder_details == "" | grepl("NA;", funder_details) | grepl("\\bNA\\b", funder_details), "Unknown", funder_details)) %>%
    mutate(grants_award_id = ifelse(is.na(grants_award_id) | grants_award_id == "" | grepl("NA;", grants_award_id) | grepl("\\bNA\\b", grants_award_id), "Unknown", grants_award_id)) %>% 
    separate_rows(c(funder_name, funder_details, grants_award_id), sep = ";") %>%
    distinct() %>% 
    filter(!doi %in% funder_full$doi)
  
  # Count number of citations with data added ----
  institutions_added <- citations_with_institution %>% 
    filter(!name == "Unknown")
  
  discipline_added <- citations_with_discipline %>% 
    filter(!main_discipline == "Unknown")
  
  funders_added <- citations_with_funder %>% 
    filter(!funder_name == "Unknown")
  
  # Overwrite tables with new data ----
  dbWriteTable(con, "institution_tag", citations_with_institution, append = TRUE)
  dbWriteTable(con, "discipline_tag", citations_with_discipline, append = TRUE)
  dbWriteTable(con, "funder_grant_tag", citations_with_funder, append = TRUE)
  dbWriteTable(con, "oa_tag", res_oa, append = TRUE)
  dbWriteTable(con, "citation_count_tag", res_citation_count, append = TRUE)
  
  message(paste0("Data retrieved from OpenAlex:"))
  message(paste0(length(institutions_added$uid), " citations with institution data added to institution_tag! ", "(",(length(citations_with_institution$uid) - (length(institutions_added$uid))),  " 'Unknown')"))
  message(paste0(length(discipline_added$uid), " citations with discipline data added to discipline_tag! ", "(",(length(citations_with_discipline$uid) - (length(discipline_added$uid))),  " 'Unknown')"))
  message(paste0(length(funders_added$uid), " citations with funder data added to funder_grant_tag! ", "(",(length(citations_with_funder$uid) - (length(funders_added$uid))),  " 'Unknown')"))
  message(paste0(length(res_oa$doi), " citations with open access data added to oa_tag!"))
  message(paste0(length(res_citation_count$doi), " citations with citation count added to citation_count_tag!"))
}

get_openalex_metadata(con, n = 20)

# dbListTables(con)
# 
# citation_count <- tbl(con, "citation_count_tag") %>% 
#   collect()
# 
# 
# my_dois_works <- rcrossref::cr_works(dois = citations_missing_data$doi)
# 
# crossref_data <- my_dois_works$data
# 
# crossref_funder <- crossref_data %>% 
#   unnest(funder) %>% 
#   select(doi, funder_name = name, award) %>% 
#   distinct() %>% 
#   filter(doi %in% funders_added$doi)
# 
# crossref_article <- crossref_data %>% 
#   select(doi, type, language) %>% 
#   distinct(type)
# 
# 

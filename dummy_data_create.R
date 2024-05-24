# Load required libraries
library(dplyr)
library(purrr)

# Define list names
evidence_categories <- c("Controlled observational study evaluating intervention (Aim #1)", "Controlled experiment evaluating intervention (Aim #1)", "Other evidence supporting an intervention (non-comparator research) (Aim #2)", "Evaluation of modifying factors of intervention (Aim #3)")
intervention_categories <- c("Reporting guidelines, checklists, or standards (for example: TRIPOD, SRWR, COROQ, PRISMA, STROBE, SPIRIT, AGREE, SRQR, ARRIVE, ICMJE, SQUIRE, CHEERS, CARE, STARD, CONSORT, TOP, MDAR)", "Data sharing policy/guideline", "Materials sharing policy/guideline", "Code sharing policy/guideline", "other", "Quality checks / feedback (for example: Reporting quality, Computational reproducibility, Code quality, Data quality)", "Trial registration", "Analysis plan", "Pre-registration", "Protocol registration")
                             #"Registered reports", "Assessment quality", "Peer review processes (for example: Results-free peer review, Open peer review, Other peer review process)", "Workflow standardisation", "Data access policies / agreements", "Open science plans", "Citation standards", "Open access publication", "Publication policies", "Preprints", "Online sharing platform (for example: Open Science Framework , Zenodo, GitHub)", "Centralised sharing platform", "Open science infrastructure", "Open science badges (for example: Open code, Open materials, Open data, Pre-registration)", "Electronic lab notebooks", "Version control system", "Documentation system", "Experimental design", "Statistical method", "CRediT taxonomy", "ORCID iD", "Data management training", "Systematic review training", "Pre-registration training", "Statistical training", "Reproducible code/analysis training", "Training community", "Mentoring / role models")
provider_categories <- c("Government", "Funder", "Publisher / journal", "Institution", "Students", "Researchers / Researcher Collaboration", "Learned societies", "Research support staff", "Organisation", "Other")
mod_categories <- c("Uptake of tool/practice", "Policy",  "Endorsements and encouragement", "Rewards and incentives", "Training and Education", "other")
target_categories <- c("Government", "Funder", "Publisher / journal", "Institution", "Students", "Researchers / Researcher Collaboration", "Learned societies", "Research support staff", "Organisation", "Other")
discipline_categories <- c("Natural sciences (e.g., Mathematics, Computer and information sciences, Physical sciences, Chemical sciences, Earth and related environmental sciences, Biological sciences)", "Engineering and technology (e.g., Civil engineering, Electrical engineering, Mechanical engineering, Chemical engineering, Materials engineering, Medical engineering, Environmental engineering, Environmental biotechnology, Industrial biotechnology, Nano-technology)", "Biomedical and health sciences (e.g., Basic medicine, Clinical medicine, Health sciences, Medical biotechnology)", "Agricultural and veterinary sciences (e.g., Agriculture, forestry and fisheries, Animal and diary science, Veterinary science, Agricultural biotechnology)", "Social sciences (e.g., Psychological and cognitive sciences, Economics and business, Education, Sociology, Law, Political Science, Social and economic geography, Media and communications)", "Humanities and the arts (e.g., History and archaeology, Languages and literature, Philosophy, ethics and religion, Arts)", "Other")
research_stage_categories <- c("Planning and design stage", "Conduct stage", "Reporting stage", "Outreach", "Assessment")
outcome_categories <- c("Type-I error reduction", "Type-II error reduction", "Effect size estimation", "Transparency of funding", "Transparency of interests", "Transparency of contributions", "Data availability and re-use", "Materials availability and re-use", "Code / analysis availability and re-use", "Reporting quality", "Transparency of evaluation", "Reporting bias", "Publication bias", "Computational reproducibility", "Research process / workflow transparency")

# Generate dummy data
generate_dummy_data <- function(categories) {
  sample(categories, 20000, replace = TRUE)
}

data <- list(
  #'Evidence Category' = generate_dummy_data(evidence_categories),
  "intervention" = generate_dummy_data(intervention_categories),
  "intervention_provider" = generate_dummy_data(provider_categories),
  "method_of_delivery" = generate_dummy_data(mod_categories),
  "target_population" = generate_dummy_data(target_categories),
  "discipline" = generate_dummy_data(discipline_categories),
  "research_stage" = generate_dummy_data(research_stage_categories),
  "outcome_measures" = generate_dummy_data(outcome_categories)
)

# Convert list to data frame
dummy_data <- as.data.frame(data) %>%
  mutate(discipline = gsub("\\(.*?\\)", "", discipline)) %>% 
  mutate(intervention = gsub("\\(.*?\\)", "", intervention)) 
  


random_uids <- dbReadTable(con, "study_classification") %>% 
  filter(decision == "include") %>% 
  select(uid) %>% 
  slice_sample(n = 200, replace = TRUE)

dummy_data_for_bubble <- cbind(random_uids, dummy_data) 

dummy_count <- dummy_data_for_bubble %>% 
  count(uid)

# Create Dummy Data for Funder -----

generate_funder_dummy_data <- function(categories) {
  sample(categories, 20000, replace = TRUE)
}

data <- list(
  #'Evidence Category' = generate_funder_dummy_data(evidence_categories),
  "intervention" = generate_funder_dummy_data(intervention_categories),
  "intervention_provider" = generate_funder_dummy_data(provider_categories),
  "method_of_delivery" = generate_funder_dummy_data(mod_categories),
  "target_population" = generate_funder_dummy_data(target_categories),
  "discipline" = generate_funder_dummy_data(discipline_categories),
  "research_stage" = generate_funder_dummy_data(research_stage_categories),
  "outcome_measures" = generate_funder_dummy_data(outcome_categories)
)

# Convert list to data frame
dummy_data <- as.data.frame(data) %>%
  mutate(discipline = gsub("\\(.*?\\)", "", discipline)) %>% 
  mutate(intervention = gsub("\\(.*?\\)", "", intervention)) 



random_uids <- dbReadTable(con, "study_classification") %>% 
  filter(decision == "include") %>% 
  select(uid) %>% 
  slice_sample(n = 10000, replace = TRUE)

dummy_data_for_funder <- cbind(random_uids, dummy_data) 

# with_doi <- citations_for_dl %>%
#   filter(!(doi == ""|is.na(doi)))
# 
# pdfs <- dbReadTable(con, "full_texts") %>%
#   filter(doi %in% with_doi$doi)
# 
# pdf_count <- pdfs %>%
#   count(status)

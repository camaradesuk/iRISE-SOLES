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

dataframes_for_app <- list()

# For citations download
unique_citations_for_dl <- tbl(con, "unique_citations") %>%
  collect()

unique_citations_for_db <- unique_citations_for_dl %>%
  mutate(year = as.numeric(year)) %>%
  filter(!(is.na(year) | year == ""))

dataframes_for_app[["unique_citations_for_db"]] <- unique_citations_for_db

dataframes_for_app[["unique_citations_for_dl"]] <- unique_citations_for_dl

pico <- data.frame(uid = character())

dataframes_for_app[["pico"]] <- pico

# Create folder for fst_files if it does not exist
fst_files_written <- 0
if (!file.exists("db_only_deploy_app/fst_files")) {
  dir.create("db_only_deploy_app/fst_files")
}

# Write all of the dataframes required to fst files
for (name in names(dataframes_for_app)) {
  dataframe <- dataframes_for_app[[name]]
  write_fst(dataframe, paste0("db_only_deploy_app/fst_files/", name, ".fst"))
  fst_files_written <- fst_files_written + 1
}


# Redeploy the app
app_deploy <- try({
  rsconnect::deployApp(
    appDir = "db_only_deploy_app",
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

# get file paths
wos_folder <- list.files("search2/wos/", pattern=".bib", full.names = T)
medline_folder <- list.files("search2/medline", full.names = T)
embase_folder <- list.files("search2/embase", full.names = T)
psychinfo_folder <- list.files("search2/psychinfo", full.names = T)
scopus_folder <- list.files("search2/scopus", full.names = T)


library(soles)

wos <- manual_upload(wos_folder, source = "wos")
medline <- manual_upload(medline_folder, source = "medline")
medline <- medline %>% unique()
scopus <- manual_upload(scopus_folder, source = "scopus")
scopus <- scopus %>% unique()
psychinfo <- manual_upload(psychinfo_folder, source = "psychinfo")
embase <- manual_upload(embase_folder, source = "embase")

# combine into one df
combined <- rbind(wos, medline, psychinfo, embase, scopus)

fst::write.fst(combined, "second_search_modified.fst")

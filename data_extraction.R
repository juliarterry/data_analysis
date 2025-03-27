#environment set up
rm(list=ls())
library(rgbif)
library(usethis)

#extract occurrences from API using 'rgbif' package
species <- c("Amanita muscaria", "Coprinus comatus")
#can quickly look occurences up using occ_search, but limited to 100,000 records
occ_search(scientificName = "Coprinus comatus", continent = "europe", year = 2008:2024)

#instead use gbif_download
usethis::edit_r_environ() 
#edit .Renviron to look like this:
  # GBIF_USER="jwaller"
  # GBIF_PWD="safe_fake_password_123"
  # GBIF_EMAIL="jwaller@gbif.org"
#will allow the download of occurrence data without need for entering log in details to GBIF

#set parameters for search
taxon_key <- name_backbone("Coprinus comatus")$usageKey
continent <- "europe"
year <- 2008 #all years >= 2008

#get preview to check query:
occ_download_prep(pred("taxonKey", taxon_key), 
                  pred("continent", continent),
                  pred_gte("year", year),
                  format = "SIMPLE_CSV")

#download occurrences
occ_download(
  pred("taxonKey", taxon_key), 
  pred("continent", continent),
  pred_gte("year", year),
  format = "SIMPLE_CSV")

occ_download_wait('0002934-250325103851331') #check status

d <- occ_download_get('0002934-250325103851331') %>%
  occ_download_import() #retrieve results

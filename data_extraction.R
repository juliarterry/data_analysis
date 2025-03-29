#environment set up
rm(list=ls())
library(rgbif)
library(usethis)
library(tidyverse)

#extract occurrences from API using 'rgbif' package
#can quickly look occurrences up using occ_search, but limited to 100,000 records
occ_search(scientificName = "Amanita muscaria", continent = "europe", year = 2024)

#for full data extraction use gbif_download
usethis::edit_r_environ() 
#edit .Renviron to look like this:
  # GBIF_USER="jsmith"
  # GBIF_PWD="safe_fake_password_123"
  # GBIF_EMAIL="jsmith@mail.com"
#will allow the download of occurrence data without need for entering log in details to GBIF

#set parameters for search
taxon_key <- name_backbone("Amanita muscaria")$usageKey
continent <- "europe"
year <- 2000 #all years >= 2000

#query GBIF database
occ_download(
  pred("taxonKey", taxon_key), 
  pred("continent", continent),
  pred_gte("year", year),
  pred_default(), #remove occurrences with: geospatial issues, no coordinates, absent records, fossils and living specimens
  format = "SIMPLE_CSV")

#check status
occ_download_wait('0005988-250325103851331') 

#retrieve results
raw_data <- occ_download_get('0005988-250325103851331') %>%
  occ_download_import() 

#preview results 
head(raw_data)

#clean data
clean_data <- raw_data %>%
  #filter out only results from iNaturalist
  filter(institutionCode == "iNaturalist") %>%
  #select only cols relevant for analysis
  select(c(decimalLatitude, decimalLongitude, eventDate)) 


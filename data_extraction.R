#environment set up
rm(list=ls())
library(rgbif)
library(usethis)
library(tidyverse)
library(lubridate)

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
  dplyr::select(c(decimalLatitude, decimalLongitude, eventDate, month, year, day)) %>%
  #include only years since iNaturalist was released and only including full years
  filter(year < 2025) %>%
  filter(2007 < year)

#count observations per year-month
monthly_counts <- clean_data %>%
  #count total obs for each month
  group_by(year, month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  #group by year and calculate total annual observations
  group_by(year) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  #add a new column which shows monthly observations as a proportion of annual
  mutate(prop = n/total)

#let's pop in a quick test to make sure nothing has gone skew-whiff! 
#the sum of all unique values of annual total should be equal to the number of observations
if(sum(unique(monthly_counts$total)) == nrow(clean_data)){
  print("all is well :)")
}else{
  print("oopsie woopsie")
}

#plot the data, grouping by year
ggplot(monthly_counts, aes(x = month, y = prop, color = factor(year))) +
  geom_line() +  
  geom_point() + 
  labs(title = "Observations per Month",
       x = "Date (Month)",
       y = "Proportion of Annual Observations",
       color = "Year") +
  theme_minimal()

#from a first glance, it does look like the fruiting time is shifting later as we hypothesized, but we are losing some details in grouping by month, so let's increase the granularity
daily_counts <- clean_data %>%
  #convert the date column into date format, removing the time and year
  mutate(date = as.Date(eventDate)) %>%
  mutate(date = format(date, "%m-%d")) %>%
  dplyr::select(!c(eventDate, month, day)) %>%
  #count total obs for each day
  group_by(year, date) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  #group by year and calculate total annual observations
  group_by(year) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  #add a new column which shows monthly observations as a proportion of annual
  mutate(prop = n/total)

#let's pop in a quick test to make sure nothing has gone skew-whiff! 
#the sum of all unique values of annual total should be equal to the number of observations
if(sum(unique(daily_counts$total)) == nrow(clean_data)){
  print("all is well :)")
}else{
  print("oopsie woopsie")
}

#specify date as date so it is treated as continuous in the plot
daily_counts$date <- as.Date(daily_counts$date, format = "%m-%d")
  
#plot the data, grouping by year
ggplot(daily_counts, aes(x = date, y = prop, color = factor(year))) +
  #geom_point() +
  geom_smooth() +  
  labs(title = "Annual Distribution of Observations",
       x = "Date",
       y = "Proportion of Annual Observations",
       color = "Year") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()  

#some of those early years are looking a bit funky - perhaps we should check how many observations they have

#lets do some stats and check out these results! 



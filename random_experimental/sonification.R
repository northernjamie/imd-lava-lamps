# data prep for the sonification of deprivation in English Local Authorities

## First attempt preparing the data for the web interface http://musicalgorithms.org/4.1/app/

### PCarve the 32,844 lsoas into 88 bins (one for each key on a piano)

library(tidyverse)

## get the 2019 indices data

iod_data <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices")

## Get the lookups

lu_la_region <- readr::read_csv("https://github.com/northernjamie/imd-lava-lamps/raw/master/geo_lookups/lookup_la_region_2019.csv")
lu_lsoa_la <- readr::read_csv("https://github.com/northernjamie/imd-lava-lamps/raw/master/geo_lookups/lookup_lsoa_la_2019.csv")
lu_lsoa_ward <- readr::read_csv("https://github.com/northernjamie/imd-lava-lamps/raw/master/geo_lookups/lookup_lsoa_ward_2018.csv")
lu_ward_pcon <- readr::read_csv("https://github.com/northernjamie/imd-lava-lamps/raw/master/geo_lookups/lookup_ward_westminster_constituency_2018.csv")



iod_data_lsoa_la <- iod_data %>%
  filter(Measurement == 'Rank') %>%
  rename(domain = 'Indices of Deprivation') %>%
  filter(domain == 'a. Index of Multiple Deprivation (IMD)') %>%
  left_join(lu_lsoa_la, by = c('FeatureCode' = 'LSOA11CD')) %>%
  left_join(lu_la_region, by = c('LAD19CD' = 'LAD19CD')) %>%
  left_join(lu_lsoa_ward, by = c('FeatureCode' = 'LSOA11CD')) %>%
  select('FeatureCode','LAD19CD','LAD19NM.x','Value','RGN19NM') %>%
  rename(LAD19NM = 'LAD19NM.x') %>%
  rename(LSOACD = 'FeatureCode') %>%
  mutate(pianotile = ntile(Value, 88)) %>%
  filter(LAD19NM == 'Manchester') %>%
  arrange(pianotile)

# outputs the pianotile column as a commma separated string

toString(iod_data_lsoa_la$pianotile)

# Code for Swirrl blogpost - getting the linked data programmatically using R
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
  mutate(vigintile = ntile(Value, 20)) %>%
  filter(RGN19NM == 'North West') 
  



lavalamp_chart <- ggplot(data = iod_data_lsoa_la, aes(LAD19NM, vigintile, fill = LAD19NM)) +
  geom_violin() +
  facet_wrap(~LAD19NM, strip.position = 'bottom', scales = 'free_x',ncol = 13) +
  theme(legend.position = 'none',
        plot.subtitle = element_text(color = '#b1b1b1', size = 18, face='italic'),
        plot.title = element_text(color = "#b1b1b1",size=36),
        strip.background = element_rect(fill = '#f7f1d4'),
        strip.text = element_text(color='#232323',size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(color="#232323",size=12),
        plot.background=element_rect(fill="#f7f1d4"),
        panel.background = element_rect(fill="#f7f1d4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

lavalamp_chart
  
  
iod_data_lsoa_la_trafford <- iod_data_lsoa_la %>%
  filter(LAD19NM == 'Trafford')



chart <- ggplot(data = iod_data_lsoa_la, aes(x = vigintile)) +
  geom_bar() + facet_wrap(~RGN19NM)

chart

chart_bar <- ggplot(data = iod_data_lsoa_la, aes(x = reorder(LSOACD,Value), y = Value)) +
  geom_bar(stat='identity') + facet_wrap(~RGN19NM)

chart_bar


## IMD Violin plots by parliamentary constituency

library(tidyverse)


### Get the geo lookups

lu_lsoa_la <- readr::read_csv("geo_lookups/lookup_lsoa_la_2019.csv")
lu_lsoa_ward <- readr::read_csv("geo_lookups/lookup_lsoa_ward_2018.csv")
lu_ward_pcon <- readr::read_csv("geo_lookups/lookup_ward_westminster_constituency_2018.csv")
lu_la_region <- readr::read_csv("geo_lookups/lookup_la_region_2019.csv")

imd_data_la_19 <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findicesbyla")

imd_data_lsoa_19 <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices")

pcon_19_elec <- readr::read_csv("parliamentary_constituencies/source/2019-results.csv")
pcon_mp_party_cabinet <- readr::read_csv("parliamentary_constituencies/source/pcon_mp_name_cabinet.csv")
write.csv(imd_data_lsoa_19, file="source_all/imd_source_lsoa_19.csv")
write.csv(imd_data_la_19, file="source_all/imd_source_la_19.csv")

palControl <- c(Lab = '#DC241f',
                LD = '#FAA61A',
                Con = '#0087DC',
                Green = '#69b044',
                Spk = '#aaaaaa',
                UKIP = '#70147A',
                NPC = '#333333',
                IND = 'pink')

iod_data_lsoa_pcon <- imd_data_lsoa_19 %>%
  filter(Measurement == 'Rank') %>%
  rename(domain = 'Indices of Deprivation') %>%
  filter(domain == 'a. Index of Multiple Deprivation (IMD)') %>%
  left_join(lu_lsoa_la, by = c('FeatureCode' = 'LSOA11CD')) %>%
  left_join(lu_la_region, by = c('LAD19CD' = 'LAD19CD')) %>%
  left_join(lu_lsoa_ward, by = c('FeatureCode' = 'LSOA11CD')) %>%
  left_join(lu_ward_pcon, by = c('WD18CD' = 'WD18CD')) %>%
  left_join(pcon_19_elec, by = c('PCON18CD' = 'ccode')) %>%
  left_join(pcon_mp_party_cabinet, by = c('PCON18CD' = 'PCON19CD')) %>%
  mutate(vigintile = ntile(Value, 20)) %>%
  select('FeatureCode','RGN19NM','PCON18NM','vigintile', 'first19','PCON18CD','Value','mp_name','title_short') %>%
  replace(., is.na(.), "") %>%
  mutate(mp_title_short_prep = ifelse(title_short != "",paste0(" (",title_short,")"),"")) %>%
  mutate(mp_name_title = paste(mp_name,mp_title_short_prep)) #%>%
  filter(RGN19NM == 'North West') # filter to one region to make it easier to test
  
  
  
  
iod_pcon_sort <- iod_data_lsoa_pcon %>% 
  group_by(PCON18CD) %>%
  summarise(averagerank = mean(Value)) %>% 
  left_join(iod_data_lsoa_pcon, by = c('PCON18CD' = 'PCON18CD')) %>%
  mutate(PCON18NM_sort = fct_reorder(PCON18NM,averagerank, .desc = T))

  eng_imd_pcon_pol_lava <- ggplot(iod_pcon_sort, aes(PCON18NM_sort, vigintile, fill = first19)) + 
    facet_wrap(~ PCON18NM_sort + mp_name_title, strip.position = 'bottom', scales = 'free_x',ncol = 22) +
    geom_violin(linetype = 0) +
    scale_fill_manual(values = palControl) + 
    theme(legend.position = 'none',
          plot.subtitle = element_text(color = '#b1b1b1', size = 18, face='italic'),
          plot.title = element_text(color = "#b1b1b1",size=36),
          #strip.background = element_rect(fill = '#f7f1d4'),
          strip.background = element_blank(),
          strip.text = element_text(color='#232323',size=6),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x=element_blank(),
          axis.title = element_text(color="#232323",size=8),
          #plot.background=element_rect(fill="#f7f1d4"),
          #panel.background = element_rect(fill="#f7f1d4"),
          plot.background=element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

eng_imd_pcon_pol_lava

ggsave(paste0("parliamentary_constituencies/outputs/nw2019IMDsort.svg"),eng_imd_pcon_pol_lava,width=38,height=52,units="cm")




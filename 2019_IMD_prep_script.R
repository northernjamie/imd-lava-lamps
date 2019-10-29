# IMD lava lamps for 2019 with new data

library(tidyverse)
library(googlesheets)
## Political control for councils

### In this google sheet: https://docs.google.com/spreadsheets/d/1vkDwmBKL9P7m99oQXdieJzLFDna2XIhOFDWQ5gVoXq8/edit#gid=0

pol_control <- gs_key("1vkDwmBKL9P7m99oQXdieJzLFDna2XIhOFDWQ5gVoXq8")
pol_control <- gs_read(pol_control)
pol_control <- pol_control %>%
  select('ONS_LA_code','2019')

## Get the lookups in

lu_lsoa_la <- readr::read_csv("data/lookup_lsoa_la_2019.csv")
lu_lsoa_ward <- readr::read_csv("data/lookup_lsoa_ward_2018.csv")
lu_ward_pcon <- readr::read_csv("data/lookup_ward_westminster_constituency_2018.csv")
lu_la_region <- readr::read_csv("data/lookup_la_region_2019.csv")

## Get the IMD data

### use the 2015 IMD data for now - 2019 is the same shape - just need to change the source file

### IMD data at LSOA level

imd_data_lsoa <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices")

### IMD data at LA level rank of average rank

imd_data_la <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findicesbyla")

iod_domain <- "g. Barriers to Housing and Services Domain"                 
iod_domain <- "h. Living Environment Deprivation Domain"                   
iod_domain <- "f. Crime Domain"                                            
iod_domain <- "j. Income Deprivation Affecting Older People Index (IDAOPI)"
iod_domain <- "i. Income Deprivation Affecting Children Index (IDACI)"     
iod_domain <- "b. Income Deprivation Domain"                               
iod_domain <- "c. Employment Deprivation Domain"                           
iod_domain <- "a. Index of Multiple Deprivation (IMD)"                     
iod_domain <- "e. Health Deprivation and Disability Domain"                
iod_domain <- "d. Education, Skills and Training Domain"

## match the LSOA data to the LA data
imd_data_lsoa_la <- imd_data_lsoa %>%
  filter(Measurement == 'Rank') %>%
  rename(domain = 'Indices of Deprivation') %>%
  filter(domain == iod_domain) %>%
  left_join(lu_lsoa_la, by = c('FeatureCode' = 'LSOA11CD')) %>%
  left_join(imd_data_la, by =  c('LAD19CD' = 'FeatureCode')) %>%
  rename(la_domain = 'Indices of Deprivation') %>%
  filter(la_domain == 'a. Index of Multiple Deprivation (IMD)') %>%
  filter(Measurement.y == 'Rank of average rank') %>%
  rename('Rank_of_avg_rank' = 'Value.y') %>%
  left_join(pol_control, by = c('LAD19CD' = 'ONS_LA_code')) %>%
  rename(pol_control_2019 = '2019') %>%
  left_join(lu_la_region, by = c('LAD19CD' = 'LAD19CD')) %>%
  left_join(lu_lsoa_ward, by = c('FeatureCode' = 'LSOA11CD')) %>%
  mutate(vigintile = ntile(Value.x, 20)) %>%
  filter(LAD19CD != "E06000053") %>%
  select('FeatureCode','LAD19CD','LAD19NM.x','vigintile','pol_control_2019','Rank_of_avg_rank')
  

#left_join(lu_ward_pcon, by = c('WD18CD' = 'WD18CD'))  




### when doing this with the real data there should be 32,844 lsoas
### There are duplicates eg lsoa E01000330. These seem to arise from the ward
### to pcon lookup, where some wards straddle more than one consituency
### Best thing to do is to maybe ignore pcon for the general data munging
### and merge it in for that bit only.

## set the colors for the parties
palControl <- c(LAB = '#DC241f',
                LD = '#FAA61A',
                CON = '#0087DC',
                GREEN = '#69b044',
                NOC = '#aaaaaa',
                UKIP = '#70147A',
                OTHER = 'pink',
                NPC = '#333333',
                IND = 'pink')


## add the sort factor
imd_data_lsoa_la$LAD19NM_IMD <- reorder(imd_data_lsoa_la$LAD19NM.x,imd_data_lsoa_la$Rank_of_avg_rank) 

## Make the lava lamp plot
eng_imd_pol_lava <- ggplot(imd_data_lsoa_la, aes(LAD19NM_IMD, vigintile, fill = pol_control_2019)) + 
  facet_wrap(~ LAD19NM_IMD, strip.position = 'bottom', scales = 'free_x',ncol = 16) +
  ggtitle(iod_domain) + 
  geom_violin(linetype = 0) +
  scale_fill_manual(values = palControl) + 
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

ggsave(paste0("eng2019imdsort",substr(iod_domain,1,1),".png"),eng_imd_pol_lava,width=24,height=24.15,units="in")
#ggsave("engimdsortIMDfillpolitic2019.svg",eng_imd_pol_lava,width=24,height=24.15,units="in")

test_violin_data <- imd_data_lsoa_la %>%
  filter(LAD19NM.x == 'Hackney')

test_violin_plot <- ggplot(test_violin_data, aes(LAD19NM_IMD, vigintile, fill = pol_control_2019)) + 
  facet_wrap(~ LAD19NM_IMD, strip.position = 'bottom', scales = 'free_x',ncol = 16) +
  geom_violin(color = '#f7f1d4') +
  scale_fill_manual(values = palControl)

test_violin_plot

## function to sort the IMD data 

tidyIMD <- function (year,iod_domain) {
  options(warn=-1)
  pol_control <- gs_key("1vkDwmBKL9P7m99oQXdieJzLFDna2XIhOFDWQ5gVoXq8")
  pol_control <- gs_read(pol_control)
  pol_control <- pol_control %>%
    select('ONS_LA_code','2019')
  
  lu_lsoa_la <- readr::read_csv("geo_lookups/lookup_lsoa_la_2019.csv")
  lu_lsoa_ward <- readr::read_csv("geo_lookups/lookup_lsoa_ward_2018.csv")
  lu_ward_pcon <- readr::read_csv("geo_lookups/lookup_ward_westminster_constituency_2018.csv")
  lu_la_region <- readr::read_csv("geo_lookups/lookup_la_region_2019.csv")
  
  imd_data_la_19 <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findicesbyla")
  
  if(year == "2015") {
    #iod_data <- readr::read_csv("https://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd%2Findices")
    iod_data <- readr::read_csv("data/IMD_PMD_2015_LSOA_ENG.csv")
  } else if (year == "2019") {
    iod_data <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices")
    
  }
  
  iod_data_lsoa_la <- iod_data %>%
    filter(Measurement == 'Rank') %>%
    rename(domain = 'Indices of Deprivation') %>%
    filter(domain == iod_domain) %>%
    left_join(lu_lsoa_la, by = c('FeatureCode' = 'LSOA11CD')) %>%
    left_join(imd_data_la_19, by =  c('LAD19CD' = 'FeatureCode')) %>%
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
    select('FeatureCode','LAD19CD','LAD19NM.x','vigintile','pol_control_2019','Rank_of_avg_rank','la_domain')
  options(warn=0)
  return (iod_data_lsoa_la)
}

makeLavaLamps <- function(iod_data,filename) {
  
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
  iod_data$LAD19NM_IMD <- reorder(iod_data$LAD19NM.x,iod_data$Rank_of_avg_rank) 
  
  eng_imd_pol_lava <- ggplot(iod_data, aes(LAD19NM_IMD, vigintile, fill = pol_control_2019)) + 
    facet_wrap(~ LAD19NM_IMD, strip.position = 'bottom', scales = 'free_x',ncol = 16) +
    ggtitle(iod_data$la_domain[1]) + 
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
  
  #ggsave(paste0(filename,".png"),eng_imd_pol_lava,width=24,height=24.15,units="in")
  #ggsave(paste0(filename,".svg"),eng_imd_pol_lava,width=24,height=24.15,units="in")  
  ggsave(paste0(filename,".png"),eng_imd_pol_lava,width=3,height=3,units="in")
  ggsave(paste0(filename,".svg"),eng_imd_pol_lava,width=3,height=3,units="in")  
  
}
  
  
  
  
  


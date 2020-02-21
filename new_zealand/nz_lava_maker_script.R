## get and process the New Zealand Data

#libraries

library(tidyverse)

nz_imd <- readr::read_csv("new_zealand/source/NZIMD2013.csv")
nz_political <- readr::read_csv("new_zealand/source/NZ_GED_2017_POL_PARTY.csv")

nz_imd_vigintile <- nz_imd %>%
  mutate(vigintile = ntile(-IMDRank, 20)) %>%
  select(IMDRank,vigintile,GED2014Name)

nz_imd_sort_avg_rank <- nz_imd_vigintile %>%
  group_by(GED2014Name) %>%
  summarise(averagerank = mean(IMDRank)) %>%
  left_join(nz_imd_vigintile, by=c('GED2014Name' = 'GED2014Name')) %>%
  arrange(averagerank) %>%
  mutate(GED2014Name_sort = fct_reorder(GED2014Name,averagerank, .desc = T)) %>%
  left_join(nz_political, by = c('GED2014Name' = 'GED'))
  

nz_imd_sort_avg_rank$GED2014Name_sort <- reorder(nz_imd_sort_avg_rank$GED2014Name,nz_imd_sort_avg_rank$averagerank) 

nz_palControl <- c(Labour = '#D82A20',
                National = '#00529F',
                ACT = '#FDE401')

nz_imd_ll_plot <- ggplot(data = nz_imd_sort_avg_rank, aes(GED2014Name_sort, vigintile, fill = Party)) + 
  facet_wrap(~ GED2014Name_sort, strip.position = 'bottom', scales = 'free_x',ncol = 8) +
  ggtitle("NZ IMD") + 
  scale_fill_manual(values = nz_palControl) + 
  geom_violin(linetype = 0) + 
  theme(legend.position = 'none',
        plot.subtitle = element_text(color = '#b1b1b1', size = 18, face='italic'),
        plot.title = element_text(color = "#b1b1b1",size=36),
        strip.background = element_rect(fill = '#d9d9d9'),
        strip.text = element_text(color='#232323',size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(color="#232323",size=12),
        plot.background=element_rect(fill="#d9d9d9"),
        panel.background = element_rect(fill="#d9d9d9"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

nz_imd_ll_plot

ggsave(paste0("new_zealand/outputs/nz2013IMDsort.svg"),nz_imd_ll_plot,width=26,height=30,units="cm")


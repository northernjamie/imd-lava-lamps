## get and process the New Zealand Data

#libraries

library(tidyverse)

nz_imd <- readr::read_csv("new_zealand/source/NZIMD2013.csv")

nz_imd <- nz_imd %>%
  mutate(vigintile = ntile(-IMDRank, 20)) %>%
  select(IMDRank,vigintile,GED2014Name)

nz_imd_ll_plot <- ggplot(nz_imd, aes(GED2014Name, vigintile, fill = GED2014Name)) + 
  facet_wrap(~ GED2014Name, strip.position = 'bottom', scales = 'free_x',ncol = 8) +
  ggtitle("NZ IMD") + 
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

ggsave(paste0("new_zealand/outputs/nz2013alphasort.svg"),nz_imd_ll_plot,width=30,height=37.5,units="cm")


# Make the lava lamps for Wales IMD

library(tidyverse)

# get the data

wimd <- readr::read_csv("wales/source/wales_imd_2019_prepped.csv")

wimd_vigintile <- wimd %>% 
  mutate(vigintile = ntile(WIMD, 20)) %>%
  filter(!is.na(LAName))

wales_imd_la_pol_lava <- ggplot(wimd_vigintile, aes(LAName, vigintile, fill = '#5c5e66')) + 
  facet_wrap(~ LAName, strip.position = 'bottom', scales = 'free_x',ncol = 6) + 
  geom_violin(linetype = 0) +
  #scale_fill_manual(values = palControl) + 
  theme(legend.position = 'none',
        plot.subtitle = element_text(color = '#b1b1b1', size = 18, face='italic'),
        plot.title = element_text(color = "#b1b1b1",size=36),
        strip.background = element_rect(fill = '#cfdee2'),
        strip.text = element_text(color='#232323',size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(color="#232323",size=12),
        plot.background=element_rect(fill="#cfdee2"),
        panel.background = element_rect(fill="#cfdee2"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

wales_imd_la_pol_lava

ggsave(paste0("wales/outputs/2019WIMD.svg"),wales_imd_la_pol_lava,width=26,height=30,units="cm")





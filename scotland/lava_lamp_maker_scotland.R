# Scotland Lava Lamps from 2020 SIMD

# Get the data from the folder - data has already been got from statistics.scot.gov via a sparql query
# it's a sparql query because it;s the only way I can work out 
# how to get the LA and scottish parliamentary constituency 

library (tidyverse)

simd <- readr::read_csv("scotland/SIMD_2020_raw.csv")

simd_pol_lava <- ggplot(simd, aes(lalabel, vigintile, fill = lalabel)) + 
  facet_wrap(~ lalabel, strip.position = 'bottom', scales = 'free_x',ncol = 8) +
  ggtitle("title") + 
  geom_violin(linetype = 0) +
  #scale_fill_manual(values = palControl) + 
  theme(legend.position = 'none',
        #plot.subtitle = element_text(color = '#b1b1b1', size = 18, face='italic'),
        #plot.title = element_text(color = "#b1b1b1",size=36),
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
simd_pol_lava

ggsave("scotland/simd_2020.svg",simd_pol_lava,width=24,height=24.15,units="in")

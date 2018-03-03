library(ggplot2)
library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)


## Get lsoas with imd rank and local authority

lsoa_imd <- read.csv("~/Projects/Propolis_Stuff/R_Projects/LA_IMD_Violin/data/IMD_LSOA_2015.csv")
la_imd <- read.csv("~/Projects/Propolis_Stuff/R_Projects/LA_IMD_Violin/data/IMD_LA_2015.csv")
LSOA_LA_Lookup <- read.csv("~/Projects/Propolis_Stuff/R_Projects/LA_IMD_Violin/data/OA_LSOA_LA_Lookup.csv")

#Need to get rid of unnnecessary columns

lsoa_imd_rank <- lsoa_imd[which(lsoa_imd$Measurement=='Rank'), ]
lsoa_imd_rank <- lsoa_imd_rank[which(lsoa_imd_rank$Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)'), ]
lsoa_imd_rank_min <- data.frame('geocode' = lsoa_imd_rank$GeographyCode,
                                'rank' = lsoa_imd_rank$Value)

la_imd_rank <- la_imd[which(la_imd$Measurement=='Rank of average rank'), ]
la_imd_rank <- la_imd_rank[which(la_imd_rank$Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)'), ]
la_imd_rank_min <- data.frame('geocode' = la_imd_rank$GeographyCode,
                                'rank_of_avg_rank' = la_imd_rank$Value)

LSOA_LA_Lookup_min <- data.frame('lsoa' = LSOA_LA_Lookup$LSOA11CD,
                                 'la' = LSOA_LA_Lookup$LAD16CD,
                                 'la_name' = LSOA_LA_Lookup$LAD16NM)

lsoa_la_imd <- merge(lsoa_imd_rank_min,LSOA_LA_Lookup_min,by.x="geocode", by.y="lsoa", all.x=TRUE)
lsoa_la_imd_full <- merge(lsoa_la_imd,la_imd_rank_min, by.x="la",by.y="geocode", all.x=TRUE)

lsoa_la_imd_dedup <- unique(lsoa_la_imd_full)

lsoa_la_imd_vingtile <- lsoa_la_imd_dedup %>%
  mutate(vingtile = ntile(rank, 20))

lsoa_la_imd_vingtile_noscilly <- lsoa_la_imd_vingtile[which(lsoa_la_imd_vingtile$la != "E06000053"), ]

g <- ggplot(lsoa_la_imd_vingtile_noscilly, aes(la_name, vingtile, fill=la_name))
g <- g + facet_wrap(~la_name, strip.position = 'bottom', scales = 'free_x',ncol = 15)
g <- g + geom_violin(color="white") + ggtitle("England Local Authority Deprivation Profiles",subtitle="These charts show the distribution of Lower Super Output Areas by deprivation in GM local authorities. \nA fatter bottom indicates proportionally more more-deprived LSOAs. A fatter head indicate more less-deprived LSOAs. \n\n@northernjamie ") +
  #labs(title="Greater Manchester Local Authority Deprivation Profiles",subtitle="These charts show the distribution of Lower Super Output Areas by deprivation in GM local authorities. /n A fatter bottom indicates more more-deprived LSOAs. A fatter head indicate more less-deprived LSOAs. @northernjamie ") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=12),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
g
ggsave("laimd.png",g,width=24,height=36,units="in")
gmla <- lsoa_la_imd_vingtile[which(lsoa_la_imd_vingtile$la_name %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")),]

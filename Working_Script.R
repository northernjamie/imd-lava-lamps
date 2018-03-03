library(ggplot2)
library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)


## Get the data

lsoa_imd <- read.csv("~/Projects/Propolis_Stuff/R_Projects/LA_IMD_Violin/data/IMD_LSOA_2015.csv")
la_imd <- read.csv("~/Projects/Propolis_Stuff/R_Projects/LA_IMD_Violin/data/IMD_LA_2015.csv")
LSOA_LA_Lookup <- read.csv("~/Projects/Propolis_Stuff/R_Projects/LA_IMD_Violin/data/OA_LSOA_LA_Lookup.csv")

# Filter the big LSOA data set to get rid of unwanted values (decile, score, and various domains of the IMD)

lsoa_imd_rank <- lsoa_imd[which(lsoa_imd$Measurement=='Rank'), ]
lsoa_imd_rank <- lsoa_imd_rank[which(lsoa_imd_rank$Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)'), ]
lsoa_imd_rank_min <- data.frame('geocode' = lsoa_imd_rank$GeographyCode,
                                'rank' = lsoa_imd_rank$Value)

# Filter the local authority table to keep only the index of multiple deprivation and the vale for rank of average ranks
la_imd_rank <- la_imd[which(la_imd$Measurement=='Rank of average rank'), ]
la_imd_rank <- la_imd_rank[which(la_imd_rank$Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)'), ]
la_imd_rank_min <- data.frame('geocode' = la_imd_rank$GeographyCode,
                                'rank_of_avg_rank' = la_imd_rank$Value)

# Get only the columns from the lookup file that we want to use (makes it faster) 

LSOA_LA_Lookup_min <- data.frame('lsoa' = LSOA_LA_Lookup$LSOA11CD,
                                 'la' = LSOA_LA_Lookup$LAD16CD,
                                 'la_name' = LSOA_LA_Lookup$LAD16NM)

# Perform the lookup, getting the Local authority for each LSOA
lsoa_la_imd <- merge(lsoa_imd_rank_min,LSOA_LA_Lookup_min,by.x="geocode", by.y="lsoa", all.x=TRUE)

# Get the local authority IMD rank of average ranks score into the LSOA table
lsoa_la_imd_full <- merge(lsoa_la_imd,la_imd_rank_min, by.x="la",by.y="geocode", all.x=TRUE)

# Remove duplicate rows
lsoa_la_imd_dedup <- unique(lsoa_la_imd_full)

# Add a new column, splitting the IMD rank into vingtiles
lsoa_la_imd_vingtile <- lsoa_la_imd_dedup %>%
  mutate(vingtile = ntile(rank, 20))

# Remve the scilly isles, because there's only 1 LSOA there, and it messes everything up
lsoa_la_imd_vingtile_noscilly <- lsoa_la_imd_vingtile[which(lsoa_la_imd_vingtile$la != "E06000053"), ]

# Draw the plot
g <- ggplot(lsoa_la_imd_vingtile_noscilly, aes(la_name, vingtile, fill=la_name))
g <- g + facet_wrap(~la_name, strip.position = 'bottom', scales = 'free_x',ncol = 15)
g <- g + geom_violin(color="white") + ggtitle("England Local Authority Deprivation Profiles",subtitle="These charts show the distribution of Lower Super Output Areas by deprivation in GM local authorities. \nA fatter bottom indicates proportionally more more-deprived LSOAs. A fatter head indicate more less-deprived LSOAs. \n\n@northernjamie ") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=12),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))

# Save the plot to png file, A1 size
ggsave("laimd.png",g,width=24,height=36,units="in")

# Filter the dataset to show only LAs in Greater Manchester (this can then be used in place of 'lsoa_la_imd_vingtile_noscilly' in the plot above)
gmla <- lsoa_la_imd_vingtile[which(lsoa_la_imd_vingtile$la_name %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")),]

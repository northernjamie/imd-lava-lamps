library(ggplot2)
library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)
library(geofacet)

### ENGLAND
## Get the data


lsoa_imd <- read.csv("./data/IMD_LSOA_2015.csv")
la_imd <- read.csv("./data/IMD_LA_2015.csv")
LSOA_LA_Lookup <- read.csv("./data/OA_LSOA_LA_Lookup.csv")


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

# Number of plots per row
pperrow <- 16


# Draw the plot
e <- ggplot(lsoa_la_imd_vingtile_noscilly, aes(la_name, vingtile, fill=la_name))
e <- e + facet_wrap(~la_name, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
e <- e + geom_violin(color="white") + ggtitle("England",subtitle="English Index of Multiple Deprivation 2015") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=12),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))

# Save the plot to png file, A1 size
ggsave("laimd.png",g,width=24,height=36,units="in")

# Filter the dataset to show only LAs in Greater Manchester (this can then be used in place of 'lsoa_la_imd_vingtile_noscilly' in the plot above)
gmla <- lsoa_la_imd_vingtile[which(lsoa_la_imd_vingtile$la_name %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")),]

## London geographically
# Not strictly necessary to filter to only areas included in the grid but 
# it avoids a warning message
l <- lsoa_la_imd_vingtile_noscilly %>% 
  filter(la %in% london_boroughs_grid$code_ons) %>% 
  ggplot(aes(1, vingtile, fill=la)) +
  geom_violin(color="white") + 
  ggtitle("London Local Authority Deprivation Profiles",subtitle="These charts show the distribution of Lower Super Output Areas by deprivation in London. \nA fatter bottom indicates proportionally more more-deprived LSOAs. A fatter head indicate more less-deprived LSOAs. \n\n@northernjamie @DaveMawdsley") +
  facet_geo(~la, grid=london_boroughs_grid, label="name") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=12),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))

ggsave("london.png",l,width=18,height=18,units="in")

### Wales

## Get the data


walesimd <- read.csv("./data/YUF_IMD_Wales_2014_WLSOA.csv")
waleslsoa_la <- read.csv("./data/wales_LSOA_LA_Lookup.csv")

wlsoa_la_imd <- merge(walesimd,waleslsoa_la,by.x="GSSCode", by.y="LSOA.Code", all.x=TRUE)
wlsoa_la_imd <- data.frame("lsoa" = wlsoa_la_imd$GSSCode,
                           "rank" = wlsoa_la_imd$WIMDRank,
                           "la_name" = wlsoa_la_imd$LA.name..in.English.,
                           "la_code" = wlsoa_la_imd$LA.Code)

# Add a new column, splitting the IMD rank into vingtiles
wlsoa_la_imd_vingtile <- wlsoa_la_imd %>%
  mutate(vingtile = ntile(rank, 20))

# Make the plot
w <- ggplot(wlsoa_la_imd_vingtile, aes(la_name, vingtile, fill=la_name))
w <- w + facet_wrap(~la_name, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
w <- w + geom_violin(color="white") + ggtitle("Wales",subtitle="Welsh Index of Multiple Deprivation 2014") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 10, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
w

##################

### Scotland

scotland_imd <- read.csv("./data/SIMD_DZ_LA.csv")

s_imd_vingtile <- scotland_imd %>%
  mutate(vingtile = ntile(rank, 20))
  
s <- ggplot(s_imd_vingtile, aes(lalabel, vingtile, fill=lalabel))
s <- s + facet_wrap(~lalabel, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
s <- s + geom_violin(color="white") + ggtitle("Scotland",subtitle="Scottish Index of Multiple Deprivation 2016") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 10, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
s

### Northern Ireland

niimd <- read.csv("./data/NI_IMD_LGD_LSOA.csv")
ni_imd_vingtile <- niimd %>%
  mutate(vingtile = ntile(rank,20))

ni <- ggplot(ni_imd_vingtile, aes(LGD2014NAME, vingtile, fill=LGD2014NAME))
ni <- ni + facet_wrap(~LGD2014NAME, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
ni <- ni + geom_violin(color="white") + ggtitle("Northern Ireland",subtitle="Northern Ireland Multiple Deprivation Measure 2017") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 10, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=6),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
ni

# Make the big poster

# 26 rows of charts 30 inches = 1.15 in per row
## England 326 charts 21 rows
## Wales 22 charts 2 rows
## Scotland 32 charts 2 rows
## NIreland 11 charts 1 row
# 4 Country titles 4 Inches
# 1 big title - 2 Inches
# 36 Inches

# Save the plot to png file, A1 size
ggsave("engimd.png",e,width=24,height=24.15,units="in")
ggsave("walesimd.png",w,width=24,height=3,units="in")
ggsave("scotlandimd.png",s,width=24,height=4,units="in")
ggsave("nirelandimd.png",ni,width=17.6,height=2,units="in")


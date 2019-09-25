library(ggplot2)
library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)
library(geofacet)
library(tidyr)

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
  
# Prep the LSOA -> Ward -> Parl con lookup

lsoa_ward <- read.csv("data/GEO_LSOA_WARD_LOOKUP.csv")
ward_pcon <- read.csv("data/GEO_LOOKUP_WD15_PCON15.csv")

lsoa_pcon <- merge(lsoa_ward,ward_pcon,by = 'WD15CD')
lsoa_pcon_imd <- merge(lsoa_pcon,lsoa_imd_rank_min,by.x='LSOA11CD',by.y='geocode', all.x=T)

lsoa_pcon_imd_vingtile <- lsoa_pcon_imd %>%
  mutate(vingtile = ntile(rank, 20))

lsoa_pcon_imd_vingtile <- lsoa_pcon_imd_vingtile %>% drop_na()

e <- ggplot(lsoa_pcon_imd_vingtile, aes(PCON15NM, vingtile, fill = PCON15NM))
e <- e + facet_wrap(~ PCON15NM, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
e <- e + geom_violin(color="white") + ggtitle("England",subtitle="English Index of Multiple Deprivation 2015.  Parliamentary Constituencies. \n@northernjamie \nData: MHCLG @ http://opendatacommunities.org") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
e
ggsave("engimdsortIMDpcon2.png",e,width=24,height=24.15,units="in")

# Match the England LA dataframe to Latitude

eng_la_centroids <- read.csv("data/eng_la_centroids.csv")
eng_la_centroids <- data.frame('la_code' = eng_la_centroids$CODE,
                               'la_latitude' = eng_la_centroids$Y)

lsoa_la_imd_vingtile_noscilly <- merge(lsoa_la_imd_vingtile_noscilly,eng_la_centroids,by.x = 'la', by.y='la_code', all.x= T)



## England sorted by IMD Filled by Political Control

# Match the England LA dataframe to political control

eng_la_control <- read.csv("data/la_pol_control_2017.csv")

lsoa_la_imd_vingtile_noscilly <- merge(lsoa_la_imd_vingtile_noscilly,eng_la_control,by.x = 'la',by.y='la_code',all.x=T)
lsoa_la_imd_vingtile_noscilly <- subset(lsoa_la_imd_vingtile_noscilly, select=c(1:9))
# Allerdale missing from LGiU dataset so forcing it in.
lsoa_la_imd_vingtile_noscilly[lsoa_la_imd_vingtile_noscilly$la_name=='Allerdale',10] <- 'NOC'

# palette for political control

palControl <- c(LAB = '#DC241f',LD = '#FAA61A',CON = '#0087DC',NOC = '#aaaaaa', UKIP = '#70147A', OTHER = 'pink', NPC = '#333333')

# Draw the plot England Sorted by IMD coloured by political control
e <- ggplot(lsoa_la_imd_vingtile_noscilly, aes(la_name, vingtile, fill = la_control_2017))
e <- e + facet_wrap(~ la_name_order_IMD, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
e <- e + geom_violin(color = 'white') + ggtitle("England Local Authorities: Deprivation Profile vs Political Control",subtitle="English Index of Multiple Deprivation 2015. Local Authority areas sorted according to the deprivation of the area, ie top left is most deprived, bottom right least deprived. \nShapes show the deprivation profile of each area - wider bottom indicates more more-deprived LSOAs, wider top indicates more less-deprived LSOAs. \n@northernjamie \nData: MHCLG @ http://opendatacommunities.org & LGiU @ https://www.lgiu.org.uk/local-government-facts-and-figures/") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=36),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="bottom",legend.title=element_blank(),legend.background=element_rect(fill='#000000'),legend.key.size=unit(1,"cm"),legend.text = element_text(color='white',size=24),axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
e <- e + scale_fill_manual(values = palControl)
e
ggsave("engimdsortIMDfillpolitic3.svg",e,width=24,height=24.15,units="in")

# Add the factor sorted by LA dep rank
lsoa_la_imd_vingtile_noscilly$la_name_order_IMD <- reorder(lsoa_la_imd_vingtile_noscilly$la_name,lsoa_la_imd_vingtile_noscilly$rank_of_avg_rank) 

# Add the factor sorted by LA latitude
lsoa_la_imd_vingtile_noscilly$la_name_order_lat <- reorder(lsoa_la_imd_vingtile_noscilly$la_name,-lsoa_la_imd_vingtile_noscilly$la_latitude) 

# Draw the plot
e <- ggplot(lsoa_la_imd_vingtile_noscilly, aes(la_name, vingtile, fill = la_control_2017))
e <- e + facet_wrap(~ la_name_order_IMD, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
e <- e + geom_violin(color="white") + ggtitle("England",subtitle="English Index of Multiple Deprivation 2015. Local Authority areas sorted according to the latitude of the area, ie top left is most northern, bottom right most southern. \n@northernjamie \nData: MHCLG @ http://opendatacommunities.org") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 18, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))

ggsave("engimdsortIMDfillpolitic.png",e,width=24,height=24.15,units="in")
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

# Reorder the factor levels (local auth names) so that they 
wlsoa_la_imd_vingtile$la_name2 <- reorder(wlsoa_la_imd_vingtile$la_name,wlsoa_la_imd_vingtile$rank) 

w <- ggplot(wlsoa_la_imd_vingtile, aes(la_name, vingtile))
w <- w + facet_wrap(~la_name2, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
w <- w + geom_violin(color="white",fill="#ff0000") + ggtitle("Wales",subtitle="Welsh Index of Multiple Deprivation 2014") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 10, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
w

##################

### Scotland

pperrow <- 8
scotland_imd <- read.csv("./data/SIMD_DZ_LA.csv")

s_imd_vingtile <- scotland_imd %>%
  mutate(vingtile = ntile(rank, 20))

# make the coloured scotland viz ready to be cleaned up in illustrator
s <- ggplot(s_imd_vingtile, aes(lalabel, vingtile,fill = lalabel))
s <- s + facet_wrap(~lalabel, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
s <- s + geom_violin(color="white", scale="count") + ggtitle("Scotland",subtitle="Scottish Index of Multiple Deprivation 2016") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'white', size = 10, face='italic'),plot.title = element_text(color = "white",size=26),strip.background = element_rect(fill = 'black'),strip.text = element_text(color='white',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="white",size=12),plot.background=element_rect(fill="black"),panel.background = element_rect(fill="black"))
s
ggsave("scothighres.svg",s,width=10,height=10,units="in")
# ,fill="#244eaf"
### Northern Ireland

niimd <- read.csv("./data/NI_IMD_LGD_LSOA.csv")
ni_imd_vingtile <- niimd %>%
  mutate(vingtile = ntile(rank,20))

ni <- ggplot(ni_imd_vingtile, aes(LGD2014NAME, vingtile))
ni <- ni + facet_wrap(~LGD2014NAME, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
ni <- ni + geom_violin(color="white",fill="#40ae49") + ggtitle("Northern Ireland",subtitle="Northern Ireland Multiple Deprivation Measure 2017") +
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
ggsave("engimdsortlatitude.png",e,width=24,height=24.15,units="in")
ggsave("walesimd.png",w,width=24,height=3,units="in")
ggsave("scotlandimd.png",s,width=24,height=4,units="in")
ggsave("nirelandimd.png",ni,width=16.8,height=2,units="in")

# ad hoc request

## Colin Birchenall - what does the glasgow city region look like:
## Glasgow, East Ren, Renfrewshire, North Lan, South Lan, West Dun, East Dun and Inverclyde

## Add a column to a temporary dataframe of scotland, to apply GCR flag to dzs

gcr_df <- s_imd_vingtile

gcr_las <- c("Glasgow City","East Renfrewshire","Renfrewshire","North Lanarkshire","South Lanarkshire","West Dunbartonshire","East Dunbartonshire","Inverclyde")
dfcol <- c("Glasgow City Region","Glasgow City Region","Glasgow City Region","Glasgow City Region","Glasgow City Region","Glasgow City Region","Glasgow City Region","Glasgow City Region")
gcr_lu <- data.frame(gcr_las,dfcol)

gcr_df <- merge(s_imd_vingtile,gcr_lu,by.x="lalabel",by.y="gcr_las",all.x=T)
s_gcr <- ggplot(gcr_df, aes(dfcol, vingtile,fill = dfcol))
s_gcr <- s_gcr + facet_wrap(~dfcol, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
s_gcr <- s_gcr + geom_violin(color="#d9d9d9", scale="count") + ggtitle("Scotland",subtitle="Scottish Index of Multiple Deprivation 2016") +
  theme_classic() + theme(plot.subtitle = element_text(color = 'black', size = 10, face='italic'),plot.title = element_text(color = "black",size=26),strip.background = element_rect(fill = '#d9d9d9'),strip.text = element_text(color='black',size=8),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",axis.title = element_text(color="black",size=12),plot.background=element_rect(fill="#d9d9d9"),panel.background = element_rect(fill="#d9d9d9"))
s_gcr
# End ad hoc request from Colin Birchenall
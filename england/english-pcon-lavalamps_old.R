# Make english parliamentary constituencies plot

## Get the regions into the table to produce regional lava lamp plots

### Get the LA - region lookup

lu_la_region <- read.csv("lookup_LA_region.csv")

lsoa_pcon_imd_vingtile_region <- merge(lsoa_pcon_imd_vingtile,lu_la_region,by.x = 'LAD15CD.x',by.y='LAD17CD',all.x=T)

### Match the pcon table with party and MP

pcon_party_mp <- read.csv("pcon_party_mp.csv")
pcon_party_mp$constituencyGroupOnsCode <- trimws(pcon_party_mp$constituencyGroupOnsCode)
pcon_party_mp$partyName <- trimws(pcon_party_mp$partyName)
lsoa_pcon_imd_vingtile_region_mp <- merge(lsoa_pcon_imd_vingtile_region,pcon_party_mp,by.x='PCON15CD',by.y='constituencyGroupOnsCode')

dfpartycolour <- data.frame(c("Conservative",
                              "Labour",
                              "Liberal Democrat",
                              "Democratic Unionist Party",
                              "Green party",
                              "Independent",
                              "Plaid Cymru",
                              "Scottish National Party",
                              "Sinn Fein",
                              "Speaker"),
                            c("#0087dc",
                              "#dc2727",
                              "#FAA61A",
                              "#D46A4C",
                              "#008066",
                              "#dddddd",
                              "#3F8428",
                              "#FFF95D",
                              "#008800",
                              "#FFFFFF"))
names(dfpartycolour) <- c("partyName","partyColour")
palColour <- c('Conservative' = "#0087dc",'Labour' = "#dc2727", 'Liberal Democrat' = "#FAA61A","Independent" = "#dddddd","Green Party" = "#008066","Speaker" = "#FFFFFF")
lsoa_pcon_imd_vingtile_region_mp <- merge(lsoa_pcon_imd_vingtile_region_mp,dfpartycolour,by='partyName',all.x=T)

# Rank of average rank of pcon areas

## reduce the df to only those columns needed to do aggregation
lsoa_pcon_imd_vingtile_region_mp_min <- lsoa_pcon_imd_vingtile_region_mp[c("PCON15CD","rank")]

## aggregation
lsoa_pcon_imd_vingtile_region_mp_avg <- aggregate(lsoa_pcon_imd_vingtile_region_mp_min,by = list(lsoa_pcon_imd_vingtile_region_mp_min$PCON15CD),FUN=mean)

## rank

lsoa_pcon_imd_vingtile_region_mp_avg$rankofaveragerank <- rank(lsoa_pcon_imd_vingtile_region_mp_avg$rank) 

## merge back into main df

lsoa_pcon_imd_vingtile_region_mp_avg <- merge(lsoa_pcon_imd_vingtile_region_mp,lsoa_pcon_imd_vingtile_region_mp_avg,by.x='PCON15CD',by.y='Group.1',all.x=T)

# Add in a factor that has been reordered by the rank of average rank

lsoa_pcon_imd_vingtile_region_mp_avg$PCON15NM_Sort <- reorder(lsoa_pcon_imd_vingtile_region_mp_avg$PCON15NM,lsoa_pcon_imd_vingtile_region_mp_avg$rankofaveragerank)

# North West
lsoa_pcon_imd_vingtile_region_NW <- lsoa_pcon_imd_vingtile_region_mp_avg[which(lsoa_pcon_imd_vingtile_region_mp_avg$RGN17NM == 'North West'), ]
pcon <- ggplot(lsoa_pcon_imd_vingtile_region_NW, aes(PCON15NM, vingtile, fill= partyName))
pcon <- pcon + facet_wrap(~ PCON15NM_Sort, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
pcon <- pcon + geom_violin(color="#F7ECD9") +
  theme_classic() + theme(strip.background = element_rect(color="#f7ECD9",fill = '#F7ECD9'),strip.text = element_text(color='black',size=10),axis.line.x = element_blank(),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",plot.background=element_rect(color="#f7ECD9",fill="#F7ECD9"),panel.background = element_rect(color="#f7ECD9",fill="#F7ECD9"))
pcon <- pcon + scale_fill_manual(values = palColour)
pcon
ggsave("engpconnw_raw.svg",pcon,width=400,height=500,units="mm")

# North East
lsoa_pcon_imd_vingtile_region_NE <- lsoa_pcon_imd_vingtile_region_mp_avg[which(lsoa_pcon_imd_vingtile_region_mp_avg$RGN17NM == 'North East'), ]
pcon <- ggplot(lsoa_pcon_imd_vingtile_region_NE, aes(PCON15NM, vingtile, fill= partyName))
pcon <- pcon + facet_wrap(~ PCON15NM_Sort, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
pcon <- pcon + geom_violin(color="#F7ECD9") +
  theme_classic() + theme(strip.background = element_rect(color="#f7ECD9",fill = '#F7ECD9'),strip.text = element_text(color='black',size=10),axis.line.x = element_blank(),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",plot.background=element_rect(color="#f7ECD9",fill="#F7ECD9"),panel.background = element_rect(color="#f7ECD9",fill="#F7ECD9"))
pcon <- pcon + scale_fill_manual(values = palColour)
pcon
ggsave("engpconne_raw.svg",pcon,width=400,height=200,units="mm")
  
# South East
lsoa_pcon_imd_vingtile_region_SE <- lsoa_pcon_imd_vingtile_region_mp_avg[which(lsoa_pcon_imd_vingtile_region_mp_avg$RGN17NM == 'South East'), ]
pcon <- ggplot(lsoa_pcon_imd_vingtile_region_SE, aes(PCON15NM, vingtile, fill= partyName))
pcon <- pcon + facet_wrap(~ PCON15NM_Sort, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
pcon <- pcon + geom_violin(color="#F7ECD9") +
  theme_classic() + theme(strip.background = element_rect(color="#f7ECD9",fill = '#F7ECD9'),strip.text = element_text(color='black',size=10),axis.line.x = element_blank(),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",plot.background=element_rect(color="#f7ECD9",fill="#F7ECD9"),panel.background = element_rect(color="#f7ECD9",fill="#F7ECD9"))
pcon <- pcon + scale_fill_manual(values = palColour)
pcon
ggsave("engpconse_raw.svg",pcon,width=400,height=400,units="mm")

# All

lsoa_pcon_imd_vingtile_region_SE <- lsoa_pcon_imd_vingtile_region_mp_avg[which(lsoa_pcon_imd_vingtile_region_mp_avg$RGN17NM == 'South East'), ]
pcon <- ggplot(lsoa_pcon_imd_vingtile_region_mp_avg, aes(PCON15NM, vingtile, fill= partyName))
pcon <- pcon + facet_wrap(~ PCON15NM_Sort, strip.position = 'bottom', scales = 'free_x',ncol = 12)
pcon <- pcon + geom_violin(color="#F7ECD9") +
  theme_classic() + theme(strip.background = element_rect(color="#f7ECD9",fill = '#F7ECD9'),strip.text = element_text(color='black',size=10),axis.line.x = element_blank(),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",plot.background=element_rect(color="#f7ECD9",fill="#F7ECD9"),panel.background = element_rect(color="#f7ECD9",fill="#F7ECD9"))
pcon <- pcon + scale_fill_manual(values = palColour)
pcon
ggsave("engpconall_raw.svg",pcon,width=400,height=800,units="mm")

# West Midlands
lsoa_pcon_imd_vingtile_region_WM <- lsoa_pcon_imd_vingtile_region_mp_avg[which(lsoa_pcon_imd_vingtile_region_mp_avg$RGN17NM == 'West Midlands'), ]
pcon <- ggplot(lsoa_pcon_imd_vingtile_region_WM, aes(PCON15NM, vingtile, fill= partyName))
pcon <- pcon + facet_wrap(~ PCON15NM_Sort, strip.position = 'bottom', scales = 'free_x',ncol = pperrow)
pcon <- pcon + geom_violin(color="#F7ECD9") +
  theme_classic() + theme(strip.background = element_rect(color="#f7ECD9",fill = '#F7ECD9'),strip.text = element_text(color='black',size=10),axis.line.x = element_blank(),axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(), axis.title.y = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x=element_blank(), legend.position="none",plot.background=element_rect(color="#f7ECD9",fill="#F7ECD9"),panel.background = element_rect(color="#f7ECD9",fill="#F7ECD9"))
pcon <- pcon + scale_fill_manual(values = palColour)
pcon
ggsave("engpconwm_raw.svg",pcon,width=400,height=400,units="mm")

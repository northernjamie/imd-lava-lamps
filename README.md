# IMD Lava Lamp Plots

The repo contains the data and r script to make 'lava lamp plots' by administrative geography, using the Indices of Deprivation data published by the Ministry for Housing, Communities and Local Government.

![Lava Lamp Plots](https://github.com/northernjamie/imd-lava-lamps/blob/master/laimd.png)
## Getting Started

Feel free to clone this repo into a new project to open in RStudio. 

The file [Working_Script.R](https://github.com/northernjamie/imd-lava-lamps/blob/master/Working_Script.R) has the code needed to generate the plots. I've annotated this file as best I can, without overdoing it.

To make this, I:

* Got English LSOA deprivation data from OpenDataCommunities
* Got English LA deprivation data from OpenDataCommunities
* Got an English lookup file from Office for National Statistics
* Got Welsh LSOA deprivation data from StatsWales
* Got a Welsh lookup file from StatsWales
* Separately for England and Wales Matched the data so that I had a single dataframe with lsoa, lsoa deprivation rank, la, la name, la deprivation rank
* Calculated the vingtiles of each lsoa (1 - 20)
* Plotted this as a violin plot using ggplot2 in R


Supporting data is available in the data folder.

The source of the data is:

* [England Lower super output area deprivation data](http://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd%2Findices)

* [England Local authority deprivation data](http://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd%2Findicesbyla)

* [England Lower super output area to local authority lookup](http://geoportal.statistics.gov.uk/datasets/output-area-to-local-authority-district-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-enterprise-partnership-april-2017-lookup-in-england-v2)

* [Wales Lower super output area deprivation and lookup](https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Welsh-Index-of-Multiple-Deprivation/WIMD-2014)

* [Scotland deprivation and DZ - LA data from statistics.gov.scot](http://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation-2016)
## Still to do

* Change ordering of the plots
* Do Northern ireland
* Add data for colouring (such as political control)
* Do a version by parliamentary constituency
* Make a low-res version of the image to embed in this readme
* Sort the labels so they fit (wrap or truncate gracefully)

## Built With

* [RStudio](https://www.rstudio.com/) - IDE for R
* [ggplot2](http://ggplot2.org/) - the plots
* [OpenDataCommunities](http://opendatacommunitiesorg) - For the England deprivation data
* [Office for National Statistics Geoportal](http://geoportal.statistics.gov.uk/) - For the lookups between geo boundaries

## Author

* **Jamie Whyte** - [northernjamie](https://twitter.com/northernjamie)

## Acknowledgments
* [Hadley Wickham](https://twitter.com/hadleywickham) for the marvellous ggplot2

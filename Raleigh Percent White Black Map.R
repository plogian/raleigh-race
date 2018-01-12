#create maps showing the percentage of black and white residents in each census tract in a county

#I made both a ggplot2 and a leaflet version of the maps, just so I could see the difference, ggplot2
#is commented out, but if you want to use it, see the code below. 
#use the development version of ggplot2, so it has the geom_sf() function
#library(devtools)
#install_github("tidyverse/ggplot2")
#library(ggplot2)

library(tidycensus)
library(tidyverse)
library(reshape2)
library(sf)
library(viridis)
library(ggthemes)
library(leaflet)
library(stringr)
library(htmlwidgets)

setwd("~/R/Raleigh Gentrification")
#dir.create("./plots")

StateFIPS <- 37
names(StateFIPS) <- "NC"

CountyFIPS <- 183
names(CountyFIPS) <- "Wake"

apikey <- "336e3fb621e924285143c2d9e1abec0f019046f8"
totalPop <- "B02001_001E"
whitePop <- "B02001_002E"
blackPop <- "B02001_003E"
nativePop <- "B02001_004E"
asianPop <- "B02001_005E"
nativeHaPIpop <- "B02001_006E"
otherPop <- "B02001_007E"
twoOrMorePop <- "B02001_008E"

popvars <- c(totalPop, whitePop, blackPop, nativePop, asianPop, nativeHaPIpop, otherPop, twoOrMorePop)
names(popvars) <- c("totalPop", "whitePop", "blackPop", "nativePop", "asianPop", "nativeHaPIpop", "otherPop", "twoOrMorePop")

CountyPop <- get_acs(geography="tract", variables=popvars, endyear = 2015, output = "wide",
                      state = StateFIPS, county = CountyFIPS, geometry = TRUE,
                      summary_var = NULL, key = apikey, moe_level = 90)

#rename CountyPop with human-readable names. Don't remember how it alternates variable and variableME
popvarsME <- paste0(names(popvars), "ME")
names(CountyPop) <- c("GEOID", rbind(names(popvars), popvarsME), 
                                            "NAME", "geometry") 

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

CountyPop$percentWhite <- CountyPop$whitePop/(CountyPop$totalPop+1)
CountyPopWhite <- subset(CountyPop, select=c('geometry', 'percentWhite'))

#ggplot version of the map
# CountyPopWhite %>%
#   ggplot(aes(fill = percentWhite, color = percentWhite)) +
#   geom_sf() +
#   coord_sf() +
#   scale_fill_viridis() +
#   scale_color_viridis() +
#   theme_pander()

pal <- colorQuantile(palette = "viridis", domain = CountyPopWhite$percentWhite, n = 10)

PercentageWhiteMap <- CountyPop %>%
  st_transform(crs="+init=epsg:4269") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(percentWhite)) %>%
  addLegend("bottomleft",
            #pal is color palette
            pal = pal, 
            values = ~ percentWhite,
            title = "Percentage White Population",
            opacity = 1)

saveWidget(PercentageWhiteMap, file="./plots/PercentWhiteMap.html")

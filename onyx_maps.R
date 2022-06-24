# Load the libraries
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(rgdal)
# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
library(ggplot2)
library(ggmap)
library(ggthemes)

# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
# You now have it in your current working directory, have a look!

world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/TM_WORLD_BORDERS_SIMPL-0.3") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

#Clean
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

#MERGE + select countries with counts
world_spdf<-merge(world_spdf, country_counts, by.x = "NAME", by.y ="country",all.y = T, all.x = F, sort = F)

# Create a color palette with handmade bins.
library(RColorBrewer)
mybins <- c(0,10,20,50,100,500,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$n, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "Count: ", world_spdf@data$n, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


# Basic choropleth with leaflet?
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  #addPolygons( fillColor = ~mypalette(n), stroke=FALSE )
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorNumeric("Reds", log(n))(log(n)) )


# Final Map
leaflet(world_spdf, options = leafletOptions(zoomControl = TRUE,
                                             minZoom = 1.75, maxZoom = 3,  dragging = TRUE, maxBounds = list(
                                               list(-90, -180),
                                               list(90, 180)
                                             ))) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=1.75) %>%
  addPolygons( 
    fillColor = ~mypalette(n), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="green", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~n, opacity=0.9, title = "Billionaires (2022)", position = "bottomleft" )

m  


# -- > Now you have a Spdf object (spatial polygon data frame). You can start doing maps!

#spdf_fortified <- tidy(world_spdf, region = "NAME")
#ggplot() +
#  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#  theme_void() 


data <- read.csv(file = "Onyx Data DataDNA Dataset Challenge June 2022 - Forbes World's Billionaires List 2022.csv")
country_counts <- count(data, country)


world_map <- map_data(map = "world")

world_map.counts <-merge(world_map, country_counts, by.x = "region", by.y ="country",all.x = T, sort = F)


mypalette <- colorNumeric( palette="viridis", domain=world_map.counts$n, na.color="transparent")
mypalette(c(45,43))

leaflet(sf::st_as_sf(world_map.counts, coords = c("long", "lat"), crs = 4326)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=1.75) %>%
  addPolygons( fillColor = ~mypalette(n), stroke=FALSE )




##GGMAP
library(sf)
library(geosphere)


countryOfCitizenship_counts <- data%>%
  group_by(countryOfCitizenship)%>% 
  summarise(
    f_share.citizen = sum(gender == 'Female', na.rm=T) / n(),
    n.citizen = n()
  )
country_counts <- data%>%
  group_by(country)%>% 
  summarise(
    f_share = sum(gender == 'Female', na.rm=T) / n(),
    n = n(),
    sum_wealth = sum(finalWorth),
  )
country_counts <- merge(country_counts, countryOfCitizenship_counts, by.x = "country",
                        by.y = "countryOfCitizenship", all.x = T, all.y = T)


world_map <- map_data("world")
world_map$region<-replace(world_map$region, world_map$region == "USA", "United States")
world_map$region<-replace(world_map$region, world_map$region == "UK", "United Kingdom")

ggplot() + geom_polygon( data=world_map, aes(x=long, y=lat, group=group),
                         color="black", fill="lightblue" )


#add custom data

ggplot(country_counts) +
  #geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "gray85") +
  geom_map(aes(map_id = country, fill = n), map = world_map)+
  #geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "NA") +
  #expand_limits(x = world_map$long, y = world_map$lat) +
  theme_void()+coord_fixed()





#remove antarcica
#one way
ggplot(country_counts) +
  #geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "gray85") +
  geom_map(aes(map_id = country, fill = n), map = world_map)+
  #geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "NA") +
  #expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map(ylim = c(-60, 90), xlim=c(-180,180))+
  theme_void()#+ coord_sf(ylim = c(-50, 90), crs = 3338)




#second way to remove antarctica
world.sf <- sf::st_as_sf(world_map, coords = c("long", "lat"), crs = 3857) %>%  #
  group_by(group) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

world.sf <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE), crs = st_crs(3857))
library(rgdal)
world.sf$ID<-replace(world.sf$ID, world.sf$ID == "USA", "United States")
world.sf$ID<-replace(world.sf$ID, world.sf$ID == "UK", "United Kingdom")
merged.sf <- left_join(world.sf, country_counts, by= c("ID" = "country"))


ggplot(country_counts) +
  geom_sf(data = world.sf, stroke = 0, colour = "white", fill = "gray80") + 
  coord_sf(ylim = c(-50, 90), xlim=c(-180,180), crs = st_crs(3857))+#, datum=st_crs(3857)) +
  geom_map(aes(map_id = country, fill = n), map = world_map)+
  theme_void()

ggplot() +
  geom_sf(data = world.sf, stroke = 0, colour = "white", fill = "gray80") + 
  coord_sf(ylim = c(-50, 90), xlim=c(-180,180), crs = st_crs(3857))+#, datum=st_crs(3857)) +
  geom_sf(data=country_counts, fill = n)+
  theme_void()

#MERGED
ggplot() +
  #geom_sf(data = merged.sf, color='gray85', stroke = 1, fill = "gray85") +
  geom_sf(data = merged.sf, aes(fill=n), stroke = 0.1, color='gray70') + 
  #coord_sf(crs = st_crs(3857))+
  coord_sf(ylim = c(-50, 90), xlim=c(-180,180))+
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="gray85")
  #coord_sf(ylim = c(-50, 90), xlim=c(-180,180))+
  theme_void()

#ANOTHEROPTION
spdf_world <- ne_countries(returnclass = "sf",scale = "medium") %>%
  select(name, continent, geometry) %>% filter(continent != "Antarctica")
merged.spsf <- left_join(spdf_world, country_counts, by= c("name" = "country"))


ggplot(merged.spsf) +
  geom_sf(color = "black", fill = "white")+
  coord_sf(crs = st_crs(3857))+   theme_void()

ggplot() +
  geom_sf(data = merged.spsf, aes(fill=n), stroke = 0.1, color='gray70') + 
  coord_sf(crs = st_crs(3857))+
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="gray85")+
  theme_void()

           
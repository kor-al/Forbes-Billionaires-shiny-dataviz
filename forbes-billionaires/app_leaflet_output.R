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
world_spdf<-merge(world_spdf,country_counts, by.x = "NAME", by.y ="country",all.y = T, all.x = F, sort = F)



#==============

output$map <- renderLeaflet({
  # Use leaflet() here, and only include aspects of the map that
  # won't need to change dynamically (at least, not unless the
  # entire map is being torn down and recreated).
  # Basic choropleth with leaflet?
  leaflet(world_spdf, options = leafletOptions(zoomControl = F,
                                               minZoom = 1.2, maxZoom = 1.2,  
                                               dragging = F, maxBounds = list(
                                                 list(-60, -180),
                                                 list(90, 180)
                                               ))) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=0 , zoom=1.75) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels)# %>%
  #addLegend( pal=palette, values=~input$variable, opacity=0.9, title = "Billionaires (2022)", position = "bottomleft" )
  
})


palette <-  reactive({
  
  if (input$variable == "sum_wealth"){
    mybins <- mybins.country_worth
  }
  else{
    mybins <- mybins.country_counts
  }
  
  #colorBin( palette="BuGn", domain=country_counts[input$variable], na.color="transparent", bins=mybins)
  colorBin( colorRamp(c("#f1cd8d", "#8FBCBB"), interpolate="spline"), domain=country_counts[input$variable], na.color="transparent", bins=mybins)
})

# Incremental changes to the map
observe({
  # Prepare the text for leaflet tooltips:
  mytext <- paste(
    "Country: ", world_spdf@data$NAME,"<br/>", 
    "Number of Billionaires: ", world_spdf[['n']], "<br/>",
    "Total wealth: ", format(round(world_spdf[['sum_wealth']] / 1e3, 1), trim = TRUE), "B<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  
  world.data <- world_spdf
  leafletProxy("map", data = world_spdf) %>%
    clearShapes() %>%
    addPolygons( 
      fillColor = palette()(world.data[[input$variable]]), 
      stroke=F, 
      fillOpacity = 0.9, 
      color="green", 
      weight=0.3,
      layerId = world_spdf$NAME, 
      label = mytext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) 
})

# Use a separate observer to recreate the legend as needed.
observe({
  proxy <- leafletProxy("map", data = world_spdf)
  
  # Remove any existing legend, and only if the legend is
  # enabled, create a new one.
  proxy %>% clearControls() %>%
    addLegend( pal=palette(), values=~input$variable, opacity=0.9, title = "Billionaires (2022)", position = "bottomleft" )
})

#observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
#  p <- input$map_shape_click$id
#  print(p)
#})

observeEvent(input$map_shape_click, {
  countryName <- input$map_shape_click$id
  proxy <- leafletProxy("map", data = world_spdf)
  print(countryName)
  if (countryName == "highlighted_polygon"){
    #input$map_shape_click$id<-NULL;
    proxy %>% removeShape("highlighted_polygon")
  }
  else if(!is.null(countryName)){
    #remove any previously highlighted polygon
    proxy %>% removeShape("highlighted_polygon")
    
    #get the selected polygon and extract the label point 
    selected_polygon <- subset(world_spdf,world_spdf$NAME==countryName)
    #print(selected_polygon)
    polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
    
    #add a slightly thicker red polygon on top of the selected one
    proxy %>% addPolylines(stroke=F, weight = 1,fill="red", 
                           layerId = "highlighted_polygon",
                           data=selected_polygon,group="highlighted_polygon")
  }
})



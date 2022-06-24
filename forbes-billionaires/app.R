#
# Shiny web application
#

# install.packages("remotes")
#remotes::install_github("d3treeR/d3treeR")

library(shiny)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)
#library(RColorBrewer)
#library(rcartocolor)
#library(waffle)
#library(treemap)
#library(d3treeR)
#Maps
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

data <- read.csv(file = "Onyx Data DataDNA Dataset Challenge June 2022 - Forbes World's Billionaires List 2022.csv")
data$philanthropyScore <- factor(data$philanthropyScore, levels = c(NA, 1,2,3,4,5), exclude = NULL)
levels(data$philanthropyScore)[is.na(levels(data$philanthropyScore))] <- "No Data"

data$gender[data$gender==""] <- "Couple"
data$gender <- as.factor(data$gender)
data$gender <- recode(data$gender , "F" = "Female", "M"="Male")

data$selfMade <- as.factor(data$selfMade)
data$selfMade <- recode(data$selfMade , "True" = "Yes", "False" ="No")


data$finalWorthFormatted <- paste0(format(round(data$finalWorth / 1e3, 1), trim = T, drop0trailing = T), "B")
data.us<-data[data$countryOfCitizenship == "United States",]

data.us.f <- data.us %>% filter(gender == 'Female')
data.us.m <- data.us %>% filter(gender == 'Male')
data.us.c <- data.us %>% filter(gender == "Couple")

sources <-unlist(strsplit(data$source,', '))

#calculate some stats for the text 
prop.table(table(data.us$philanthropyScore))
prop.table(table(data$countryOfCitizenship))

q.worth <- quantile(data$finalWorth, probs = c(.95))
data <- transform(data, worthGroup = ifelse(finalWorth>q.worth, 'Top 5%', 'Bottom 95%'))
data$worthGroup <- as.factor(data$worthGroup)
n.bins <- 30 # number of bins
additional.cutoffs <- q.worth

#finalWorth bins
bins <- seq(log10(min(data$finalWorth)), log10(max(data$finalWorth)), length.out = n.bins)    
bins <- c(bins, log10(additional.cutoffs)) %>% sort()



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


spdf_world <- ne_countries(returnclass = "sf",scale = "medium") %>%
  select(name, continent, geometry) %>% filter(continent != "Antarctica")
merged.spsf <- left_join(spdf_world, country_counts, by= c("name" = "country"))


# Bins for  counts per country
mybins.country_counts <- c(0,10,20,50,100,500,Inf)
# Bins for finalWorth
mybins.country_worth <- c(1e3,1e4,1e5,1e6, Inf)
#map colors
colors.country = c("#F9FADC", "#A0BC76","#9CBA8C","#79a17f","#717666","#686e5f")



#THEME
colors.gender<-c("#ed6815", "#533a9e","#79a17f")
names(colors.gender) <- levels(data$gender)
colScale.gender <- scale_colour_manual(name = "gender",values = colors.gender)
colScaleFill.gender <- scale_fill_manual(name = "genderFill",values = colors.gender)

colors.selfMade<-c("#A0BC76", "#517787")
names(colors.selfMade) <- levels(data$selfMade)
colScale.selfMade <- scale_colour_manual(name = "selfMade",values = colors.selfMade)
colScaleFill.selfMade <- scale_fill_manual(name = "selfMadeFill",values = colors.selfMade)

colors.worth<-c("#d0ccbe", "#09addb")
names(colors.worth) <- levels(data$worthGroup)
colScale.worth <- scale_fill_manual(name = "worth",values = colors.worth)

#background_color<- "#d4dadc"
background_color<- "#FEFEEE"

billion_theme <- function() {
  theme_minimal() %+replace%
    theme(
      plot.background = element_rect(fill = background_color, colour = background_color),
      #panel.background = element_rect(fill = "#ddd"),
      panel.background = element_rect(fill = background_color),
      axis.text = element_text(colour = "#000000", family = 'Source Serif Pro'),
      text = element_text(colour = "#000000", family = 'Roboto'),
      #plot.title = element_text(colour = "#fff", face = "bold", size = 18, vjust = 1, family = "Impact"),
      axis.title = element_text(colour = "#000000", size = 12, family = "Source Serif Pro"),
      #axis.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "null"),
      panel.grid.major.x = element_blank(),#element_line(colour = "#E7A922"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      #strip.text = element_text(family = "Impact", colour = "white"),
      #strip.background = element_rect(fill = "#E7A922"),
      #axis.ticks = element_line(colour = "#E7A922")
    )
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Application title
  titlePanel("Forbes World’s Billionaires List 2022"),
  fluidRow(
    column(6, 
           p("According to the Forbes data, there were 2,668 Billionaires in the world at the end of April 2022.
               Altogether, their net worth is more than $12 trillion. 
               However, most of this wealth (62%) is held by only 5% of them or a group of 143 people.")
    ),
    column(6,
           p("Almost a third of all the billionaires are U.S. citizens.
               To see how philanthropic these richest people are, Forbes assigned a philanthropy score from 1 to 5, with 5 representing the most generous givers. 
               It turns out that only 1% of the U.S. billionaires have the highest score (given away 20% or more of wealth)."),
    )
  ),
  fluidRow(
    column(6, class="flex",
           #textOutput("result")
           tags$h3("Distribution of World's billionaires worth"),
           plotlyOutput("hist.worth")
    ),
    column(6,
           tags$h3("Philanthropy scores of top 400 U.S. billionaires"),
           selectInput("scoresVariable", "colored by",
                       c("Gender" = "gender",
                         "Self made" = "selfMade")),
           plotlyOutput("scores")
    )
  ),
  fluidRow(
    column(6, 
           p("While United States is leading in the number of richest people, China and India are second and third topmost countries for billionaires. Among european countries, Germany and Italy also made the top 10 list."),
    ),
    column(6,
           p("The majority of billionaires are male, as women represent only 1% of the richest people. With the plot below, you can explore how gender distribution varies between countries and regions.")
    )
  ),
  #fluidRow(
  #  id = "mapInput",
  # column(id = "mapInput__col", 6, selectInput("variable", "",
  #                     c("Number of Billionaires" = "n",
  #                       #"Number of Billionaire citizens" = "n.citizen",
  #                       "Total Billionaires' Wealth" = "sum_wealth")),
  #         p(id="mapHeader", "by country")
  #  )
  #),
  fluidRow(class="row--no-bmargin",
           column(8,
                  #leafletOutput("map")
                  fixedRow(class = "labelHeader",
                           selectInput("variable", "",
                                       c("Number of Billionaires" = "n",
                                         #"Number of Billionaire citizens" = "n.citizen",
                                         "Total Billionaires' Wealth" = "sum_wealth")),
                           tags$h3(class="inlineHeader", "by country")),
                  p(class="subtitle", "Click on countries to explore their statistics"),
                  plotlyOutput("map"),
           ),
           column(4, 
                  #tags$h3("Gender distribution in the country"),
                  htmlOutput("header.gender"), 
                  plotlyOutput("hist.gender")
           )
  ),
  fluidRow(class="row--bmargin",
           column(4, 
                  #textOutput("result")
                  htmlOutput("header.sources"),
                  plotlyOutput("hist.sources")
           ),
           column(8, 
                  fixedRow(class = "labelHeader",
                           selectInput("categoriesX", "",
                                       c("Count" = "count",
                                         "Net Worth" = "totalWorth")),
                           htmlOutput("header.categories", inline = T)),
                  selectInput("categoriesVariable", " colored by",
                              c("Gender" = "gender",
                                "Self made" = "selfMade")),
                  plotlyOutput("hist.categories")
           )
  ),
  fluidRow(
    column(6, 
           htmlOutput("header.scatter", inline = T),
           selectInput("scatterVariable", " colored by",
                       c("Gender" = "gender",
                         "Self made" = "selfMade")),
           p(class="subtitle mt", "\nClick on dots to explore billionaires' profiles"),
           plotlyOutput("scatter")
    ),
    column(6,
           htmlOutput("result")
    )
  )
)

get_palette<-function(variable, type){
  if (variable == "gender" && type == "color"){
    pal <- colScale.gender 
  }
  if (variable == "gender" && type == "fill"){
    pal <- colScaleFill.gender 
  }
  if (variable == "selfMade" && type == "color"){
    pal <- colScale.selfMade
  }
  if (variable == "selfMade" && type == "fill"){
    pal <- colScaleFill.selfMade 
  }
  return(pal)
}

get_header_html<-function(template){
  inputCountry<- event_data(event = "plotly_click",source="map", priority = "input")$key
  if (!is.null(inputCountry)){
    text <- paste0("in <span class='highlight'>", inputCountry, "</span>") 
  }
  else{
    text <- "<span class='highlight'>worldwide</span>"
  }
  HTML(sprintf(template, text ))
}

# Define server logic required to draw
server <- function(input, output, session) {
  
  output$header.gender <- renderText({
    get_header_html("<h3>Distribution of billionaires %s by gender</h3>") 
  })
  
  output$header.sources <- renderText({
    get_header_html("<h3>Sources of billionaires' worth %s</h3>") 
  })
  
  output$header.categories <- renderText({
    get_header_html("<h3>Billionaires' categories %s</h3>") 
  })
  
  output$header.scatter <- renderText({
    get_header_html("<h3>Distribution of billionaires' age and worth %s</h3>") 
  })
  
  text_reactive <-  eventReactive(event_data("plotly_click", source = "scatter", priority = "input"), {
    point <- event_data(event = "plotly_click",  source="scatter", priority = "input")
    person <-data[data$personName == point$key,]
    text<-paste0('<h4>',person$personName, '</h4>')
    text<-paste0(text, '<p class="info info--main">',person$birthDate, ', ', person$countryOfCitizenship,'</p>')
    text<-paste0(text, '<p class="info info--sub">')
    if (person$title != ""){
      text<-paste0(text, person$title)
    }
    if (person$organization != ""){
      text<-paste0(text, ", ", person$organization)
    }
    if (person$country != person$countryOfCitizenship){
      text<-paste0(text,  ", ", person$country)
    }
    text<- paste0(text, '</p>')
    text<-paste0(text, '<p>', person$about, "</p><p>", person$bio,"</p>")
    HTML(text)
  })
  
  output$result <- renderText({text_reactive()})
  
  palette <-  reactive({
    if (input$variable == "sum_wealth"){
      mybins <- mybins.country_worth
      #scale_fill_steps(low = "#7fb6c7", high = "#1d5e5d", breaks = mybins, labels = scales::dollar,guide = "none")
    }
    else{
      mybins <- mybins.country_counts
      #scale_fill_steps(low = "#7fb6c7", high = "#1d5e5d", breaks = mybins, guide = "none")
    }
    scale_fill_stepsn(colours =  colors.country, breaks = mybins, guide = "none")
    #scale_fill_stepsn(colours =  nord("frost", length(mybins)+1), breaks = mybins)]
  })
  
  output$map <- renderPlotly({
    colorCountries <-  merged.spsf[!is.na(merged.spsf[[input$variable]]),]
    
    p <- merged.spsf %>% highlight_key(~name) %>% ggplot() +
      geom_sf(data = merged.spsf, fill = "gray85", stroke = 1, color='transparent') + 
      geom_sf(data = colorCountries,
              aes(fill = .data[[input$variable]], key = name,
                  text = paste0(name, "</br></br>", n, " Billionaires</br>",
                                format(round(sum_wealth / 1e3, 1), trim = TRUE), "B", " Total Net Wealth")),
              size=0.1, color='#1d5e5d', alpha=0.8) + 
      #geom_sf(data = merged.spsf, aes(fill=n), stroke = 0.1, color='gray70') + 
      coord_sf(crs = st_crs(3857))+
      #scale_fill_continuous(low="thistle2", high="darkred", guide="none",na.value="transparent")+
      palette()+
      billion_theme()+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"))+
      labs(fill = NULL, title=NULL)
    
    gg <- ggplotly(p, tooltip = "text", source="map") 
    
    gg  %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoom2d", "hoverClosestCartesian", "toImage",
                                        "hoverCompareCartesian", "autoScale2d")) %>%
      layout(hoverlabel=list(bgcolor="white")) %>% 
      #      style(
      #        hoveron = "fills",
      #        # override the color mapping
      #        line.color = "transparent",
      #        # don't apply these style rules to the first trace, which is the background graticule/grid
      #        traces = seq.int(2, length(gg$x$data))
      #      ) %>%
      style(
        hoveron = "fill",
        traces =  c(1)) %>%
      event_register('plotly_click') %>%
      highlight(on = "plotly_hover", off = "plotly_doubleclick")
    
  })
  
  observeEvent(event_data("plotly_click",source="map",  priority = "input"), {
    point <- event_data(event = "plotly_click",source="map", priority = "input")
    #print(point)
  })
  
  selectedCountry<-eventReactive(event_data("plotly_click",source="map",  priority = "input"), {
    event_data(event = "plotly_click",source="map", priority = "input")$key
  })
  
  output$hist.worth <-renderPlotly({
    ggplotly(ggplot(data, aes(finalWorth, fill = worthGroup)) +
               geom_histogram(breaks = 10^bins) +
               #geom_vline(xintercept = quantile(data$finalWorth, probs = c(.95)), color= colors.worth['Top 5%'])+
               scale_x_continuous(trans='log10', breaks = 10^bins[seq(1, length(bins), 4)],
                                  labels = unit_format(accuracy = 1,unit = "B", scale = 1e-3))+
               annotate("text", x = c(4000,80000), y=300,
                        color = c(colors.worth['Bottom 95%'], colors.worth['Top 5%']), size= 8,
                        label = c(paste(round(share.worth * 100),"%",sep=""),
                                  paste(round((1-share.worth)*100),"%",sep="")), hjust=0)+
               annotate("text", x = 80000, y=260,color = colors.worth['Top 5%'], size=5,
                        label = "of Total Worth", hjust=0)+
               labs(fill = "Billionaires by Worth", x=NULL, y="Number of Billionaires") +
               billion_theme() + colScale.worth, tooltip = c("count")
    ) %>% 
      layout(
        xaxis = list(title = list(text = "Billionaire Worth", standoff = 10L)),
        legend = list(
          orientation = 'h', x = 0.4, y = 1.1, 
          title = list(text = '')
        )
      ) %>% config(displayModeBar = F)
  })
  
  
  output$scatter <- renderPlotly({
    inputCountry<- event_data(event = "plotly_click",source="map", priority = "input")$key
    pal <- get_palette(input$scatterVariable, "color")
    if (!is.null(inputCountry)){
      data.filtered <-  data %>% filter(country==inputCountry)
    }
    else{
      data.filtered <-  data %>% top_n(n=400, wt=finalWorth)
    }
    
    ggplotly(
      data.filtered %>%
        ggplot( aes(finalWorth, age, 
                    shape = (!!as.symbol(input$scatterVariable)),
                    color = (!!as.symbol(input$scatterVariable)), 
                    key = personName, label = personName, alpha = 0.1,
                    text = paste0("Name: ", personName, "</br></br>",
                                  "Final Worth: ", format(round(finalWorth / 1e3, 1), trim = TRUE), "B</br>",
                                  "Age: ", age))) +
        scale_y_continuous()+
        scale_x_continuous(trans='log10', breaks = 10^bins[seq(1, length(bins), 4)],
                           #breaks = trans_breaks(trans = 'log10', inv = function(x) 10^x, n=8),
                           labels = unit_format(accuracy = 1,unit = "B", scale = 1e-3))+
        labs(color = input$scatterVariable, x=NULL, y="Age")+
        geom_jitter(height = 0.2, width = 0) + billion_theme() + pal
      , tooltip = c("text"), source="scatter"
    ) %>% 
      layout(
        xaxis = list(title = list(text = "Billionaire Worth", standoff = 10L)),
        legend = list(orientation = 'h', x = 0, y = 1.1,
                      title = list(text = input$scatterVariable))
      ) %>% event_register("plotly_click") %>% config(displayModeBar = F)
  })
  
  output$scores <-renderPlotly({
    pal <- get_palette(input$scoresVariable, "color")
    ggplotly(
      data.us %>% #filter(!is.na(philanthropyScore)) %>% 
        top_n(n=400, wt = finalWorth)%>%
        ggplot(aes(finalWorth, philanthropyScore,
                   color = (!!as.symbol(input$scoresVariable)),
                   shape = (!!as.symbol(input$scoresVariable)),
                   key = personName, alpha = 0.1,
                   text = paste0("Name: ", personName, "</br></br>",
                                 "Final Worth: ", format(round(finalWorth / 1e3, 1), trim = TRUE), "B</br>",
                                 "Philanthropy Score: ", philanthropyScore
                   )), size = 1)+
        scale_y_discrete(limits = levels(data$philanthropyScore))+
        scale_x_continuous(trans='log10', breaks = 10^bins[seq(1, length(bins), 4)],
                           #breaks = trans_breaks(trans = 'log10', inv = function(x) 10^x, n=8),
                           labels = unit_format(accuracy = 1,unit = "B", scale = 1e-3))+
        labs(color = input$scoresVariable, x=NULL, y="Philanthropy Score")+
        geom_jitter(height = 0.2, width = 0) + billion_theme() + pal
      , tooltip = c("text"), source="scatter"
    ) %>% 
      layout(
        xaxis = list(title = list(text = "Billionaire Worth", standoff = 10L)),
        legend = list(orientation = 'h', x = 0, y = 1.1,
                      title = list(text = input$scoresVariable))
      ) %>% config(displayModeBar = F)
  })
  
  output$hist.gender <- renderPlotly({
    inputCountry<- event_data(event = "plotly_click",source="map", priority = "input")$key
    if (!is.null(inputCountry)){
      data.filtered <-  data %>% filter(country==inputCountry)
    }
    else{
      data.filtered <-  data
    }
    
    ggplotly(
      data.filtered %>%
        ggplot(aes(gender, fill=selfMade, alpha=0.9)) + 
        geom_bar( na.rm=TRUE) +
        labs(x=NULL, y=NULL, title=NULL)+billion_theme()+
        colScaleFill.selfMade,
      tooltip = c("y", "x", "fill")
    )%>% 
      layout(
        legend = list(
          orientation = 'h', x = 0, y = 1.1, 
          title = list(text = 'Self Made')
        ))%>%
      config(displayModeBar = F)
    
    #coubnts
    #ggplotly(
    #  data.filtered %>%
    #    ggplot(aes(y=category, fill=selfMade)) + 
    #    geom_bar()+ theme_bw()
    #)
    
  })
  
  output$hist.categories <- renderPlotly({
    inputCountry<- event_data(event = "plotly_click",source="map", priority = "input")$key
    pal <- get_palette(input$categoriesVariable, "fill")
    if (!is.null(inputCountry)){
      data.filtered <-  data %>% filter(country==inputCountry)
    }
    else{
      data.filtered <-  data
    }
    
    if(input$categoriesX == "totalWorth"){
      scaleX <- scale_x_continuous(
        labels = unit_format(accuracy = 1,unit = "B", scale = 1e-3))
    }
    else{
      scaleX <- scale_x_continuous()
    }
    
    ggplotly(
      data.filtered %>%
        #group_by(category,selfMade) %>%
        #summarise(totalWorth = sum(finalWorth), count = n()) %>%
        #ungroup() %>%
        #arrange(count) %>%
        group_by(category, (!!as.symbol(input$categoriesVariable))) %>% 
        summarise(totalWorth = sum(finalWorth), count = n()) %>% 
        mutate(size= sum(totalWorth)) %>% 
        ungroup %>%
        arrange(-size, -totalWorth, category) %>%
        ggplot(aes(y=category,x=(!!as.symbol(input$categoriesX)), alpha=0.9,
                   fill= (!!as.symbol(input$categoriesVariable)))) + 
        geom_col( na.rm=TRUE) +
        labs(x=NULL, y=NULL, title=NULL)+billion_theme()+pal+scaleX,
      tooltip = c("y", "x", "fill")
      #theme(plot.title = element_text(hjust = -0.3),
      #       plot.margin = rep(grid::unit(0.75,"in"),4))
    )%>% 
      layout(
        title = list(y = 1.5,x=0, xref = "plot"),
        xaxis = list(title = list(text = input$categoriesX, standoff = 10L)),
        legend = list(
          orientation = 'h', x = 0, y = 1.1, 
          title = list(text = input$categoriesVariable)
        ))%>%
      config(displayModeBar = F)
    
    #coubnts
    #ggplotly(
    #  data.filtered %>%
    #    ggplot(aes(y=category, fill=selfMade)) + 
    #    geom_bar()+ theme_bw()
    #)
    
  })
  
  output$hist.sources <- renderPlotly({
    
    if (!is.null(input$map_shape_click$id) && input$map_shape_click$id!= "highlighted_polygon"){
      data.filtered <-  data %>% filter(country==input$map_shape_click$id)
    }
    else{
      data.filtered <-  data
    }
    sources <-unlist(strsplit(data.filtered$source,', '))
    df.sources <- data.frame(table(sources))
    
    ggplotly(
      df.sources%>%top_n(n = 10, wt = Freq) %>%
        ggplot(aes(x=Freq, y=reorder(sources, Freq),
                   text = paste0("Source: ", reorder(sources, Freq), "</br></br>",
                                 "Count: ", Freq, "</br>"))) +
        geom_point(size = 2, colour = "#517787")+
        geom_segment(aes(xend = 0, yend =  sources), size = 0.2, colour = "#517787")+
        #geom_bar(stat="identity", width=0.5) + 
        labs(x=NULL, y=NULL, title=NULL)+billion_theme()+
        theme(plot.title =element_text(size = 14, hjust=0.5, margin = margin(0,0,30,0))),
      tooltip = c("text"))%>% 
      layout(
        title = list(y = 1.5,x=0, xref = "plot"),
        xaxis = list(title = list(text = "Count", standoff = 10L)),
        margin = list(l = 0, t = 0)
      ) %>% config(displayModeBar = F)
  })
  
  
  output$circles.scores<- renderPlotly({
    #scores
    
    #Create layouts
    data.gg <- data.us %>% 
      split(.$philanthropyScore) %>% 
      map(~circleProgressiveLayout((.x$finalWorth), sizetype='radius')) %>% 
      imap_dfr(~circleLayoutVertices(.x, npoints=50) %>% mutate(category = .y))
    
    ggplotly(
      ggplot() + 
        geom_polygon(data = data.gg, aes(x, y, group = id, fill = category), alpha = 0.6) +
        facet_wrap(~category) +
        theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
        coord_equal() +billion_theme()+
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),axis.title.y=element_blank(),
              legend.position="none",panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank()) +scale_fill_nord("frost")
    )
  })
  
  output$waffle.scores <- renderPlotly({
    data.scores <- data.us %>%group_by(philanthropyScore)%>%summarise(count=n())
    data.scores.vec<-data.scores$count
    names(data.scores.vec) <- data.scores$philanthropyScore
    ggplotly(waffle(data.scores.vec, title="Scores",rows= 25, size = 0.5)+scale_fill_nord())
    #xlab="1 square = 1000 persons")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


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
share.worth<-sum(data$finalWorth[data$finalWorth > q.worth])/sum(data$finalWorth)
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
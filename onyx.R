library(dplyr)
library(ggplot2)
#theme_set(theme_bw())
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(scales)
library(waffle)
# library
library(treemap)
library(d3Tree)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

data <- read.csv(file = "Onyx Data DataDNA Dataset Challenge June 2022 - Forbes World's Billionaires List 2022.csv")
print(data)

count(data, gender)
data %>%
  group_by(gender, category) %>%
  summarise(count=n())

counts.gender.selfMade <- data %>%group_by(gender, selfMade)%>%summarise(count=n())
females <- as.data.frame(counts.gender.selfMade[counts.gender.selfMade$gender == 'M',])
females.vec<-females$count
names(females.vec) <- females$selfMade
waffle(females.vec, rows=50, size=0.5, 
       title="Age Groups bifurcation", 
       xlab="1 square = 1000 persons")

#sources
unlist(strsplit(data$source,', '))
count()
sources <-unlist(strsplit(data$source,', '))
df.sources <- data.frame(table(sources))

#add another column
#sources.cat <- data %>% 
#  group_by(selfMade) %>%
#  do(data.frame(val=unlist(strsplit(.$source,', '))))

count(sources.cat, selfMade, val)

#ggplot(data.frame(sources), aes(y = sources)) + geom_bar()

#
#
#df.sources.top10 <-df.sources[order(-df.sources$Freq),][0:10,] 
df.sources.top10 <- df.sources%>%top_n(n = 10, wt = Freq)
ggplot(data=df.sources.top10, aes(x=Freq, y=reorder(sources, Freq))) + geom_bar(stat="identity", width=0.5)


#country counts
data %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(1-freq)

ggplot(data, aes(country)) + geom_bar() +
  theme(axis.text.x=element_text(angle=90,hjust=1))


#worth
quantile(data$finalWorth, probs = c(.05, .95))
q.worth <- quantile(data$finalWorth, probs = c(.95))
share.worth<-sum(data$finalWorth[data$finalWorth > q.worth])/sum(data$finalWorth)

data <- transform(data, worthGroup = ifelse(finalWorth>q.worth, 'Top 5%', 'Bottom 95%'))
n.bins <- 30 # number of bins
additional.cutoffs <- q.worth

bins <- seq(log10(min(data$finalWorth)), log10(max(data$finalWorth)), length.out = n.bins)    
bins <- c(bins, log10(additional.cutoffs)) %>% sort()

ggplot(data, aes(finalWorth, fill = worthGroup)) + geom_histogram(breaks = 10^bins)+#binwidth=0.1) +
  geom_vline(xintercept = quantile(data$finalWorth, probs = c(.95)), color= "#00BFC4")+
  scale_x_continuous(trans='log10', breaks = 10^bins[seq(1, length(bins), 3)],
                     #breaks = trans_breaks(trans = 'log10', inv = function(x) 10^x, n=8),
                     labels = unit_format(accuracy = 1,unit = "B", scale = 1e-3))+
  annotate("text", x = c(3000,24000), y=300,color = c("#F8766D", "#00BFC4"), size= 16,
           label = c(paste(round(share.worth * 100),"%",sep=""),
                     paste(round((1-share.worth)*100),"%",sep="")), hjust=0)+
  annotate("text", x = c(3000,24000), y=260,color = c("#F8766D", "#00BFC4"), size= 5,
           label = "of Total Worth", hjust=0)+
  labs(fill = "Billionaires by Worth", x="Billionaire Worth", y="Number of Billionaires")+
  theme(legend.position = c(0.9, 0.5))


length(data$finalWorth[data$finalWorth > quantile(data$finalWorth, probs = c(.95))])/length(data$finalWorth)

ggplot(data, aes(finalWorth)) + geom_histogram() 
+ geom_vline(xintercept = quantile(data$finalWorth, probs = c(.95)))

#siblings
ggplot(data[!is.na(data$numberOfSiblings),], aes(numberOfSiblings)) + geom_histogram(binwidth = 0.5)
data[order(data$numberOfSiblings),][1:10,1:10]

ggplot(data[!is.na(data$numberOfSiblings),], aes((numberOfSiblings), (finalWorth))) + geom_point()

#age
ggplot(data, aes(age)) + geom_histogram()

#age
ggplot(data, aes(age)) + geom_histogram()
ggplot(data, aes(log(finalWorth), log(age))) + geom_point()
ggplot(data, aes((rank), (age))) + geom_point()
ggplot(data, aes((philanthropyScore), (age))) + geom_point()

#philanthropyScore
ggplot(data, aes(philanthropyScore)) + geom_histogram(binwidth = 0.5)
ggplot(data, aes(philanthropyScore, category)) + geom_boxplot()  + facet_wrap(~gender, nrow = 1)
ggplot(data, aes(philanthropyScore, age)) + geom_boxplot()  + facet_wrap(~gender, nrow = 1)
ggplot(data, aes(philanthropyScore, age)) + geom_boxplot()  + facet_wrap(~selfMade, nrow = 1)

ggplot(data, aes((finalWorth), (philanthropyScore))) + geom_point()
ggplot(data, aes((philanthropyScore), (numberOfSiblings), size = finalWorth)) + geom_jitter()
ggplot(data, aes((philanthropyScore), (age), size = finalWorth, color=selfMade)) + geom_jitter()
ggplot(data, aes((philanthropyScore), (age), size = finalWorth, color=gender,alpha = 0.5)) + geom_jitter(width = 0.2)
ggplot(data, aes((philanthropyScore), (age), size = rank, color=gender)) + geom_jitter(width = 0.3)+
  scale_size(trans = 'reverse')

ggplot(data, aes((philanthropyScore), log(finalWorth), size = age,alpha = 0.5)) + geom_jitter(width = 0.3)


ggplot(data, aes(y=finalWorth, x = factor(philanthropyScore))) + geom_boxplot() 

ggplot(data, aes(y=finalWorth, x = factor(philanthropyScore))) + geom_boxplot() + facet_wrap(~selfMade, nrow = 1)
ggplot(data, aes(y=finalWorth, x = factor(philanthropyScore))) + geom_boxplot() + facet_wrap(~gender, nrow = 1)




#selfMade
ggplot(data, aes(log(finalWorth), log(age))) + geom_point()  + facet_wrap(~interaction(selfMade, gender), ncol = 1)
ggplot(data, aes(interaction(selfMade, gender),log(finalWorth))) + geom_boxplot() 
ggplot(data, aes(selfMade,log(finalWorth))) + geom_boxplot()  + facet_wrap(~gender, nrow = 1)


#category
ggplot(data, aes(category)) + geom_bar() +
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data,aes(category,finalWorth, fill=selfMade)) + 
  geom_bar(stat = "identity", na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=90,hjust=1))


ggplot(data, aes(category, log(finalWorth))) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=1))



#gender
count(data, gender)
ggplot(data, aes(gender, fill = selfMade)) + geom_bar()

ggplot(data, aes(age, fill = gender)) + geom_histogram(binwidth = 0.5)
ggplot(data, aes(age, fill = gender)) + geom_histogram(binwidth = 0.5) + facet_wrap(~gender, scales = "free_y", ncol = 1)
ggplot(data, aes(y=age, fill = gender)) + geom_boxplot() + facet_wrap(~gender, nrow = 1)
ggplot(data, aes(y=age, x = gender)) + geom_violin()


aggregate(philanthropyScore ~ gender+category, data=data, function(x) {sum(is.na(x))/length(x)}, na.action = NULL)

ggplot(data, aes(philanthropyScore,fill=selfMade)) +
  #geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_histogram(binwidth = 0.5)

ggplot(data, aes(philanthropyScore)) +
  #geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_histogram(aes(y = stat(density) * 1), binwidth = 1) + 
  facet_wrap(~gender, nrow = 1, scales = "free_y")+
  scale_y_continuous(labels = percent)


ggplot(data, aes((finalWorth), (age))) + geom_point() + facet_wrap(~gender, ncol = 1)
ggplot(data, aes((rank), (age))) + geom_point() + facet_wrap(~gender, ncol = 1)
ggplot(data, aes(category, log(finalWorth))) + 
  geom_boxplot() +
  geom_line(colour = "#3366FF", alpha = 0.5)  +
  theme(axis.text.x=element_text(angle=90,hjust=1))  + facet_wrap(~gender, ncol = 1)
ggplot(data, aes(category)) + geom_bar() +
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  facet_wrap(~gender, ncol = 1)



#WAFFLE

#gender

counts.gender.selfMade <- data %>%group_by(gender, selfMade)%>%summarise(count=n())
females <- as.data.frame(counts.gender.selfMade[counts.gender.selfMade$gender == 'M',])
females.vec<-females$count
names(females.vec) <- females$selfMade
waffle(females.vec, rows=50, size=0.5, 
       title="Self Made, Females")


#scores

cc <- scales::seq_gradient_pal("grey", "red", "Lab")(seq(0,1,length.out=6))

data.scores <- data.us %>%group_by(philanthropyScore)%>%summarise(count=n())
data.scores.vec<-data.scores$count
names(data.scores.vec) <- data.scores$philanthropyScore
waffle(data.scores.vec, rows=25, size=0.5, title="Scores") 
       #xlab="1 square = 1000 persons")



#CIRCLES

#Create layouts for each group by splitting, mapping and recombining
split<- data.us %>% split(.$philanthropyScore)
packing <- split %>% map(~circleProgressiveLayout((.x$finalWorth), sizetype='area')) 
data.circles <- cbind(split, packing)
data.gg<- packing %>% imap_dfr(~circleLayoutVertices(.x, npoints=50) %>% mutate(category = .y))

#Do the thing
ggplot() + 
  geom_polygon(data = data.gg, aes(x, y, group = id, fill = category), alpha = 0.6)+
  scale_fill_viridis_d() +
  geom_text(data = data.circles , aes(x, y, size=finalWorth, label = personName), color="black")+
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  facet_wrap(~category) +
  coord_equal()



#HIERARCHY CIRCLES


#ONE
# Libraries
library(tidyverse)
library(hrbrthemes)
library(circlepackeR)  #devtools::install_github("jeromefroe/circlepackeR")
# Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/..., so I build it
library(data.tree)

data.us$pathString <- paste("Billionairs", data.us$philanthropyScore, data.us$personName, sep = "/")
population <- as.Node(data.us)

# You can custom the minimum and maximum value of the color range.
circlepackeR(population, size = "finalWorth", color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)")

#TWO
#TO DO

#SANKEY
library(networkD3)

links <- data %>% filter(country!=countryOfCitizenship & country!="") %>%
  group_by(countryOfCitizenship, country) %>% summarize(count=n())
links <- data.frame(links)
links<- links[links$count>1,]


#nodes <- data.frame(
#  name=c(as.character(links$countryOfCitizenship), 
#         as.character(links$country)) %>% unique()
#)
# put your df in two columns, and preserve the ordering in many levels (columns) with paste0
#links <- data.frame(source =  as.character(c(paste0(links$countryOfCitizenship,'_1'))),
#                    target   =  as.character(c(paste0(links$country,'_2'))))

links$source =  as.character(c(paste0(links$countryOfCitizenship,'_1')))
links$target =  as.character(c(paste0(links$country,'_2')))


nodes <- data.frame(name = unique(c(links$source, links$target)))

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "count", NodeID = "name", 
                   sinksRight=FALSE)
p


#MAP
world_map <- map_data(map = "world")
ggplot(data = world) + geom_sf()
#ggplot() + geom_polygon( data=world_map, aes(x=long, y=lat))
#group is important!
ggplot() + geom_polygon( data=world_map, aes(x=long, y=lat, group=group),
                color="black", fill="lightblue" )


country_counts <- count(data, country)

cities_counts <- count(data, city)
country_counts <- count(data, country)
countryOfCitizenship_counts <- count(data, countryOfCitizenship) %>%rename(n_countryOfCitizenship = n)
fgender_shares <- data%>%
  group_by(country)%>% 
  summarise(
    f_share = sum(gender == 'F', na.rm=T) / n()
  )
fgender_shares.citizenship <- data%>%
  group_by(countryOfCitizenship)%>% 
  summarise(
    f_share.citizen = sum(gender == 'F', na.rm=T) / n()
  ) 
country_counts <- merge(country_counts, countryOfCitizenship_counts, by.x = "country",
                        by.y = "countryOfCitizenship", all.x = T, all.y = T)
country_counts <- merge(country_counts, fgender_shares, by = "country", all.x = T, all.y = T)
country_counts <- merge(country_counts, fgender_shares.citizenship, by.x = "country",
                        by.y = "countryOfCitizenship", all.x = T, all.y = T)




country_gender_counts <- data %>% group_by(country, gender) %>% summarise(n=n())
country_selfMade_counts <- data %>% group_by(country, selfMade) %>% summarise(n=n())

sum(data$country != data$countryOfCitizenship)/length(data)


data.moved <-  data %>% filter(country != countryOfCitizenship)


ggplot(country_counts) +
  #geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "gray85") +
  geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  #geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "NA") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  theme_void()+coord_fixed()

ggplot(country_gender_counts) +
  #geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "gray85") +
  geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  #geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "NA") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  theme_void()+coord_fixed()+
  facet_grid(facets= gender ~.)

ggplot(country_selfMade_counts) +
  #geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "gray85") +
  geom_map(aes(map_id = country, fill = log10(n)), map = world_map)+
  #geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'gray85', fill = "NA") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  theme_void()+coord_fixed()+
  facet_grid(facets= selfMade ~.)



selectedCountries <- world_map %>% filter(region %in% country_counts$country)
selectedCountries <-merge(selectedCountries, country_counts, "country", "region")



library(sf)
library(geosphere)

world <- map_data("world")
#join counts
#world_country_counts <-merge(world, country_counts, "country", "region",all.x = T, sort = F)
#world_country_counts <- left_join(world, country_counts, by=c("region", "country"))

centroids <- world_map %>% 
  group_by(region) %>% 
  group_modify(~ data.frame(centroid(cbind(.x$long, .x$lat))))

#united states -> USA
count(world, region)
replace(country_counts$country, country_counts$country == "United States", "USA")


centroids.counts <-merge(country_counts, centroids, by.x = "country", by.y ="region",all.x = T, sort = F)

ggplot(world, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group)) +
  geom_map(map = world, aes(map_id = region), colour = 'gray80', fill = "gray85") +
  geom_point(data = centroids.counts, aes(lon, lat, size=n), colour = 'purple', alpha = .5)+
  scale_size_continuous(range = c(1, 8), breaks = c(100, 250, 500, 600))

#remove antarcica
world.sf <- sf::st_as_sf(world, coords = c("long", "lat"), crs = 4326) %>% 
  group_by(group) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

world.plot <- ggplot() +
  geom_sf(data = world.sf, colour = "gray85", fill = "gray80") + 
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme(panel.background = element_rect(fill = 'white'))

world.plot +
  geom_point(data = centroids.counts, aes(lon, lat, size=n), colour = 'purple', alpha = .5)+
  scale_size_continuous(range = c(1, 8), breaks = c(100, 250, 500, 600))


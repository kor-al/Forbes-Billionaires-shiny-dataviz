library(ggplot2)
library(plotly)

data <- read.csv(file = "Onyx Data DataDNA Dataset Challenge June 2022 - Forbes World's Billionaires List 2022.csv")
print(data)


p <- data %>%
  filter(country=='Russia') %>%
  ggplot( aes(finalWorth, age, color=gender)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  theme_bw()

ggplotly(p)


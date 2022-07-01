output$waffle.scores <- renderPlotly({
  data.scores <-
    data.us %>% group_by(philanthropyScore) %>% summarise(count = n())
  data.scores.vec <- data.scores$count
  names(data.scores.vec) <- data.scores$philanthropyScore
  ggplotly(waffle(
    data.scores.vec,
    title = "Scores",
    rows = 25,
    size = 0.5
  ) + scale_fill_nord())
  #xlab="1 square = 1000 persons")
})


output$circles.scores <- renderPlotly({
  #scores
  
  #Create layouts
  data.gg <- data.us %>%
    split(.$philanthropyScore) %>%
    map( ~ circleProgressiveLayout((.x$finalWorth), sizetype = 'radius')) %>%
    imap_dfr( ~ circleLayoutVertices(.x, npoints = 50) %>% mutate(category = .y))
  
  ggplotly(
    ggplot() +
      geom_polygon(
        data = data.gg,
        aes(x, y, group = id, fill = category),
        alpha = 0.6
      ) +
      facet_wrap( ~ category) +
      theme(
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      ) +
      coord_equal() + billion_theme() +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) + scale_fill_nord("frost")
  )
})

plot_tile_plot <- function(x,y){ 
  
  a <- ggplot(x, aes(water_year, y=Name,fill=Result))+
    geom_tile_interactive(color='white',
                          aes(
                            tooltip= glue(
                              "Water Year: {water_year}
                      Metric: {Name}
                      Result: {Result}
                      Value: {Value}"
                            )
                          ))+
    scale_fill_manual(values=c('#20639B','#F6D55C'))+
    theme_minimal(base_size=7)+
    labs(x='Year', y='')+
    scale_x_continuous(breaks=seq(start_year(),end_year(),1))+
     theme(axis.text.x= element_text(angle=90),
           legend.position = 'bottom')
  
  # y <- y %>% 
  #   mutate(Name = str_wrap(Name,20))
  b <- ggplot(y , aes(water_year, y=Name,fill=Status))+
    geom_tile_interactive(color='white',
                          aes( tooltip= glue(
                            "Water Year: {water_year}
                      Value: {Value}
                      Long Term Mean: {`Long Term Average`}"
                          )
                          )) +
    scale_fill_manual(values=c('#7570b3','#d95f02'))+
    theme_minimal(base_size=7)+
    labs(x='Water Year', y='')+
    scale_x_continuous(breaks=seq(start_year(),end_year(),1))+
    theme(
      axis.text.x= element_text(angle=90),
      legend.position = 'bottom')
  
  # make a girafe plot (interactive) report card plot
  g_inter <- girafe(
    code = print(a / b +  plot_layout(heights = c(.7,.3))), 
    # width_svg = 24, height_svg = 12, bg = "#D7E0DA",
    options = list(
      opts_tooltip(
        opacity = 0.8, use_fill = TRUE,
        use_stroke = FALSE, 
        css = "padding:5pt;font-family: Open Sans;color:black"),
      opts_hover_inv(css = "opacity:0.5"), 
      opts_hover(
        css = girafe_css(
          css = "fill:#4c6061;",
          text = "stroke:none;fill:white;fill-opacity:1;font-size:smaller"
        )
      )
    )
  )
}

plot_BC_hydro_plot <- function(Q, linekey, dat,yr1, yr2){
  
  p <- ggplot(Q)+
    geom_blank( aes(fake_date, q_cfs))+
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'low') %>%
        filter(month %in% 5:10),
      aes(fake_date,ymin=min_acceptable,ymax=min_optimal,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'optimal') %>%
        filter(month %in% 5:10),
      aes(fake_date,ymin=min_optimal,ymax=max_optimal,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'high') %>%
        filter(month %in% 5:10),
      aes(fake_date,ymin=max_optimal,ymax=max_acceptable,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    scale_fill_manual(guide='legend',values=c('red','blue','green','black'),
                      breaks=c('low','optimal','high'),
                      labels = c('Low Acceptable','Optimal','High Acceptable'),name='Boating\nRecreational Preferences')+
    new_scale_fill() +
    
    geom_line(data=dat %>% filter(name !='peak'),aes(fake_date, y=value, color=name, linetype=name),size=1.2)+
    scale_color_manual(guide='legend',values=linekey$line_colors,breaks=linekey$name,
                       labels=linekey$labels,name='Environmental Targets')+
    scale_linetype_manual(guide='legend',values=linekey$line_types,
                          breaks=linekey$name, labels=linekey$labels,name='Environmental Targets')+
    
    new_scale_fill() +
    geom_point(data=dat %>% filter(name =='peak'),aes(fake_date, value,fill=name),shape =23, size=5, alpha=.7)+
    scale_fill_manual(guide='legend',values='yellow',name='Instantenous Peak Target',label='Peak') +
    scale_x_date(date_labels = '%b', date_breaks = '1 months') +
    new_scale_color() +
    geom_line(data = yr1,  linetype='dotdash',
              #col='#1b9e77',
              size=1.2,
              aes(fake_date, q_cfs,group=water_year, col=factor(water_year)
              ))+
    geom_line(data = yr2, linetype='dotdash',
              #col='#d95f02',
              size=1.2,
              aes(fake_date, q_cfs,group=water_year,col=factor(water_year)))+
    scale_color_manual(guide='legend',values=c('black','red'),name='Historical Hydrograph')+
    theme_minimal()+
    scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
    labs(x='Month', y = "Flow (cfs)")
}

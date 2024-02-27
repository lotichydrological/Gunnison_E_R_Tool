plot_BC_hydro_plot_customQ <- function(Q, linekey, dat){
  
  p <- ggplot(Q)+
    geom_blank(aes(date, q_cfs))+
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'low') %>%
        filter(month %in% 5:10),
      aes(fake_date_customQ,ymin=min_acceptable,ymax=min_optimal,fill=fill_name,group=year(fake_date_customQ)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'optimal') %>%
        filter(month %in% 5:10),
      aes(fake_date_customQ,ymin=min_optimal,ymax=max_optimal,fill=fill_name,group=year(fake_date_customQ)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'high') %>%
        filter(month %in% 5:10),
      aes(fake_date_customQ,ymin=max_optimal,ymax=max_acceptable,fill=fill_name,group=year(fake_date_customQ)), alpha=.2
    ) +
    scale_fill_manual(guide='legend',values=c('red','blue','green','black'),
                      breaks=c('low','optimal','high'),
                      labels = c('Low Acceptable','Optimal','High Acceptable'),name='Boating\nRecreational Preferences')+
    new_scale_fill() +
    
    geom_line(data=dat %>% filter(name !='peak'),aes(fake_date_customQ, y=value, color=name, linetype=name),size=1.2)+
    scale_color_manual(guide='legend',values=linekey$line_colors,breaks=linekey$name,
                       labels=linekey$labels,name='Environmental Targets')+
    scale_linetype_manual(guide='legend',values=linekey$line_types,
                          breaks=linekey$name, labels=linekey$labels,name='Environmental Targets')+
    
    new_scale_fill() +
    geom_point(data=dat %>% filter(name =='peak'),aes(fake_date_customQ, value,fill=name),shape =23, size=5, alpha=.7)+
    scale_fill_manual(guide='legend',values='yellow',name='Instantenous Peak Target',label='Peak') +
    scale_x_date(date_labels = '%b', date_breaks = '1 months') +
    new_scale_color() +
    geom_line(linetype='solid',
              #col='#1b9e77',
              size=1.2,
              aes(date, q_cfs,group=name,col=factor(name)
              ))+
    scale_color_manual(guide='legend',values=c('black'),name='Custom Hydrograph')+
    theme_minimal()+
    scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
    labs(x='Month', y = "Flow (cfs)")
}

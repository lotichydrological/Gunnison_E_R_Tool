plot_BC_hydro_plot_new <- function(Q, linekey, dat,yr1, yr2){
  
  year1 <- yr1 %>% 
    pull(water_year) %>% unique(.)
  
  year2<- yr2 %>% 
    pull(water_year) %>% unique(.)
  
  year1_bc_targets <- year1_bc_targets %>% mutate(name = factor(name, levels=c('Peak Target','High Flow Minimum','Baseflow')))
  
  p_yr1 <- ggplot(Q)+
    geom_blank( aes(fake_datetime, q_cfs))+
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'low') %>%
        filter(month %in% 5:10),
      aes(fake_datetime,ymin=min_acceptable,ymax=min_optimal,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'optimal') %>%
        filter(month %in% 5:10),
      aes(fake_datetime,ymin=min_optimal,ymax=max_optimal,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'high') %>%
        filter(month %in% 5:10),
      aes(fake_datetime,ymin=max_optimal,ymax=max_acceptable,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    scale_fill_manual(guide='legend',values=c('red','blue','green','black'),
                      breaks=c('low','optimal','high'),
                      labels = c('Low Acceptable','Optimal','High Acceptable'),name='Boating\nRecreational Preferences')+
    new_scale_fill() +
    geom_line(data=year1_bc_targets,aes(fake_datetime, y=value, color=name),size=1.2)+
    geom_point(data=year1_bc_targets,aes(fake_datetime, y=value, color=name,shape=name),fill='yellow')+
    scale_color_manual(guide='legend',values=c('black',linekey$line_colors),breaks=c('Peak Target',as.character(linekey$name)),
                       labels=c('Spring Peak',as.character(linekey$name)),name='Environmental Flow Targets')+
    scale_shape_manual(guide='legend',values=c(NA,NA,NA),breaks=c('Peak Target',as.character(linekey$name)),
                       labels=c('Spring Peak',as.character(linekey$name)),name='Environmental Flow Targets')+
    guides(color = guide_legend(override.aes = list(linetype= c(0,1,1),size=c(4,4,4),shape=c(23,NA,NA))))+
  
    new_scale_fill() +
    geom_point(data=year1_bc_targets %>% filter(name =='Peak Target'),aes(fake_datetime, value,fill=name),shape =23, size=5, alpha=.7)+
    scale_fill_manual(guide='none',values='yellow',name='Peak Target',label='Peak') +
    #  scale_linetype_manual(guide='none',values=linekey$line_types,
    #                       breaks=linekey$name, labels=linekey$name,name='Environmental Targets')+
     scale_x_date(date_labels = '%b', date_breaks = '1 months') +
    new_scale_color() +
    geom_line(data = yr1,  linetype='solid',
              #col='#1b9e77',
              lwd=1.05,
              aes(fake_datetime, q_cfs,group=water_year, col=factor(water_year)
              ))+
    # geom_line(data = yr2, linetype='dotdash',
    #           #col='#d95f02',
    #           size=1.2,
    #           aes(fake_datetime, q_cfs,group=water_year,col=factor(water_year)))+
    scale_color_manual(guide='legend',values=c('black'),name='Historical Hydrograph')+
    theme_minimal()+
    scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
    labs(x='Month', y = "Flow (cfs)",title=year1)+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  p_yr2 <- ggplot(Q)+
    geom_blank( aes(fake_datetime, q_cfs))+
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'low') %>%
        filter(month %in% 5:10),
      aes(fake_datetime,ymin=min_acceptable,ymax=min_optimal,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'optimal') %>%
        filter(month %in% 5:10),
      aes(fake_datetime,ymin=min_optimal,ymax=max_optimal,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    geom_ribbon(
      data=dat %>%
        mutate(fill_name = 'high') %>%
        filter(month %in% 5:10),
      aes(fake_datetime,ymin=max_optimal,ymax=max_acceptable,fill=fill_name,group=year(fake_date)), alpha=.2
    ) +
    scale_fill_manual(guide='none',values=c('red','blue','green','black'),
                      breaks=c('low','optimal','high'),
                      labels = c('Low Acceptable','Optimal','High Acceptable'),name='Boating\nRecreational Preferences')+
    new_scale_fill() +
    geom_line(data=year2_bc_targets %>% filter(name !='Peak Target'),aes(fake_datetime, y=value, color=name, linetype=name),size=1.2)+
    scale_color_manual(guide='none',values=linekey$line_colors,breaks=linekey$name,
                       labels=linekey$name,name='Other Flow Targets')+
    scale_linetype_manual(guide='none',values=linekey$line_types,
                          breaks=linekey$name, labels=linekey$name,name='Environmental Targets')+
    
    new_scale_fill() +
    geom_point(data=year2_bc_targets %>% filter(name =='Peak Target'),aes(fake_datetime, value,fill=name),shape =23, size=5, alpha=.7)+
    scale_fill_manual(guide='none',values='yellow',name='Peak Target',label='Peak') +
    scale_x_date(date_labels = '%b', date_breaks = '1 months') +
    new_scale_color() +
    geom_line(data = yr2,  linetype='solid',
              #col='#1b9e77',
              lwd=1.05,
              aes(fake_datetime, q_cfs,group=water_year, col=factor(water_year)
              ))+
    # geom_line(data = yr2, linetype='dotdash',
    #           #col='#d95f02',
    #           size=1.2,
    #           aes(fake_datetime, q_cfs,group=water_year,col=factor(water_year)))+
    scale_color_manual(guide='legend',values=c('red'),name='Historical Hydrograph')+
    theme_minimal()+
    scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
    labs(x='Month', y = "Flow (cfs)",title=year2)+
    theme(plot.title = element_text(hjust = 0.5))
  
  p <- p_yr1 / p_yr2
    
    
}

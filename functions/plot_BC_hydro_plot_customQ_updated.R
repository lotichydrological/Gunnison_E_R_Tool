plot_BC_hydro_plot_customQ_updated <- function(Q, linekey, dat){
  
  custom_bc_targets <- custom_bc_targets %>% mutate(name = factor(name, levels=c('Peak Target','High Flow Minimum','Baseflow')))
  
  
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
    geom_line(data=custom_bc_targets,aes(fake_date_customQ, y=value, color=name),size=1.2)+
    geom_point(data=custom_bc_targets,aes(fake_date_customQ, y=value, color=name,shape=name),fill='yellow')+
    scale_color_manual(guide='legend',values=c('black',linekey$line_colors),breaks=c('Peak Target',as.character(linekey$name)),
                       labels=c('Spring Peak',as.character(linekey$name)),name='Environmental Flow Targets')+
    scale_shape_manual(guide='legend',values=c(NA,NA,NA),breaks=c('Peak Target',as.character(linekey$name)),
                       labels=c('Spring Peak',as.character(linekey$name)),name='Environmental Flow Targets')+
    guides(color = guide_legend(override.aes = list(linetype= c(0,1,1),size=c(4,4,4),shape=c(23,NA,NA))))+
    new_scale_fill() +
    geom_point(data=custom_bc_targets %>% filter(name =='Peak Target'),aes(fake_date_customQ, value,fill=name),shape =23, size=5, alpha=.7)+
    scale_fill_manual(guide='none',values='yellow',name='Peak Target',label='Peak') +
    
    
    scale_x_date(date_labels = '%b', date_breaks = '1 months') +
    new_scale_color() +
    geom_line(linetype='solid',
              #col='#1b9e77',
              size=1.2,
              aes(date, q_cfs,group=name,col=factor(name)
              ))+
    scale_color_manual(guide='legend',values=c('black'),name='Hydrograph',label='Custom')+
    theme_minimal()+
    scale_y_log10(breaks=c(100, 250, 500,750,1000,1500,2500,5000,10000 ))+
    labs(x='Month', y = "Flow (cfs)")
}

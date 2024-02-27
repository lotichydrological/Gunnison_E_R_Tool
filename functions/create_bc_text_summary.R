create_bc_text_summary <- function(EFT, REC){
  
  head1<- '<h3>Peak Flows</h3>'
  df_peak <- EFT %>% filter(Type =='24 Hour Instantaneous Peak Flow')
  out<- vector()
  for (i in 1:nrow(df_peak)){
    
    if(df_peak$Status[i] == 'Not Met'){
      out[i]<- paste0('<h4>The ',df_peak$Name[i], ' was met in <b>',df_peak$Result[i],'% </b> of selected years. The target of <b>',df_peak$`Target Goal`[i], '% </b> of years was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out[i]<-paste0('<h4>The ',df_peak$Name[i], ' was met in <b>',df_peak$Result[i],'% </b> of selected years. The target of <b>',df_peak$`Target Goal`[i], '% </b> of years was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }
    
  }
  
  head2<- '<br><h3>High Flow Minimums</h3>'
  
  df_highflow <- EFT %>% filter(Type =='High Flow Period Minimum')
  out2<- vector()
  for (i in 1:nrow(df_highflow)){
    
    if(df_highflow$Status[i] == 'Not Met'){
      out2[i]<- paste0('<h4>The ',df_highflow$Name[i], ' was met in <b>',df_highflow$Result[i],'% </b> of selected years. The target of <b>',df_highflow$`Target Goal`[i], '% </b> of years was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out2[i]<-paste0('<h4>The ',df_highflow$Name[i], ' was met in <b>',df_highflow$Result[i],'% </b> of selected years. The target of <b>',df_highflow$`Target Goal`[i], '% </b> of years was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }
    
  }
  
  head3<- '<br><h3>Baseflow Minimum</h3>'

  df_base <- EFT %>% filter(Type =='Baseflow Minimum')
  out3<- vector()
  for (i in 1:nrow(df_base)){

    if(df_base$Status[i] == 'Not Met'){
      out3[i]<- paste0('<h4>The mean duration in days above the ',df_base$Name[i], ' was <b>',df_base$Result[i],' days </b> in selected years. The target of a mean duration above baseline of <b>',df_base$`Target Goal`[i], '</b> days was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")

    } else{
      out3[i]<- paste0('<h4>The mean duration in days above the ',df_base$Name[i], ' was <b>',df_base$Result[i],' days </b> in selected years. The target of a mean duration above baseline of <b>',df_base$`Target Goal`[i], '</b> days was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }

  }
  
  head4<- '<br><h3>Ramping Rate </h3>'
  
  df_ramp <- EFT %>% filter(Type =='Ramping Rate')
  out4<- vector()
  for (i in 1:nrow(df_ramp)){
    
    if(df_ramp$Status[i] == 'Not Met'){
      out4[i]<- paste0('<h4>The mean percent of days with ramping rates below the ',df_ramp$Name[i], ' was <b>',df_ramp$Result[i],'% </b> of days. The target of of <b>',df_ramp$`Target Goal`[i], '% </b> of days was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out4[i]<- paste0('<h4>The mean percent of days with ramping rates below the ',df_ramp$Name[i], ' was <b>',df_ramp$Result[i],'% </b> of days. The target of of <b>',df_ramp$`Target Goal`[i], '% </b> of days was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }
  }
    
  head5<- '<br><h3>Recreational Flows</h3>'
  
  out5<- vector()
  for (i in 1:nrow(REC)){
    
    if(REC$Status[i] == 'Below'){
      out5[i]<- paste0('<h4>The mean number of ',REC$Name[i], ' boatable days was <b>',REC$Result[i],' days </b>. This was <span style=\"color:orange\"><b>below</b></span> the long-term average of <b>',REC$`Long Term Average`[i],"</b></h4>")
      
    } else{
      out5[i]<- paste0('<h4>The mean number of ',REC$Name[i], ' boatable days was <b>',REC$Result[i],' days </b>. This was <span style=\"color:green\"><b>above or equal</b></span> the long-term average of <b>',REC$`Long Term Average`[i],"</b></h4>")
    }
    
  }
  HTML(paste0(head1,out[1],out[2],out[3],out[4],out[5],out[6], head2, out2[1],out2[2],out2[3],out2[4],head3,out3[1],head4, out4[1], head5, out5[1],out5[2],out5[3],out5[4]))
  
  #HTML(paste0(head1,out[1],out[2],out[3],out[4],out[5],out[6], head2, out2[1],out2[2],out2[3],out2[4],head3,head4, out4[1], head5, out5[1],out5[2],out5[3],out5[4]))
}

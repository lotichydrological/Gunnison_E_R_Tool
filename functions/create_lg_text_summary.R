create_lg_text_summary <- function(EFT, REC){
  
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
  
  head2<- '<br><h3>High Flow Duration</h3>'
  
  df_highflow <- EFT %>% filter(Type =='High Flow Duration')
  out2<- vector()
  for (i in 1:nrow(df_highflow)){
    
    if(df_highflow$Status[i] == 'Not Met'){
      out2[i]<- paste0('<h4>The mean duration of days above the ',df_highflow$Name[i], ' target was <b>',df_highflow$Result[i],' days </b>. The target of a mean duration of <b>',df_highflow$`Target Goal`[i], '</b> days was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out2[i]<- paste0('<h4>The mean duration of days above the ',df_highflow$Name[i], ' was <b>',df_highflow$Result[i],' days </b> in selected years. The target of a mean duration of <b>',df_highflow$`Target Goal`[i], '</b> days was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }
    
  }
  
  head3<- '<br><h3>Baseflow Minimum</h3>'

  df_base <- EFT %>% filter(Type =='Baseflows')
  out3<- vector()
  for (i in 1:nrow(df_base)){

    if(df_base$Status[i] == 'Not Met'){
      out3[i]<- paste0('<h4>The ',df_base$Name[i], ' was met in <b>',df_base$Result[i],'% </b> of selected years. The target of <b>',df_base$`Target Goal`[i], '% </b> of years was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out3[i]<- paste0('<h4>The ',df_base$Name[i], ' was met in <b>',df_base$Result[i],'% </b> of selected years. The target of <b>',df_base$`Target Goal`[i], '% </b> of years was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }

  }
  
    
  head4<- '<br><h3>Recreational Flows</h3>'
  
  out4<- vector()
  for (i in 1:nrow(REC)){
    
    if(REC$Status[i] == 'Below'){
      out4[i]<- paste0('<h4>The mean number of ', REC$Type[i],': '  , REC$Name[i], ' boatable days was <b>',REC$Result[i],' days </b>. This was <span style=\"color:orange\"><b>below</b></span> the long-term average of <b>',REC$`Long Term Average`[i]," days</b></h4>")
      
    } else{
      out4[i]<- paste0('<h4>The mean number of ',REC$Type[i],': '  , REC$Name[i], ' boatable days was <b>',REC$Result[i],' days </b>. This was <span style=\"color:green\"><b>above or equal</b></span> the long-term average of <b>',REC$`Long Term Average`[i]," days</b></h4>")
    }
    
  }
  HTML(paste0(head1,out[1],out[2],out[3],out[4],out[5], head2, out2[1],out2[2],out2[3],out2[4],head3,out3[1],out3[2],out3[3],out3[4],head4, out4[1],out4[2],out4[3],out4[4]))
  
}

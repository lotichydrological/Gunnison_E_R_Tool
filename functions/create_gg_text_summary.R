create_gg_text_summary <- function(EFT, REC){
  
  head1<- '<h3>Trout Maximum Spring Flows</h3>'
  df_max <- EFT %>% filter(Type =='Maximum Spring Flow for Fish')
  out<- vector()
  for (i in 1:nrow(df_max)){
    
    if(df_max$Status[i] == 'Not Met'){
      out[i]<- paste0('<h4>The mean duration of days above the',df_max$Name[i], ' was <b>',df_max$Result[i],' days</b>. The target of a mean duration above maximum flows of <b>',df_max$`Target Goal`[i], '</b> days was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out[i]<- paste0('<h4>The mean duration of days above the',df_max$Name[i], ' was <b>',df_max$Result[i],' days</b>. The target of a mean duration above maximum flows of <b>',df_max$`Target Goal`[i], '</b> days was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")    }
    
  }
  
  head2<- '<br><h3>Trout Habitat Ideal Range</h3>'
  
  df_ideal <- EFT %>% filter(Type =='Trout Habitat Flows')
  out2<- vector()
  for (i in 1:nrow(df_ideal)){
    
    if(df_ideal$Status[i] == 'Not Met'){
      out2[i]<- paste0('<h4>The mean duration of days within the Trout ',df_ideal$Name[i], ' was <b>',df_ideal$Result[i],' days</b>. The target of a mean duration of <b>',df_ideal$`Target Goal`[i], '</b> days was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")
      
    } else{
      out2[i]<- paste0('<h4>The mean duration of days within the Trout ',df_ideal$Name[i], ' was <b>',df_ideal$Result[i],' days</b>. The target of a mean duration of <b>',df_ideal$`Target Goal`[i], '</b> days was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }
    
  }
  
  head3<- '<br><h3>Baseflow Minimum</h3>'

  df_base <- EFT %>% filter(Type =='Baseflow')
  out3<- vector()
  for (i in 1:nrow(df_base)){

    if(df_base$Status[i] == 'Not Met'){
      out3[i]<- paste0('<h4>The mean duration of days above the ',df_base$Name[i], ' was <b>',df_base$Result[i],' days </b> in selected years. The target of a mean duration above baseline of <b>',df_base$`Target Goal`[i], '</b> days was ',"<span style=\"color:red\"><b>Not Met</b></span></h4>")

    } else{
      out3[i]<- paste0('<h4>The mean duration of days above the ',df_base$Name[i], ' was <b>',df_base$Result[i],' days </b> in selected years. The target of a mean duration above baseline of <b>',df_base$`Target Goal`[i], '</b> days was ',"<span style=\"color:blue\"><b>Met</b></span></h4>")
    }

  }
  
    
  head4<- '<br><h3>Recreational Flows</h3>'
  REC <- REC %>% 
    filter(Type != 'Safety') 
  out4<- vector()
  for (i in 1:nrow(REC)){
    
    if(REC$Status[i] == 'Below'){
      out4[i]<- paste0('<h4>The mean number of ', REC$Type[i],': '  , REC$Name[i], ' boatable days was <b>',REC$Result[i],' days </b>. This was <span style=\"color:orange\"><b>below</b></span> the long-term average of <b>',REC$`Long Term Average`[i],"</b></h4>")
      
    } else{
      out4[i]<- paste0('<h4>The mean number of ',REC$Type[i],': '  , REC$Name[i], ' boatable days was <b>',REC$Result[i],' days </b>. This was <span style=\"color:green\"><b>above or equal</b></span> the long-term average of <b>',REC$`Long Term Average`[i],"</b></h4>")
    }
    
  }
  HTML(paste0(head1,out[1],out[2], head2, out2[1],head3,out3[1],head4, out4[1],out4[2],out4[3],out4[4],out4[5],out4[6],out4[7],out4[8],out4[9] ))
  
}

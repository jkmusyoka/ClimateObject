climate$methods(extreme_events=function(data_list=list(), year, required_var=rain_label,na.rm=TRUE,max_min=TRUE,extreme=max,sum_day=1,val_threshold=FALSE,
                                        threshold_value=0,start_day=1, end_day=366){
  
  #required variable
  data_list = add_to_data_info_required_variable_list(data_list, list(required_var))
  # date time period
  data_list = add_to_data_info_time_period(data_list, daily_label)
  
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)
  for(data_obj in climate_data_objs) {
    #Getting the threshold
    #curr_threshold = data_obj$get_meta(threshold_label,threshold)    
    
    #add season column to the data
    if ( !(data_obj$is_present(season_label))) {
      data_obj$add_doy_col()
    }
    #if dos is not in the data create it
    if( !( data_obj$is_present(dos_label) && data_obj$is_present(season_label) ) ) {
      data_obj$add_doy_col()
    }
    #get names of the columns in the data   
    rain_col  = data_obj$getvname(required_var)
    
    dos_col   = data_obj$getvname(dos_label) 
   
    season_col= data_obj$getvname(season_label)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for (curr_data in curr_data_list){
      
      if(missing(year)){  
        warning("Since no years have been specified, we will take the whole years in the data")
        
        year = unique(curr_data[[season_col]])
      }
      else {  year = unique(year)}
      
      mx=list()
      
      thresh=list()
      
      for (yr in year){
        sub=subset(curr_data,curr_data[[season_col]]==yr & curr_data[[dos_col]]>=start_day & curr_data[[dos_col]]<=end_day)
        
        val=rowSums(outer(1:(length(sub[[rain_col]])-sum_day+1),1:sum_day,FUN=function(i,j){sub[[rain_col]][(j - 1) + i]}),na.rm=na.rm)
        
        doy=sub[[dos_col]][sum_day-1+which(val %in% max(val,na.rm=na.rm))]
        if(max_min){
          if (length(doy)>1){
            doy=min(doy) 
            }
          mx[[yr-min(unique(curr_data[[season_col]])-1)]]=c(yr,doy,extreme(val,na.rm=na.rm))          
        }       
        
        if (val_threshold){
          thresh[[yr-min(unique(curr_data[[season_col]])-1)]]=c(yr,doy)          
        }       
      }
    
      if (max_min){
        df <- data.frame(matrix(unlist(mx), nrow=length(year), byrow=T))
        names(df)=c("Year","DOY","Amount")
        return(df)
      }
      if (val_threshold){
        return(thresh)
      }      
    }
  }  
}
)
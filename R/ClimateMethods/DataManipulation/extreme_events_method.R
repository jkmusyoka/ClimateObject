climate$methods(extreme_events=function(data_list=list(),required_var=rain_label,na.rm=T){
  
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
      mx=list()
      for (year in unique(curr_data[[season_col]])){
        sub=subset(curr_data,curr_data[[season_col]]==year)
        
        mx[[year-min(unique(curr_data[[season_col]])-1)]]=c(year,sub[[dos_col]][sub[[rain_col]]==max(sub[[rain_col]],na.rm=na.rm)],max(sub[[rain_col]],na.rm=na.rm))  
        
      }
      df <- data.frame(matrix(unlist(mx), nrow=length(unique(curr_data[[season_col]])), byrow=T))
      names(df)=c("Year","DOY","Amount")
      return(df)
    }
  }  
}
)
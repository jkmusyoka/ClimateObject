climate$methods(boxplot_monthly_daily_rainfall=function( data_list=list(), threshold=0.85, whisklty=1, whiskcol="red", fill_col="blue", connect_median=FALSE, lty_median=1,
                                                          col_median="black", lwd=1, title="Monthly Rainfall Amount",ylab="Rainfall (mm)",xlab="Month"){
  #--------------------------------------------------------------------------------------------#
  # This function plots the boxplot of the daily rainfall observations per month for all the years
  #-------------------------------------------------------------------------------------------#
  
  # rain variable is required for this method
  data_list = add_to_data_info_required_variable_list( data_list, list(rain_label) )
  
  # daily data is required for this method
  data_list=add_to_data_info_time_period( data_list, daily_label )
  
  # use data_list to get the required data objects
  climate_data_objs = get_climate_data_objects( data_list )
  
  for( data_obj in climate_data_objs ){
    
    threshold = data_obj$get_meta_new(threshold_label,missing(threshold),threshold)
    data_name=data_obj$get_meta( data_name_label )
    
    if( ! data_obj$is_present( month_label ) ){
      data_obj$add_year_month_day_cols()
    }
    # Get the title of the column of months
    month_col = data_obj$getvname(month_label)
    rain_col =  data_obj$getvname(rain_label)
    
    # Access data in methods
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      dat <- curr_data[curr_data[[rain_col]] > threshold, c(rain_col,month_col)]
      #print(curr_data[ which(curr_data[[rain_col]]>threshold),])
      mon = month(dat[[month_col]], label=T)
      
      if( connect_median == TRUE) {
        lines(  boxplot(dat[[rain_col]]~mon,whisklty=whisklty,whiskcol=whiskcol,col=fill_col,xlab=xlab,
                        ylab=ylab, main= c(data_name, title) )$stats[3,], col=col_median, lty=lty_median, lwd=lwd )
      }else{
        boxplot(dat[[rain_col]]~mon,whisklty=whisklty,whiskcol=whiskcol,col=fill_col,xlab=xlab,
                ylab=ylab, main= c(data_name, title) )
      }
      
    }
  }
  
} 
)  

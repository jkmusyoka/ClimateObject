#==================================================================================================
# SEASONAL SUMMARIES
#' @title compute seasonal summaries.
#' @name seasonal_summary
#' @author Frederic and Fanuel 2015 (AMI)

#' @description \code{seasonal.summary} 
#' Adds a column of sesonal summaries (e.g rain totals and number of rain days) given climate object
#' 
#' @param data_list list. 
#' 
#' @param seasonal.summary type character. Type of summary to be computed. It can be either
#'  "season", "year". Default: "season"
#  
#' @examples
#' ClimateObj <- climate( data_tables = list( data ), date_formats = list( "%m/%d/%Y" ) )
#' Default dateformats: "%Y/%m/%d"
#' # where "data" is a data.frame containing the desired data to be computed.
#' climateObj$seasonal_summary()
#' @return return columns of seasonal summaries
#' 

climate$methods(seasonal_summary = function(data_list = list(), month_start, number_month = 3, threshold = 0.85, 
                                            col_name = list(c("Rain Total","Number Raindays")), season_rain_total=FALSE, season_rain_days=FALSE,
                                           na.rm = FALSE, replace = FALSE){  
  # rain required
  data_list = add_to_data_info_required_variable_list(data_list, list(rain_label))
  # date time period is "daily"
  data_list = add_to_data_info_time_period(data_list, daily_label)
  # a list of climate data objects
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    curr_threshold = data_obj$get_meta(threshold_label,threshold)
    
    rain_col  = data_obj$getvname(rain_label) 
    
    # Must add month column if not present
    if( !( data_obj$is_present(month_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    
    month_col = data_obj$getvname(month_label)
    
    # must add seasonal column to the data
    if ( !(data_obj$is_present(season_label))) {
      data_obj$add_doy_col()
    }
    season_col = data_obj$getvname(season_label) 
    
    if(missing(month_start)){
      curr_season_start_day = data_obj$get_meta(season_start_day_label)
      year=1952
      date = doy_as_date(curr_season_start_day, year) 
      month_start = month(date)
    }
    col_name0=col_name

    for (period in 1:length(month_start)){
          
      if (is.character(month_start[period])){
        if (!month_start[period] %in% c(month.abb, month.name,tolower(c(month.abb, month.name)))){
          stop("Enter the upper or lower case of English names for the months of the year; e.g Jan,January,jan,january")
        }
        month_start[period]= 1 + ((match(tolower(month_start[period]), tolower(c(month.abb, month.name))) - 1) %% 12)
      }else {
        month_start[period] = month_start[period]
      }
      
      months = 1+(((month_start[period]:(month_start[period]+number_month-1)) -1) %% 12)
      
      
      if (length(month_start) > length(col_name) && period==1){
        col_name = list(paste(col_name[[1]], month_start[1], sep = "_"))
      }
      if (length(month_start) > length(col_name) && period>1){
        col_name = append(col_name, list(paste(col_name0[[1]], month_start[period], sep = "_")))
      }
      
      summary_obj <- get_summary_name(yearly_label, data_obj)
      
      continue = TRUE
      
      curr_definition = list(month_start = month_start[period], number_month = number_month, threshold = threshold)
      labs = c(seasonal_total_label , seasonal_raindays_label)
      conditions =c((season_rain_total || !season_rain_days), (season_rain_days || !season_rain_total))
      for(i in 1:length(col_name[[period]])){
        if(conditions[i]){
          if(col_name[[period]][i] %in% names(summary_obj$get_data()) && !replace) {
            message(paste("A column named", col_name[[period]][i], "already exists. The column will not be replaced.
                          To replace to column, re run this function and specify replace = TRUE."))
            continue = FALSE
          }
          if(col_name[[period]][i] %in% names(summary_obj$get_data()) && replace){
            message(paste("A column named", col_name[[period]][i], "already exists. The column will be replaced 
                          in the data."))
          }
          if( continue && summary_obj$is_definition(rain_label,labs[i],curr_definition)) {
            message("A column with this defintion already exists in the data.
                    The column will not be added again.")
            continue = FALSE
          }
          }
      }
      
      if(continue) {
        
        curr_data_list = data_obj$get_data_for_analysis(data_list)
        
        for( curr_data in curr_data_list ) {
          month_tot=matrix(NA,length(unique(curr_data[[season_col]])),12)
          rownames(month_tot)=as.character(unique(curr_data[[season_col]]))
          colnames(month_tot)=c(month.abb)
          raindays=month_tot
          #   loop over months and years to get summary statistics
          for (mon in months) {
            rain.season = curr_data[curr_data[month_col]==mon,c(season_col,rain_col)]   
            for (yr in unique(curr_data[[season_col]])) {
              if(season_rain_total || !season_rain_days){
                month_tot[yr-min(unique(curr_data[[season_col]])-1),mon]=sum(rain.season[rain.season[,season_col]==yr,rain_col])
              }
              if(season_rain_days || !season_rain_total){
                raindays[yr-min(unique(curr_data[[season_col]])-1),mon]=sum(rain.season[rain.season[,season_col]==yr,rain_col]>threshold)
              }
            }
          }
          month_tot <- rowSums(month_tot[,months], na.rm = na.rm) 
          raindays <-  rowSums(raindays[,months], na.rm = na.rm)
          result = list(month_tot, raindays)
          
        }
        
        for (j in 1:length(col_name[[period]])){
          if (conditions[j]){
            summary_obj$append_column_to_data(result[[j]], col_name[[period]][j])
            label = summary_obj$get_summary_label(rain_label, labs[j],curr_definition)
            summary_obj$append_to_variables(label, col_name[[period]][j])
          }
        }
        
      }
    }
    
    
    }
  }
)
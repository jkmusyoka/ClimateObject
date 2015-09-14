#==================================================================================================================
# DISPLAY DAILY
#' @title Get the tables of daily data
#' @name Display_daily
#' @author Frederic Ntirenganya 2015 (AMI)
#' 
#' @description \code{Display daily data in tables }
#' Display daily data in tables for any variable 
#'  
#' @param data_list list. this is a list containing stations for analysis, the years or periods to be analyzed and the required variables from the data 
#' @param Print_tables Logical,if true, the method print the table in the console
#' @param Row.names Logical, if FALSE the row names attributed to the dataframe are removed
#' @param Na.rm Logical, if true remove the missing value
#' @param Variable  This is the variable to be displayed
#' @param Threshold The least amount of rainfall for which a day is considered rainy
#' @param Month_list The three-letter abbreviations for the English month names
#' @param Day_display Column name showing the day of the month
#'   
#' @examples
#' ClimateObj <- climate( data_tables = list( data ), date_formats = list( "%m/%d/%Y" ) )
#' Default dateformats: "%Y/%m/%d"
#' # where "data" is a data.frame containing the desired variable to be displayed.
#' climateObj$display_daily()
#' @return It returns tables list

climate$methods(display_daily = function(data_list = list(), print_tables = FALSE, month_summary = max, total_name = "Total", max_name = "Maximum", threshold_name = "",
                                         na.rm = FALSE, variable = rain_label, threshold = 0.85, months_list = month.abb, day_display = "Day") {
    
  #required variable
  data_list = add_to_data_info_required_variable_list(data_list, list(variable))
  # data time period is daily
  data_list = add_to_data_info_time_period( data_list, daily_label )
  rettables=list()
  climate_data_objs = get_climate_data_objects( data_list )
  
  out = c()
  for( data_obj in climate_data_objs ) {
    
    curr_threshold = data_obj$get_meta(threshold_label,missing(threshold),threshold)
    
    #get required variable name
    interest_var = data_obj$getvname(variable)
    
    # must add these columns if not present for displaying
    if( !(data_obj$is_present( year_label ) && data_obj$is_present( month_label ) && data_obj$is_present( day_label )) ) {
      data_obj$add_year_month_day_cols()
    }
    year_col = data_obj$getvname( year_label )
    month_col = data_obj$getvname( month_label )
    day_col = data_obj$getvname( day_label )
    
    # access data for analysis
    curr_data_list = data_obj$get_data_for_analysis( data_list )
    for( curr_data in curr_data_list ) {
      
      # initialize tables as a list
      tables = list()
      tables_summary = list()
      tables_merged = list()
      
      # Split curr_data into a list of data frames - one for each year
      years_split <- split( curr_data, list( as.factor( curr_data[[year_col]] ) ) )
      # counter used in the loop
      i = 1
      # loop through the splited data frames 
      for ( single_year in years_split ) {
        # produce table with data
        tables[[i]] <- dcast( single_year, single_year[[ day_col ]] ~ single_year[[ month_col ]], value.var = interest_var)
        
        # Name the columns of the table
        end = length( colnames( tables[[i]] ) )
        names( tables[[i]] )[ 1 ] <- day_display
        colnames( tables[[i]] )[2:end] <- months_list[1:end-1]
        
        #create quick function to count number of obs larger than a certain value which can be run in an apply
        largerthan <- function(x,val, na.rm){
          sum(x>val, na.rm = na.rm)
        }
        #produce second table with summary stats
        tables_summary[[i]] <- suppressWarnings(rbind(colSums(tables[[i]][,-1], na.rm = na.rm), apply(tables[[i]][,-1],2, FUN = month_summary, na.rm = na.rm), apply(tables[[i]][,-1],2,largerthan, val = curr_threshold, na.rm = na.rm)))
        # add dimnames
        if(threshold_name == "") threshold_name = paste("Number >", threshold)
        dinnames = c(total_name, max_name, threshold_name)
        tables_summary[[i]] <- cbind(c(dinnames), tables_summary[[i]])

        # Making dataframe for second table
        tables_summary[[i]] <- data.frame(tables_summary[[i]])
        #add dimnames for the first column.
        colnames(tables_summary[[i]])[1] <- ("Day")
        # merge the tables
        tables_merged[[i]] <- rbind(tables[[i]], tables_summary[[i]])
        i = i + 1
      }
      # Use the years to name each table.
      names( tables_merged ) <- names( years_split )
      # Use the station name to name the list of all years
      rettables[[data_obj$get_station_data(curr_data, station_label)]] = tables_merged  
      
      # Only print if requested
      if (print_tables) print( tables_merged)
      
    }
        
    }  
  return(rettables)
}
)

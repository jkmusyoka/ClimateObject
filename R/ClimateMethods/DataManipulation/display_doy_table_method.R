#==================================================================================================================
# DISPLAY DAY OF THE YEAR 
#' @title Get the table of day of the year
#' @name display_doy
#' @author Frederic Ntirenganya 2015 (AMI)
#' 
#' @description \code{get.table_doy }
#' Display day of the year in a table  
#'  
#' @param data_list  this is a list containing stations for analysis, the years or periods to be analyzed and the required variables from the data 
#' @param Row.names logical, if FALSE the row names attributed to the dataframe are removed 
#' @param Month_list  the three-letter abbreviations for the English month names
#' @param File   the path of the output file
#' @param Width  the width of the output page
#' @param Height   the width of the output page
#' @param Day_display  column name in showing the day of the month
#' @param Font_size  default font size for the document in the points
#' @param Na.string character string of missing value to be displayed in the table
#' @param Save_table Logical, if TRUE save the output file
#' 
#' @examples
#' ClimateObj <- climate( data_table = list( data ), date_formats = list( "%m/%d/%Y" ) )
#' Default dateformats: "%Y/%m/%d"
#' # where "data" is a data.frame containing the desired variable to be displayed.
#' climateObj$display_doy()
#' @return It returns  a table

climate$methods(display_doy = function(data_list = list(), months_list = month.abb, single_year, day_display = "Day", file="DOY.doc",save_table = FALSE,
                                       row.names = FALSE, width=8.5, height=11, font.size=6, title="DOY table", font.size2=10, NA.string=" "){
  
  # data time period.
  data_list = add_to_data_info_time_period( data_list, daily_label )
  #Get the data objects
  climate_data_objs = get_climate_data_objects( data_list )
    
  for( data_obj in climate_data_objs ) {

    # must add these columns if not present for displaying
    if( !(data_obj$is_present( year_label ) && data_obj$is_present( month_label ) && data_obj$is_present( day_label )) ) {
      data_obj$add_year_month_day_cols()
    }
    year_col = data_obj$getvname( year_label )
    month_col = data_obj$getvname( month_label )
    day_col = data_obj$getvname( day_label )
    # must add doy_col if not present
    if( !(data_obj$is_present( doy_label )) ) {
      data_obj$add_doy_col()
    }
    doy_col = data_obj$getvname( doy_label )
    
    curr_data_list = data_obj$get_data_for_analysis( data_list )
    
    for( curr_data in curr_data_list ) {
      # subset curr_data into single data frame for a specific year 
      # note that all years have equal day of the year
      # It returns a data.frame of the first year in the data 
      first_year<-subset(curr_data, curr_data[[year_col]]==min(unique(curr_data[[year_col]])))
      # produce table with day of the year
      table <- dcast( first_year, first_year[[ day_col ]] ~ first_year[[ month_col ]], value.var = doy_col)
      # Added day_display and months_list as extra arguments so it is more flexible
      end = length( colnames( table ) )
      names( table )[ 1 ] <- day_display
      colnames( table )[2:end] <- months_list[1:end-1]  
    }
    #Always print table
    print( table, row.names = row.names )
    #Some one might want this file. 
    if(save_table==TRUE){
      #set output file
      rtf<-RTF(file=file, width=width, height=height, font.size=font.size)
      #add title
      addHeader(rtf, title=title, font.size=font.size2)
      #add table
      addTable(rtf, table, NA.string=NA.string, row.names=row.names)
      #save output file
      done(rtf)
      
    }
    
  }  
}
)

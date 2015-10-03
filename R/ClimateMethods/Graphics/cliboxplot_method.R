#==================================================================================================
# 
#' @title Boxplot of daily rainfall per month.
#' @name cliboxplot
#' @author Fanuel 2015 (AMI)

#' @description \code{Box plot} 
#' produces box-and-whisker plot(s).
#'  
#' @param whisklty Whisker line type.
#' @param fill_col Box color.
#' @param whiskcol Color of whisker.
#' @param connect_median A logical scalar. Should the medians be connected by a line?
#' @param col_median Color of the line connecting the medians.
#' @param lty_median Median line type.
#' @param lwd Line width of the median line.
#' @param title Boxplot title.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' 
#' @examples
#' ClimateObj <- climate( data_tables = list( dataframe=dataframe ), date_formats = list( "%m/%d/%Y" ) )
#' Default dateformats: "%Y/%m/%d"
#' where "data" is a data.frame containing the desired data to be computed.
#' climateObj$boxplot_monthly_daily_rainfall(). 
#' @return return box-and-whisker plot(s).
#'

climate$methods(cliboxplot=function( data_list=list(),var=rain_label,factor_level=TRUE, factor=month_label, threshold=0.85, whisklty=1, whiskcol="red",
                                                         fill_col="blue",lwd=1, title="Monthly Rainfall Amount",ylab="Rainfall (mm)",xlab="Month",
                                                         range = 1.5, width = NULL, varwidth = FALSE,notch = FALSE, outline = TRUE, plot = TRUE,
                                                         border = par("fg"), col = NULL, log = "",pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
                                                         horizontal = FALSE, add = FALSE, at = NULL,names=month.abb){
  #--------------------------------------------------------------------------------------------#
  # This function plots the boxplot of the daily rainfall observations per month for all the years
  #-------------------------------------------------------------------------------------------#
  
  # rain variable is required for this method
  data_list = add_to_data_info_required_variable_list( data_list, list(var))  
    
  # use data_list to get the required data objects
  climate_data_objs = get_climate_data_objects( data_list )
  
  for( data_obj in climate_data_objs ){
    
    threshold = data_obj$get_meta(threshold_label,missing(threshold),threshold)
    data_name=data_obj$get_meta( data_name_label )
    
    if( ! data_obj$is_present( month_label ) ){
      data_obj$add_year_month_day_cols()
    }
    # Get the title of the column of months
    factor_col = data_obj$getvname(factor)
    var_col =  data_obj$getvname(var)
    # Access data in methods
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    for( curr_data in curr_data_list ) {
      
      if( factor_level == TRUE) {
        curr_dat <- curr_data[curr_data[[var_col]] > threshold, c(var_col,factor_col)]
        boxplot(curr_dat[[var_col]]~curr_dat[[factor_col]],whisklty=whisklty,whiskcol=whiskcol,col=fill_col,xlab=xlab,
                ylab=ylab, main= c(data_name, title),range = range, width = width, varwidth = varwidth,
                notch = notch, outline = outline, plot = plot,border = border, log = log,
                pars = pars,horizontal = horizontal, add = add, at = at,names=names )
      }else{
        curr_dat <- curr_data[curr_data[[var_col]] > threshold,c(var_col)]
        boxplot(curr_dat,whisklty=whisklty,whiskcol=whiskcol,col=fill_col,xlab=xlab,
                ylab=ylab, main= c(data_name, title),range = range, width = width, varwidth = varwidth,
                notch = notch, outline = outline, plot = plot,border = border, log = log,
                pars = pars,horizontal = horizontal, add = add, at = at )
      }
      
    }
  }
  
} 
)  

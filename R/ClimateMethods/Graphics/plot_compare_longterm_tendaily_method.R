#=============================================================================
#' Compare yearly long term mean with 10-daily totals
#' @title Compare yearly long term mean with ten-daily totals
#' @name plot_compare_longterm_tendaiily_totals
#' @author Abib Duut 2015 

#' @description \code{plot_compare_longterm_tendaiily_totals} 
#' this plots the long term mean against ten daily totals
#' 
#' @param data_list list. 
#' 
#' @param time_period This is can be 
#' "10-daily", "season", "year". Default value: "10-daily" 
#  
#' @examples
#' climObj <- climate(data_tables=list(data),date_format="%Y-%m-%d")
#' # where "data" is a data.frame containing the desired data to be plotted.
#' climObj$plot_compare_longterm_tendaiily_totals()
#' @return  a comparison plot
#' 
climate$methods(plot_compare_longterm_tendaily_totals = function(data_list = list(), time_period="10-daily", year=1960,
                                        rain_threshold=0.85){  
  #data required 
  data_list=add_to_data_info_required_variable_list(data_list, 
                                                    	list(var_label))
  #date time period is "daily" todo check it works for all time periods
  data_list=add_to_data_info_time_period(data_list, daily_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
  
  	for(data_obj in climate_data_objs) {
	    data_name = data_obj$get_meta(data_name_label)    
	    date_col = data_obj$getvname(date_label)
    	    curr_threshold = data_obj$get_meta(var_threshold_label,var_threshold)
    	    var_col  = data_obj$getvname(var_label)
            station_id_col = data_obj$getvname(station_label)
            print(station_id_col)

#Get the data frames for analysis
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    # loop for computing 
    for( curr_data in curr_data_list ) {
      #subset by year.
      curr_data<-subset(curr_data, year)
      
    	#split rain values in to groups of 10 for ten daily totals.
	dat1<-split(curr_data[[var_col]], ceiling(seq_along(curr_data[[var_col]])/10))
	dat2<-lapply(dat1, sum)

	#split date values in to groups of 10 for the axis.
	deta<-split(curr_data[[date_col]], ceiling(seq_along(curr_data[[date_col]])/10))

	ndte<-NULL
		for(i in 1:length(deta)){
			dte<-as.Date(deta[[i]][10])
			ndte<-as.Date(rbind(ndte,dte))
		}

	#compute monthly mean for each year.	
	curr_data$Year<-year(curr_data[[date_col]])
	curr_data$Month<-month(curr_data[[date_col]])
	m_tot<-ddply(curr_data,.(Year,Month),summarise,sum(as.numeric(Rain),na.rm=T))
	names(m_tot)<-c("Year","Month","M_Total")
	m_stat<-ddply(m_tot,.(Month),summarise,mean(M_Total,na.rm=T))
	names(m_stat)<-c("Month" ,"Mean")
	
	plot(ndte, dat2)
	lines(m_stat)
		
				}
			}
		}
)		

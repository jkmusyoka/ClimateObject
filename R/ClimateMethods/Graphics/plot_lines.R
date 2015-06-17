# After looking and findout that the function ggplot can plot multiple columns on the same plot,we can adopt it for multiple variables
#=========================================================================================================

climate$methods(plot_lines = function(data_list=list(), interest_var, data_period_label = yearly_label)
{   
  require(ggplot2)
  require(reshape)
  
  # get_climate_data_objects returns a list of the climate_data objects specified
  # in the arguments.
  # If no objects specified then interest_var climate_data objects will be taken by default
  
  # Can I use a list of variables? Yes . 
  # The analysis should take account of the structure of the data.
  data_list = add_to_data_info_required_variable_list(data_list, interest_var) 
  data_list = add_to_data_info_time_period(data_list, data_period_label) 
  
  climate_data_objs_list = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs_list) {
    
    # we need to get the column of interest for the plot.
    # since the column of interest is a list, the loop gets interest_var at the same time.
    interest_variable =list()
    for(i in 1:length(interest_var)){
      
      interest_variable[[i]] <- data_obj$getvname(interest_var[[i]]) 
    }
    #print(interest_variable)
    
    
    date_col = data_obj$getvname(date_label)
    
    #adding year column if not present 
    if( !(data_obj$is_present(year_label)) ) {
      data_obj$add_year_col()
    }
    year_col = data_obj$getvname(year_label)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      # subset the data. Here get only time period and the interest variables 
      dat <- subset(curr_data, select=c( year_col, interest_variable = as.character(interest_variable)))
      print(head(dat))
      #Melt the data into a form suitable for easy casting
      dat2 <- melt(dat ,  id = 'Year')
      #       print(head(dat2))
             print(names(dat2))
      dat2$Year <-as.factor(dat2$Year) # factor
      dat2$value <- as.integer(dat2$value) # integer
      #       print(class(dat2$Year))
      #       print(class(dat2$variable))
      #       print(class(dat2$value))
      #"Year"     "variable" "value"
      # plot interest_var variables on the same graph
      # Need to read more about ggplot bcse here it is not plotting.
      #?ggplot() is typically used to construct a plot incrementally.
      ggplot(data = dat2, aes(x = Year, y = value, group=1)) + geom_line(aes(colour = variable))+
        ggtitle("Start of the Rain by Year")
      
    }
    
  }
}
)

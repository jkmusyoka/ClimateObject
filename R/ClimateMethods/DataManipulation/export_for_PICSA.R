#' Export data for PICSA
#' @title Output data in the form needed by PICSA.
#' @name export_for_PICSA
#' @author Abib Duut 2015
#' @description \code{export_for_PICSA} 
#' creates a file with data in the format required for PICSA. 





climate$methods(export_for_PICSA =function(data_list = list(), month_start = 1, number_month = 3, threshold = use_default_label, 
                                            summaries = list(sum_label, count_label, mean_label),
                                            use_threshold_as_lower = c(FALSE, TRUE, TRUE), strict_threshold = FALSE,
                                            longest_dry_spell = TRUE, longest_dry_spell_name = "Longest dry spell", spell_length_name = "Spell Length",
                                            na.rm = FALSE, replace = FALSE, month_col_names = "Season_A", 
                                            summary_col_names = c("Total Rain", "Number of rainy days", "Mean rain per rainy day"),...) 
    

{  
      #first call the seasonal summary method with the right agruments
     print("1")
      data_list<-seasonal_summary.rain(data_list = data_list, month_start = month_start, number_month =number_month, threshold = threshold, summaries = summaries, use_threshold_as_lower = use_threshold_as_lower, strict_threshold = strict_threshold,longest_dry_spell = longest_dry_spell, longest_dry_spell_name = longest_dry_spell_name, spell_length_name = spell_length_name, na.rm = na.rm, replace = replace, month_col_names = month_col_names, summary_col_names = suammary_col_names)
      # date time period is "yearly"
      data_list = add_to_data_info_time_period(data_list, yearly_label)
      print("1ii")
      # a list of climate data objects
      climate_data_objs = get_climate_data_objects(data_list)
  print("2")
      
	  for(data_obj in climate_data_objects){
	      curr_data_list = data_obj$get_data_for_analysis(data_list)
		  for( curr_data in curr_data_list ) {

  	    #extracting the yearly summaries.
	      names(curr_data)<-c( "Year","TotalRainfall","TotalRainfall_SeasonA","TotalRainfall_SeasonB",
		    "SeasonStart_A","SeasonStart_B","SeasonEnd_A","SeasonEnd_B","LengthOfSeason_A",
		    "LengthOfSeason_B","MinTemperature","MaxTemperature")
        write.csv(dframe, file=Sitename.csv,sep = ",",column.names=T, row.names = F,quote = F)
        }  
	    }
    
  }
)









    






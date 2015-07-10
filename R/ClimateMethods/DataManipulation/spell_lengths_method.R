
climate$methods(spell_lengths=function(data_list=list(), interest_season, doy_m, threshold= 0.85, print_table=TRUE, months_list=month.abb)
  
  {
  data_list = add_to_data_info_required_variable_list(data_list, list(rain_label))
  
  data_list=add_to_data_info_time_period(data_list, daily_label)
  
  climate_data_objs_list = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs_list){
    if( missing( doy_m ) ) {
      warning( "Since no range of values has been specified, the whole year will be considered.")
      doy_m =list(c( 1, 366 ))
    }else {
      if( !( class( doy_m) == "list" ) ){ stop( "The input doy_m should be of type list, consiting of elemnts of length 2")}
    }
    
    threshold = data_obj$get_meta_new(threshold_label,missing(threshold),threshold)
    #add season column to the data
    if ( !(data_obj$is_present(season_label))) {
      data_obj$add_doy_col()
    }
    #if doy or dos is not in the data create it
    if( !( data_obj$is_present(dos_label) && data_obj$is_present(season_label) ) ) {
      data_obj$add_doy_col()
    }
    
    # add month column if not present
    if( !( data_obj$is_present(month_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    
    #add day column if not present
    if( !( data_obj$is_present(day_label)) ) {
      data_obj$add_year_month_day_cols()
    }
    # get names of columns in the data
    rain_col  = data_obj$getvname(rain_label)   
    
    dos_col   = data_obj$getvname(dos_label)  
    
    season_col= data_obj$getvname(season_label) 
    
    month_col = data_obj$getvname(month_label)
    
    day_col   = data_obj$getvname(day_label)
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ){
      
      if(missing(interest_season)){  warning("Since no years have been specified, we will take the whole years in the
                              data set by default") 
                          interest_season = unique(curr_data[[season_col]])
      }
      else {  interest_season = unique(interest_season)}
           
      
      # initialise the variable which will contain the results
      # --------------------------------------------------------------#
      dry_spell = list()
      my = c()  
      tables=list()
      for( season in interest_season) {
        
        if( !(season %in% unique(curr_data[[season_col]]) ) ) {
          warning(paste0("There is no data for the season ", season))
          dry_spell[season-(min(interest_season)-1)] = NA
        }
        else{         
          
          # ----------------------------------------------#
          #Subset the data for a particular year
          # ----------------------------------------------#          
          season_data <-  curr_data[  curr_data[[season_col]]==season,  c(dos_col, rain_col, season_col, month_col, day_col)  ]
          season_data_1 <-  curr_data[  curr_data[[season_col]]==(season-1), c(dos_col, rain_col, season_col, month_col, day_col)  ]    
          #print(season_data_1)
          # ---------------------------------------------------------------------------------------------------#
          # Add a column called spell_length to season_data and season_data_1 which will contain 1 if that day is dry and 0 otherwise
          # ---------------------------------------------------------------------------------------------------#
          season_data$Spell_length <- "--"
                    
          # ----------------------------------------------------#
          #Get all the indices where all the obsevations are NA
          # ------------------------------------------------------#
          indic = which(season_data[,2] %in% NA)
          indic_1 = which(season_data_1[,2] %in% NA)
          
          s = which( indic %in% 60)
          s_1 = which( indic_1 %in% 60)
          
          # ----------------------------------------------#
          #Assign all the other NA's observations to 0
          # -----------------------------------------------#
          if( length(indic) >1 && length(s) >= 1) {
            season_data[ indic[-s], 2] <- 0  }
          else if(  length( indic ) >= 1 ) {  
            season_data[ indic, 2] <- 0  }
          
          if( length(indic_1) >1  && length(s_1) >= 1) {
            season_data_1[ indic_1[-s_1], 2] <- 0 }
          else  if(  length( indic_1 ) >= 1 ){  
            season_data_1[ indic_1, 2] <- 0  }
          
          # ---------------------------------------------------------------#
          #if 60 is among those indices then remove that observation
          # --------------------------------------------------------------#
          if(60 %in% indic){
            season_data <- season_data[-60 , ]
          }
          
          if(60 %in% indic_1){
            season_data_1 <- season_data_1[-60, ]
          }
          
          #loop through elements in doy_m and get the maximum spell length 
          # for the different period required for that year or for all the years
          
          period_m = c()
                 
          for( j in 1 : length(doy_m) ) {
            
            period = doy_m[[ j ]]          
            
            if( !(length( period )==2) ){stop( paste0( "Please enter only a vector of length 2 for element ",j, 
                                                       " in input doy_m. The first for the start of the  period and the second for the end.") ) }
            
            if( period[1] >= period[2] ){ stop( paste0( "Please the day of season corresponding to the start of the period for which the longest ", "",
                                                        "dry spell is required, should be less than the day of year corresponding to the end ", "", 
                                                        "of the period for element ",j, " in input doy_m.") )  } 
            
            # -------------------------------------------------------------------------------------------#
            #Take the indic in season_data, where the day of year is doym[1] and another one where it is doym[2]  
            # -------------------------------------------------------------------------------------------#
            ind1 = which( season_data[[ 1 ]] %in% period[ 1 ] )
            ind2 = which( season_data[[ 1 ]] %in% period[ 2 ] )
            
            # --------------------------------------------------#
            #subset season_data within the range of ind1 and ind2
            # ---------------------------------------------------#
            season_data_doy = season_data[ ind1:ind2, ]
            
            # ---------------------------------------------------------------------#
            #Now we assign the column vector of the season_data_doy to a variable
            # ----------------------------------------------------------------------#
            column_var = season_data_doy[ , 2 ]
                       
            season_data_2 = season_data
            
            # ----------------------#
            #initialise the count 
            # ------------------------#
            count = 1
            doym = period[1]
            
            
            # -------------------------------------------------------------------------------------------#
            #first check if the year is the first year in the data            
            # -------------------------------------------------------------------------------------------#
            dat = season_data
            first_year = FALSE
            mx = c() 
            if( season == unique(curr_data[[season_col]])[1] |  !( (season-1) %in% unique(curr_data[[season_col]]) ) ) {
              first_year = TRUE
              my = append( season, my)
              # -----------------------#
              #Start the while loop
              # ------------------------#
              
              while(count == 1){
                
                if(doym==60 & (60 %in% indic)){doym=59}
                
                if(doym ==1){
                  break
                }
                
                if( dat[ dat[[1]] == doym, 2] <= threshold ){ 
                  ind = which( dat[[ 1 ]] %in% doym )
                  if( dat[ ( ind - 1 ), 2 ] <= threshold){
                    column_var =c(0, column_var)
                    doym = doym-1
                  }
                  else {count =-1}
                }
                else {count=-2}
                
              }
              
              # ---------------------------------------------------#
              #Assign all the values in column_var whose values are <= 0.85
              # ------------------------------------------------------#
              column_var[column_var <= threshold] <- 0
              
              # ----------------------------------------#
              #Then use the following function on column_var
              # -----------------------------------------#
              zeroes <- function(column_var){
                (!(column_var)) * unlist(lapply(rle(column_var)$lengths, seq_len))
              } 
              
              Vec = zeroes(column_var)
              if( Vec[1] == 1 && period[1] == 1 ){ 
                mx = append( max(Vec), mx )
                zj = Vec %in% 0
                ij = which( zj %in% TRUE )
                Vec[ 1: (ij[1] - 1) ] <- "m"
              }     
              period_m[j] =  max(  as.numeric( Vec[ !( Vec == "m" ) ] ) )
              
            } 
            # Incase those two conditions are not satisfied 
            else{
              
              while(count == 1){
                
                if(doym==60 & (60 %in% indic)){doym=59}
                
                if(doym ==1){
                  if( dat[ dat[[ 1 ]] == doym, 2 ] <= threshold ){
                    column_var = c( 0, column_var )
                    dat = season_data_1
                    doym = dat[ dim( dat )[ 1 ], c( 1 ) ]
                  }
                }
                
                if( dat[ dat[[ 1 ]] == doym, 2 ] <= threshold ){ 
                  ind = which( dat[[ 1 ]] %in% doym )                  
                  if( dat[ ( ind - 1 ) , 2 ] <= threshold ){
                    column_var =c(0, column_var)
                    doym = doym-1
                  }
                  else {count =-1}
                }
                else {count=-2}
                
              }
              # ---------------------------------------------------#
              #Assign all the values in column_var whose values are <= 0.85
              # ------------------------------------------------------#
              column_var[column_var <= threshold] <- 0
              
              # ----------------------------------------#
              #Then use the following function on column_var
              # -----------------------------------------#
              zeroes <- function(column_var){
                (!(column_var)) * unlist(lapply(rle(column_var)$lengths, seq_len))
              }
              
              Vec = zeroes(column_var)
           
              period_m[j] =  max(Vec)
            }       
            
            
            # -------------------------------------------------------------------------------#
            # fill the column Dry_spell in season_data with values from the zeroes function if 
            # the spell length of the whole year has been required
            # -------------------------------------------------------------------------------#
            if( ( period[ 1 ] == 1 && period[ 2 ] == 366 && print_table ) | ( missing( doy_m ) && print_table ) ) {
              # -------------------------------------------------------------------------------------------#
              # if the length of the vector Vec is the equal to the row number of season_data_2, do the following
              # -------------------------------------------------------------------------------------------#
              if( length( Vec ) == dim( season_data_2 )[ 1 ] ) {
                season_data_2$Spell_length = Vec
              }
              else{         
                season_data_2$Spell_length = Vec[-( 1 : ( length( Vec ) - dim(season_data_2)[1] ) )] 
              }
              #print(season_data_2)
              
              season_data_2[[month_col ]] = format(season_data_2[[ month_col ]], levels=month.abb)             
              #print(season_data_2)
              tables[[season-min(unique(interest_season)-1)]] = dcast( season_data_2, season_data_2[[ day_col ]]~season_data_2[[month_col ]], value.var = "Spell_length")
              names(tables[[season-min(unique(interest_season)-1)]]) <-c("Day" ,months_list)
              
        
              
              #print(  paste0( "The table containing the different dry spell for each period in the year ", season, " is: " ), quote = FALSE  )
                    
                                    
            }
            
            
          }
          
          
        }
        dry_spell[season-(min(interest_season)-1)] = list(c(season,period_m))       
      } 
      if ( ( period[ 1 ] == 1 && period[ 2 ] == 366 && print_table ) | ( missing( doy_m ) && print_table ) ){
        table=tables[-(which(sapply(tables,is.null),arr.ind=TRUE))]
        return(table)
      }else{         
        return(dry_spell)
      }
    }
   }
}
)
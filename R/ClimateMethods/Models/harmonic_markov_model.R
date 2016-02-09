#==================================================================================================
# END OF RAINS
#' @title Computational of end of the rain with water balance definition
#' @name add_end_rain
#' @author Danny, Frederic and Fanuel 2015 (AMI)

#' @description \code{compute.end_rain} 
#' compute end of the rains given climate object
#' 
#' @param data_list	A list containing stations for analysis, the years or periods to be analysed and the required variables from the data.
#'  If blank, the system will choose all data appropriate for the analysis.
#' @param earliest_day  The earliest possible day for the end of the rains.
#' @param water_balance_col_name  The column name to use for the water balance column if it needs to be created. 
#' @param col_name  The column name to use for the end of the rains.
#' @param capacity_max	The maximum water balance.
#' @param evaporation  	Evaporation per day
#' @param Replace  	Logical indicating whether the column should be replaced if there is already a column in the data with the value of col_name.
#  
#' @examples
#' ClimateObj <- climate( data_tables = list( data ), date_formats = list( "%m/%d/%Y" ) )
#' Default dateformats: "%Y/%m/%d"
#' # where "data" is a data.frame containing the desired data to be computed.
#' climateObj$add_end_rain()
#' @return return end of the rain 
#' 
climate$methods(harmonic_probability_rain_model<-function(data_list=list(),fac=NULL,h_order=4,weights=NULL, parallel=FALSE, separate_station_models=TRUE) {
  
  data_list=add_to_data_info_required_variable_list(data_list, rain_label)
  data_list=add_to_data_info_time_period(data_list, daily_label)
  if(!separate_station_models || !data_obj$is_present(station_label)) {
    data_list=add_to_data_info_merge(data_list, TRUE)
  } 
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    
    #if doy or year/dos season is not in the data frame, create it.
    if( !( data_obj$is_present(dos_label) && data_obj$is_present(season_label) ) ) {
      data_obj$add_doy_col()
    }
    dos_col_name = data_obj$getvname(dos_label)
    
    if(!data_obj$is_present(rain_day_label)) data_obj$add_rain_day_column()
    rain_day_col_name = data_obj$getvname(rain_day_label)
    
    if(is.null(fac)) {
      model_string <- paste("factor() ~ ",harmonic(dos_col_name,366,h_order)) #Creates string for default case, no factor.
    }
    else {	
      if(parallel) {
        model_string <- paste("factor(", rain_day_col_name, ") ~", harmonic(dos_col_name,366,h_order),"+",fac) #The parallel case.
      }
      else { 
        model_string <- paste("factor(", rain_day_col_name, ") ~", harmonic(dos_col_name,366,h_order),"*",fac)
      }
    }
    
    curr_data_list=data_obj$get_data_for_analysis(data_list)
    
    for( curr_data in curr_data_list ) {
      
    }
    fit1<-glm(data =  ,as.formula(model_string), family=binomial, weights=NULL, na.action=na.exclude) #Glm fits the data by a binomial and computes the coefficients of the model.
    
  }
}
)

climate$methods(harmonic_markov_amount_rain_model<-function(data,fac=NULL,h_order=4,weights=NULL, parallel=FALSE) {
  
  md<-model(h_order,fac,parallel)  #This calls the model function below.

  fit1<-glm(as.formula(md), family=binomial, weights=NULL, na.action=na.exclude) #Glm fits the data by a binomial and computes the coefficients of the model.

  
  subdata<-subset(data,wet_or_dry=="w") #This subsets a portion of the data.
  
  n_rain<-subdata$Rain
  
  attach(subdata, warn.conflicts=FALSE) #This creates a directory with the subset data.
  
  md<-model1(h_order,fac,parallel) #Calls the model funtion for amounts of rainfall. 
  
  
  fit1<-glm(as.formula(md), family=Gamma, weights=NULL, na.action=na.exclude) #This fits the data by a gamma distribution and computes the coefficients of the model.
  
  detach(subdata)
  fit1 #This is the returned model.
}
)

fit_model<-function(data,fac=NULL,h_order=4,weights=NULL, parallel=FALSE) {
		
	attach(data,warn.conflicts=FALSE)   #This creates a directory with data attached for easy call of variables.
	
	md<-model(h_order,fac,parallel)  #This calls the model function below.
	
	
	fit1<-glm(as.formula(md), family=binomial, weights=NULL, na.action=na.exclude) #Glm fits the data by a binomial and computes the coefficients of the model.
	
	detach(data) #This deletes the created directory.
	fit1         #This returns the fitted model.
	}
	
	#Returns a fitted model for amount of rainfall.
	
fit_amts<-function(data,fac=NULL,h_order=3,weights=NULL, parallel=FALSE) {

	subdata<-subset(data,wet_or_dry=="w") #This subsets a portion of the data.
	
	n_rain<-subdata$Rain
	
	attach(subdata, warn.conflicts=FALSE) #This creates a directory with the subset data.
	
	md<-model1(h_order,fac,parallel) #Calls the model funtion for amounts of rainfall. 
	
	
	fit1<-glm(as.formula(md), family=Gamma, weights=NULL, na.action=na.exclude) #This fits the data by a gamma distribution and computes the coefficients of the model.
	
	detach(subdata)
	fit1 #This is the returned model.
	}

#These functions generates the string for the model to be fitted.
#There are two options; the parallel model and the interacting model.
#in both cases the functions returns the string for the model to be fitted. 
#model1 returns the string for rainfall amounts, and model returns the string for chance of rainfall.

model1 <- function(h_order=4,fac=NULL,parallel=FALSE) {
  
	if(is.null(fac)) {
		fm1 <- paste("as.numeric(n_rain) ~ ",harmonic(h_order)) #Creates string for default case, no factor.
	}
  else {	
		if(parallel) {
			fm1 <- paste("as.numeric(n_rain) ~ ",harmonic(h_order),"+",fac) #The parallel case.
		}
		else {
			fm1 <- paste("as.numeric(n_rain) ~ ",harmonic(h_order),"*",fac) #Including the interacting.
		}
	}
	fm1	#The returned string for the model.
	
}	

harmonic<-function(circ_col, circ_length,h_order=4){
	sc<-NULL #Initializes sc variable.
	for(i in 1:h_order){
		sc<-c(sc,paste("cos(",circ_col,"*",i,"*2*pi/",circ_length,")"))  #(a)these two lines generates the harmonic cols for the perodicity of the data.  
		sc<-c(sc,paste("sin(",circ_col,"*",i,"*2*pi/",circ_length,")"))
	}
	ret=paste("(",paste(sc,collapse="+"),")")
	ret
 }
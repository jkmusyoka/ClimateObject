
climate$methods(cliplot = function(data_list=list(),var_x,var_y, type = "p",  xlim = NULL, ylim = NULL,par=TRUE,plot_window=FALSE,gpar=par(mfrow=c(2,2)),
                                    log = "", main="plot",station_name=TRUE,data_time_period=yearly_label, sub = NULL, xlab = NULL, ylab = NULL,col=c("blue","green"),
                                    ann = par("ann"), axes = TRUE, frame.plot = axes,by_func=FALSE,factor_var,
                                    panel.first = NULL, panel.last = NULL, asp = NA, ...){    
    
  data_list = add_to_data_info_required_variable_list(data_list,  list(var_x, var_y)) 
  
  data_list=add_to_data_info_time_period(data_list, data_time_period)
  #Get the data objects
  climate_data_objs_list = get_climate_data_objects(data_list)
  for(data_obj in climate_data_objs_list) {    
    var_x_name = data_obj$getvname(var_x)
    var_y_name = data_obj$getvname(var_y)
    par(new=FALSE)
    if (station_name){    
      data_name = data_obj$get_meta(data_name_label)
    }else {
      data_name=NULL
    }
   
    curr_data_list = data_obj$get_data_for_analysis(data_list)      
    
    for( curr_data in curr_data_list ) {      
      
      if (plot_window){
        gpar
      }
      
      if (by_func==TRUE && length(var_y)==1){
       
        factor_split <- split(curr_data[,c(var_x,var_y)], list(as.factor(curr_data[[factor_var]])))
        j=1
        for (fact in factor_split){
          plot(fact[[var_x]], fact[[var_y]], type =type,  xlim = xlim, ylim = ylim,log =log, main = main, sub = sub, 
               xlab = xlab,ylab = ylab,col=col[j],ann =ann, axes = axes, frame.plot = axes,panel.first = panel.first, 
               panel.last =panel.first, asp = asp, ...)
        }
        j+1
      }else {        
        for (i in 1:length(var_y)){
          
          if (i>1 && par==TRUE){
            xlab=""
            ylab=""
            axes=FALSE
          }else{
            axes=TRUE
            xlab=NULL
            ylab=NULL
          }
          plot(curr_data[[var_x_name]], curr_data[[var_y_name[i]]], type =type,  xlim = xlim, ylim = ylim,
               log =log, main=c(data_name,main), sub = sub, xlab = xlab, ylab = ylab,col=col[i],
               ann =ann, axes = axes, frame.plot = axes,panel.first = panel.first, panel.last =panel.first, asp = asp) 
          
          par(new=par)
        }        
      }      
    }
  }
par(mfrow=c(1,1))
}
)
#Need to do include the factor variable 

climate$methods(cliplot = function(data_list=list(),var_x,var_y, type = "p",  xlim = NULL, ylim = NULL,par=TRUE,
                                    log = "", main, sub = NULL, xlab = NULL, ylab = NULL,col=c("blue","green"),
                                    ann = par("ann"), axes = TRUE, frame.plot = axes,by_func=FALSE,factor_var,
                                    panel.first = NULL, panel.last = NULL, asp = NA, ...){    
  
  # get_climate_data_objects returns a list of the climate_data objects specified in the arguments.
  # If no objects specified then all climate_data objects will be taken by default.
  
  data_list = add_to_data_info_required_variable_list(data_list,  list(var_x, var_y)) 
  #Get the data objects
  climate_data_objs_list = get_climate_data_objects(data_list)

  for(data_obj in climate_data_objs_list) {
    
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    
    if (missing(main)){
      data_name = data_obj$get_meta(data_name_label)
      main=c(data_name, "plot")
    }
    
    for( curr_data in curr_data_list ) {      
      par(new=FALSE)
      if (by_func==TRUE && length(var_y)==1){
#         pplot=function(){
#           plot(curr_data[[var_x]], curr_data[[var_y]], type =type,  xlim = xlim, ylim = ylim,
#                                                    log =log, main = main, sub = sub, xlab = xlab, ylab = ylab,col=col,
#                                                    ann =ann, axes = axes, frame.plot = axes,panel.first = panel.first, panel.last =panel.first, asp = asp, ...)
#         }
#         by(curr_data, curr_data[[factor_var]],pplot)
        fac = split(curr_data[[var_y]], list(as.factor(curr_data[[factor_col]])))
#         for (j in fac){
#           plot(curr_data[[var_x]], curr_data[[var_y[[j]]]], type =type,  xlim = xlim, ylim = ylim,
#                                                                    log =log, main = main, sub = sub, xlab = xlab, ylab = ylab,col=col,
#                                                                   ann =ann, axes = axes, frame.plot = axes,panel.first = panel.first, panel.last =panel.first, asp = asp, ...)
#         }
      }else {        
        for (i in 1:length(var_y)){
          
          if (i>1 && par==TRUE){
            xlab=""
            ylab=""
            axes=FALSE
          }
          plot(curr_data[[var_x]], curr_data[[var_y[i]]], type =type,  xlim = xlim, ylim = ylim,
               log =log, main = main, sub = sub, xlab = xlab, ylab = ylab,col=col[i],
               ann =ann, axes = axes, frame.plot = axes,panel.first = panel.first, panel.last =panel.first, asp = asp, ...) 
          
          par(new=par)
        }        
      }      
    }
  }
}
)
#Need to do include the factor variable using the by function 
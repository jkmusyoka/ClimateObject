
climate$methods(cliplot = function(data_list=list(),var_x,var_y,linetype=1,color=c("blue"),merge_data=FALSE,
                                    xlabel=xlab(""), ylabel=ylab(""), main_title=" ",size=0,xlim = NULL, ylim = NULL, wise = NULL,
                                    stat = "identity", position = "identity", show.legend = NA, inherit.aes = TRUE,factor_var,
                                    station_name=TRUE,value.name="value",variable.name = "variable", na.rm = FALSE){    
  
  data_list = add_to_data_info_required_variable_list(data_list,  list(var_x, var_y)) 
  data_list= add_to_data_info_merge(data_list,merge_data)
  #Get the data objects  
  climate_data_objs_list = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs_list) {    
    var_x_name = data_obj$getvname(var_x)
    var_y_name=c()
    for (i in 1:length(var_y)){
      var_y_name[i] = data_obj$getvname(var_y[i])
    }   
    
    if (station_name){    
      data_name = data_obj$get_meta(data_name_label)
    }else {
      data_name=NULL
    }
    
    main_title2=labs(title=paste(data_name,main_title))     
    curr_data_list = data_obj$get_data_for_analysis(data_list)
    #print(curr_data_list)
    if (merge_data){       
      p.plot <- ggplot(data= curr_data_list,aes_string(x=var_x_name, y=var_y_name, group=factor_var,colour=factor_var))+
        geom_line(linetype=linetype,stat = stat, position = position, show.legend = show.legend,
                  inherit.aes = inherit.aes, size=size) + xlabel+ylabel+main_title2 +
        scale_colour_manual(values=color)+ coord_cartesian(xlim = xlim, ylim = ylim, wise = wise)             
    }else{      
      for( curr_data in curr_data_list ) {      
        curr_data=subset(curr_data,select=c(var_x_name, var_y_name))
        curr_data <- melt(curr_data, id.vars=var_x_name, value.name=value.name,variable.name = variable.name, na.rm = na.rm) 
        p.plot <- ggplot(data= curr_data,aes_string(x=var_x_name, y=value.name, group=variable.name,colour=variable.name))+
          geom_line(linetype=linetype,stat = stat, position = position, show.legend = show.legend,
                    inherit.aes = inherit.aes, size=size) + xlabel+ylabel+main_title2 +
          scale_colour_manual(values=color)+ coord_cartesian(xlim = xlim, ylim = ylim, wise = wise)                     
      }
    }
    return(p.plot)
  }
}
)
#  Inventory Plot 
#' @title Inventory Plot
#' @name plot_inventory
#' @author Abib Duut 2015
#' 
#' @description \code{inventory plot}
#' This displays the data present for multiple stations, which helps to m 
#'  
#' @return an inventory plot for mutiple stations.

climate$methods(plot_inventory = function (data_list=list(), col1="blue",ylab,xlab="Year",na.rm=TRUE, pch=20,ylim=0,type="b",lty=2,col2="red",lwd = 2,lwd2 = 1.5,
                                                   var_label = rain_label,plot_line = FALSE,ygrid=0, graph_parameter = par(mar=c(6,8,4,2)),plot_window = FALSE,
                                                   main_title="Inventory Data Plot: Rain"){

  # convert data 
  data_list = c(data_list, convert_data=TRUE)
  
  # time period and station
  data_list = add_to_data_info_time_period(data_list, date_label, station_label)
  
  climate_data_objs = get_climate_data_objects(data_list)
  
  for(data_obj in climate_data_objs) {
    data_name = data_obj$get_meta(data_name_label)
    
    # Must add these columns if not present 
    if( !(data_obj$is_present(date_label) ) ) { 
      data_obj$add_date_col() 
    }
    date_col = data_obj$getvname(date_label)
    
    #interset_var_col = data_obj$getvname ("Total Rain") 
    
    if(missing(ylab)){
      ylab = data_obj$getvname(var_label)
    }
    
    data_list[[merge_data_label]]=TRUE
    curr_data = data_obj$get_data_for_analysis(data_list)
    
    #Create binary field indicating whether variable of interest (Rain) is missing or non-missing
    curr_data$val<-as.numeric(is.na(curr_data[[rain_col]]))
    
    #Stations will be plotted from bottom to top but we want alphatically first to be on the top so sort stations into reverse alphabetical order. 
    curr_data<-curr_data[rev(order(curr_data[[station_col]])),]
    
    #reshape data into 1 row per day, 1 column per station, with values as calculated previously
    curr_data<-reshape(curr_data[,c("station","Date","val")],timevar="station",idvar="Date",v.names="val",direction="wide")
    
    #where value is NA after reshape the station did not have a row for that date in the input - this is also missing data so overwrite accordingly.
    curr_data[is.na(curr_data)]<-1
    
    #sort by date
    curr_data<-curr_data[[order(curr_data[[Date]]),]]
    
    if (plot_window){   
      par = graph_parameter 
    } 
    # for the plot 
      plot_inventory <- image(x=curr_data[[date_col]],y=1:(ncol(curr_data)-1),as.matrix(curr_data[,-1]),yaxt="n",ylab="",col=c("black","gray90"),xlab="",main= c( data_name, main_title))
                        #add white spaces to help delineate the groups
                        segments(x0=min(curr_data[[date_col]]),x1=max(curr_data[[date_col]]),y0=seq(0.5,ncol(curr_data)+0.5,by=1),col="white")
                        #add labels
                        text(x=min(curr_data[[date_col]]),y=1:(ncol(curr_data)-1),rev(sort(levels(ADD$Station))),xpd=T,pos=2,cex=0.75)
      
  }
}
)
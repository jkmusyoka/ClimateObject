#' Read multiple filenames and load files into R.
#'This was inspired by work for James K. Musyoka.
#' @title read and load multiple files.  
#' @name read_load_multifile()
#' @author Abib Duut 2015 
#'
#'@description \code{read_load_multifile} 
#' This reads and loads data from a directory on your computer into R.
#' It is particularly useful when loading data for multiple stations, 
#' and when creating a climate object for multiple stations. 
#' It writes a file to the user's current working directory in the users specified format, 
#' .csv or .txt.
#' @param filename
#' format this could one of the two flat file formats .csv, .txt
#' 
#' @examples
#'        read_load_multistat()
#' @return This writes a file with the filename passed in by the user to the user's current directory.
#' 
#'------------------------------------------------------------------------------------------------
#'------------------------------------------------------------------------------------------------

read_load_multistat<-function( filename="", format= "csv"){
                      f<-list.files()  
                      f<-as.data.frame(f)
                      f<-f[1]
                      mv<-paste('read.', format, '("',f$f, '", header=T)' , sep='')
                      write.table(noquote(mv), file=as.character(filename), quote=F, row.names=F, sep=",")
                      
}
  
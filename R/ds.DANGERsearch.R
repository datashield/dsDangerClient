#' 
#' @title lists all R environments on serverside
#' @description creates a list of the names of all of the serverside
#' R environments
#' @details Clientside function \code{ds.DANGERsearch} calling serverside aggregate
#' function \code{DANGERsearchDS}. Only argument to be specified is datasouces.
#' ds.DANGERsearch is currently classified
#' as a DANGER function. This is because it is primarily of value to developers
#' and for other users to be proactively presented with the names of all of the serverside 
#' R environments may not be a good strategy from the perspective of optimising security.
#' In the future, if there is a demand, this could be converted to an open function.
#' @param datasources specifies the particular 'connection object(s)' to use.
#' e.g. if you have several data sets in the sources you are working with
#' called opals.a, opals.w2, and connection.xyz, you can choose which of
#' these to work with. The call 'datashield.connections_find()' lists all of
#' the different datasets available and if one of these is called 'default.connections'
#' that will be the dataset used by default if no other dataset is specified. If you
#' wish to change the connections you wish to use by default the call
#' datashield.connections_default('opals.a') will set 'default.connections'
#' to be 'opals.a' and so in the absence of specific instructions to the contrary
#' (e.g. by specifiying a particular dataset to be used via the <datasources>
#' argument) all subsequent function calls will be to the datasets held in opals.a.
#' If the <datasources> argument is specified, it should be set without
#' inverted commas: e.g. datasources=opals.a or datasources=default.connections.
#' The <datasources> argument also allows you to apply a function solely to a subset
#' of the studies/sources you are working with. For example, the second source
#' in a set of three, can be specified using a call such as datasources=connection.xyz[2].
#' On the other hand, if you wish to specify solely the first and third sources, the
#' appropriate call will be datasources=connections.xyz[c(1,3)]
#' @return a vector of character strings to the clientside giving the names of
#' each of the R environments on the serverside in order of their numeric "pos"
#' @author Paul Burton 2020
#' @export
ds.DANGERsearch <- function(datasources=NULL){
  
   if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }


  # call the server side function
  calltext <- call("DANGERsearchDS")

  output <- DSI::datashield.aggregate(datasources, calltext)
  
  return(output)
  }
# ds.DANGERsearch



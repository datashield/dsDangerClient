#' 
#' @title Returns any object to the client side
#' @description DANGER - special function to allow analysts to look at server side objects
#' on the client side
#' @details this function is not meant to be used anywhere except in a development environment
#' It is designed to give the analyst access to the data (i.e. a route through the DataSHIELD
#' defences)
#' @param x a character, the name of a server side object
#' @return the server side object requested
#' @author Bishop T.
#' @export
#' 
ds.danger = function(x=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the object required!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  cally <- paste0("dangerDS(", x, ")")
  object <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
    return(object)
    
}

#' 
#' @title Returns any object to the client side
#' @description DANGER - special function to allow analysts to look at server side objects
#' on the client side
#' @details this function is not meant to be used anywhere except in a development environment
#' It is designed to give the analyst access to the data (i.e. a route through the DataSHIELD
#' defences)
#' @param x a character, the name of a server side object
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the server side object requested
#' @author Bishop T.
#' @export
#' 
ds.danger = function(x=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the object required!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- dsBaseClient:::extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- dsBaseClient:::isDefined(datasources, varname)
  }else{
    defined <- dsBaseClient:::isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsBaseClient:::checkClass(datasources, x)

  cally <- paste0("dangerDS(", x, ")")
  object <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  return(object)
    
}

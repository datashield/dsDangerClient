#' @title DANGER function 
#' @description DANGER function
#' @details DANGER function
#' @param x.name character string naming first object to be
#' printed on clientside
#' from serverside to clientside
#' @param y.name character string naming second object to be
#' printed on clientside
#' @param z.name character string naming third object to be 
#' printed on clientside
#' @param datasources specifies the particular 'connection object(s)' to use.
#' @return DANGER function
#' @author DataSHIELD Development Team
#' @export
ds.DANGERprint<-function(x.name=NULL, y.name=NULL, z.name=NULL, datasources=NULL) {

  # details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(x.name)){
    return("PLEASE SPECIFY THE NAME OF AT LEAST ONE VARIABLE IN as.character FORMAT FOR QA ONLY")
  }


  if(!is.character(x.name)){
    return("PLEASE SPECIFY THE NAME OF AT LEAST ONE VARIABLE IN as.character FORMAT FOR QA ONLY")
  }



  calltext <- call('DANGERprintDS', x.name, y.name, z.name)

  extract.vars <- DSI::datashield.aggregate(datasources, calltext)


  return(extract.vars=extract.vars)

}

# ds.DANGERprint


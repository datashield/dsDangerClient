#'
#' @export
#'
ds.DANGERseedEXTRACT <- function(datasources=NULL){
  
  # details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  calltext <- call('DANGERseedEXTRACTDS')
  
  extract.seed <- DSI::datashield.aggregate(datasources, calltext)
  
  return(extract.seed=extract.seed)
  
}

# ds.DANGERseedEXTRACT


ds.DANGERseedEXTRACT <- function(datasources=NULL){
  
  # details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  
  calltext <- call('DANGERseedEXTRACTDS')
  
  extract.seed <- opal::datashield.aggregate(datasources, calltext)
  
  return(extract.seed=extract.seed)
  
}

# ds.DANGERseedEXTRACT

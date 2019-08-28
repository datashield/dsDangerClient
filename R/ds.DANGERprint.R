#'
#' @export
#'
ds.DANGERprint<-function(x.name=NULL, y.name=NULL, z.name=NULL, datasources=NULL) {

  # details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }

  if(is.null(x.name)){
    return("PLEASE SPECIFY THE NAME OF AT LEAST ONE VARIABLE IN as.character FORMAT FOR QA ONLY")
  }


  if(!is.character(x.name)){
    return("PLEASE SPECIFY THE NAME OF AT LEAST ONE VARIABLE IN as.character FORMAT FOR QA ONLY")
  }



  calltext <- call('DANGERprintDS', x.name, y.name, z.name)

  extract.vars <- opal::datashield.aggregate(datasources, calltext)


  return(extract.vars=extract.vars)

}

# ds.DANGERprint


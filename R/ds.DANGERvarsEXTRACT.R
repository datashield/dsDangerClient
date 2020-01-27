#'
#' @export
#'
ds.DANGERvarsEXTRACT<-function(x.name=NULL, y.name=NULL, z.name=NULL, u.name=NULL, v.name=NULL, w.name=NULL, datasources=NULL) {

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


  calltext <- call('DANGERvarsEXTRACTDS', x.name, y.name, z.name, u.name, v.name, w.name)

  extractVARS <- DSI::datashield.aggregate(datasources, calltext)

  numsources<-length(datasources)

  cat("list[[1]] = study.specific\nlist[[2]] = all.studies\n\n")


  ALL.SOURCES.COMBINED.VARS<-NULL
  SOURCE.ID<-NULL
  for(r in 1:numsources){
    ALL.SOURCES.COMBINED.VARS<-rbind(ALL.SOURCES.COMBINED.VARS,extractVARS[[r]])
    SOURCE.ID<-c(SOURCE.ID,rep(r,dim(extractVARS[[r]])[1]))
  }
  ALL.SOURCES.COMBINED.VARS<-data.frame(cbind(ALL.SOURCES.COMBINED.VARS,SOURCE.ID))


  return(list(study.specific.VARS=extractVARS,all.studies.VARS=ALL.SOURCES.COMBINED.VARS))

}

# ds.DANGERvarsEXTRACT

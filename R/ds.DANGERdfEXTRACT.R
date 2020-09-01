#'
#' @export
#'
ds.DANGERdfEXTRACT<-function(dataframeName=NULL, extract.study.specific=TRUE, extract.all.studies.combined=TRUE, datasources=NULL) {

  # details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }

  if(is.null(dataframeName)){
    return("PLEASE SPECIFY THE NAME OF A VALID data.frame TO EXTRACT FOR QA PURPOSES ONLY")
  }

  if(!is.character(dataframeName)){
    return("PLEASE SPECIFY A VALID data.frame IN as.character FORMAT")
  }

  calltext <- call('DANGERdfEXTRACTDS', dataframeName)

  extractDF <- DSI::datashield.aggregate(datasources, calltext)


  numsources<-length(datasources)


  ALL.SOURCES.COMBINED.DF<-NULL
  SOURCE.ID<-NULL

  #ALL SOURCES COMBINED
  for(r in 1:numsources){
    ALL.SOURCES.COMBINED.DF<-rbind(ALL.SOURCES.COMBINED.DF,extractDF[[r]])
    SOURCE.ID<-c(SOURCE.ID,rep(r,dim(extractDF[[r]])[1]))
  }

  ALL.SOURCES.COMBINED.DF<-data.frame(cbind(ALL.SOURCES.COMBINED.DF,SOURCE.ID))

  #SOURCE BY SOURCE
  for(r in 1:numsources){
    SOURCE.ID<-rep(r,dim(extractDF[[r]])[1])
    extractDF[[r]]<-data.frame(extractDF[[r]])
    extractDF[[r]]<-cbind(extractDF[[r]])
  }

  # cat("list[[1]] = study.specific\nlist[[2]] = all.studies\n\n")

  if(extract.study.specific==TRUE&&extract.all.studies.combined==TRUE)
  {
    return(list(study.specific.df=extractDF,all.studies.df=ALL.SOURCES.COMBINED.DF))
  }

  if(extract.study.specific==TRUE&&extract.all.studies.combined==FALSE)
  {
    return(list(study.specific.df=extractDF))
  }

  if(extract.study.specific==FALSE&&extract.all.studies.combined==TRUE)
  {
    return(list(all.studies.df=ALL.SOURCES.COMBINED.DF))
  }

  if(extract.study.specific==FALSE&&extract.all.studies.combined==FALSE)
  {
    warning.message<-("No data frame generated - at least one of extract.study.specific or extract.all.studies.combined arguments should be TRUE")

    return(list(warning.message=warning.message))
  }

}

# ds.DANGERdfEXTRACT

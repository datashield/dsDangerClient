#' @title DANGER function returning data.frame (df) from processing servers to
#' clientside - *** DEPRECATED BY ds.DANGERdmtEXTRACT *** 
#' @description Copies a df from data servers to clientside. 
#' @details *** DEPRECATED BY ds.DANGERdmtEXTRACT ***
#' @param dataframeName character string that specifies the name of the df 
#' to be copied from the serverside to the clientside 
#' @param extract.study.specific logical defaulted to TRUE which means that the
#' df from each processing source will be combined and returned as a single object.
#' If FALSE this will not be returned. If the <extract.study.specific> and the
#' <extract.all.studies.combined> arguments are both TRUE separate and
#' combined dmts will be returned.
#' @param extract.all.studies.combined  logical defaulted to TRUE which means that the
#' dfs from each processing source will be returned as a separate object. If FALSE
#' these will not be returned
#' @param datasources specifies the particular 'connection object(s)' to use.
#' @return the serverside df specified by name dfName as a
#' data.frame/matrix/tibble on the clientside.
#' @author DataSHIELD Development Team
#' @export
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

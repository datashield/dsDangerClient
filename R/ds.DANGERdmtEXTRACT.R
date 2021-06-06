#' @title DANGER function returning data.frame, Matrix or tibble (DMT) from
#' processing servers to clientside
#' @description Copies a DMT from data servers to clientside. 
#' @details ds.DANGERdmtEXTRACT calling serverside aggregate function DANGERdmtEXTRACTDS.
#' DANGER FUNCTION: must not be installed on data processing servers holding
#' real data. ONLY TO BE USED FOR DEVELOPMENT PURPOSES
#' @param dfName is a character string that specifies the name of the DMT 
#' to be copied from the serverside to the clientside 
#' @param dfClass is an optional character string specifying that, once the 
#' serverside dmt has been returned to the clientside, you force
#' ds.DANGERdmtEXTRACT to configure it as a 'data.frame' 'matrix' or 'tibble'
#' with aliases 'df', 'mat' and 'tbl'. If no value is specified it treats the
#' incoming dmt as a data.frame.
#' @param extract.study.specific logical defaulted to TRUE which means that the
#' dmt from each processing source will be combined and returned as a single object.
#' If FALSE this will not be returned. If the <extract.study.specific> and the
#' <extract.all.studies.combined> arguments are both TRUE separate and
#' combined dmts will be returned.
#' @param extract.all.studies.combined  logical defaulted to TRUE which means that the
#' dmts from each processing source will be returned as a separate object. If FALSE
#' these will not be returned
#' @param datasources specifies the particular 'connection object(s)' to use.
#' @return the serverside dmt specified by name dfName as a
#' data.frame/matrix/tibble on the clientside.
#' @author Paul Burton for DataSHIELD Development Team - 4th June, 2021
#' @export

ds.DANGERdmtEXTRACT<-function(dfName=NULL, dfClass="df",extract.study.specific=TRUE, extract.all.studies.combined=TRUE, datasources=NULL) {

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  

if(is.null(dfName)){
	return("PLEASE SPECIFY THE NAME OF A VALID data.frame, matrix or tibble TO EXTRACT FOR QA PURPOSES ONLY")
}	

if(!is.character(dfName)){
	return("PLEASE SPECIFY THE NAME OF A VALID data.frame, matrix or tibble IN as.character FORMAT")
}	
 

calltext <- call('DANGERdmtEXTRACTDS', dfName)

extractDF <- DSI::datashield.aggregate(datasources, calltext)



numsources<-length(datasources)


ALL.SOURCES.COMBINED.DF<-NULL
SOURCE.ID<-NULL

#ALL SOURCES COMBINED
for(r in 1:numsources){
ALL.SOURCES.COMBINED.DF<-rbind(ALL.SOURCES.COMBINED.DF,extractDF[[r]])
SOURCE.ID<-c(SOURCE.ID,rep(r,dim(extractDF[[r]])[1]))
}


if(dfClass=="df"||dfClass=="data.frame")
{
ALL.SOURCES.COMBINED.DF<-data.frame(cbind(ALL.SOURCES.COMBINED.DF,SOURCE.ID))
}

if(dfClass=="mat"||dfClass=="matrix")
{
  ALL.SOURCES.COMBINED.DF<-as.matrix(cbind(ALL.SOURCES.COMBINED.DF,SOURCE.ID))
}

if(dfClass=="tbl"||dfClass=="tibble")
{
  ALL.SOURCES.COMBINED.DF<-dplyr::tibble(cbind(ALL.SOURCES.COMBINED.DF,SOURCE.ID))
}

#SOURCE BY SOURCE
if(dfClass=="df"||dfClass=="data.frame")
{
  for(r in 1:numsources){
  SOURCE.ID<-rep(r,dim(extractDF[[r]])[1])
  extractDF[[r]]<-data.frame(extractDF[[r]])
  extractDF[[r]]<-cbind(extractDF[[r]],SOURCE.ID)
  }
}

if(dfClass=="mat"||dfClass=="matrix")
{
  for(r in 1:numsources){
    SOURCE.ID<-rep(r,dim(extractDF[[r]])[1])
    extractDF[[r]]<-as.matrix(extractDF[[r]])
    extractDF[[r]]<-cbind(extractDF[[r]],SOURCE.ID)
  }
}

if(dfClass=="tbl"||dfClass=="tibble")
{
  for(r in 1:numsources){
    SOURCE.ID<-rep(r,dim(extractDF[[r]])[1])
    extractDF[[r]]<-cbind(extractDF[[r]],SOURCE.ID)
    extractDF[[r]]<-dplyr::tibble(extractDF[[r]])
  }
} 

cat("list[[1]] = study.specific\nlist[[2]] = all.studies\n\n")

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

# ds.DANGERdmtEXTRACT 


#'
#' @export
#'
ds.DANGERplot<-function(x.name=NULL, y.name=NULL, z.name=NULL, datasources=NULL) {

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



  calltext <- call('DANGERvarsEXTRACTDS', x.name, y.name, z.name)

  extractDF <- DSI::datashield.aggregate(datasources, calltext)

  numsources<-length(datasources)


  ALL.SOURCES.COMBINED.DF<-NULL
  SOURCE.ID<-NULL
  for(r in 1:numsources){
    ALL.SOURCES.COMBINED.DF<-rbind(ALL.SOURCES.COMBINED.DF,extractDF[[r]])
    SOURCE.ID<-c(SOURCE.ID,rep(r,dim(extractDF[[r]])[1]))
  }
  ALL.SOURCES.COMBINED.DF<-data.frame(cbind(ALL.SOURCES.COMBINED.DF,SOURCE.ID))

  P<-extractDF
  Q<-ALL.SOURCES.COMBINED.DF



  graphics::par(mfrow=c(4,3),ask=TRUE)
  #ESC cancels figure construction if it sticks


  for(j in 1:numsources)
  {
    if(dim(P[[j]])[2]==1)
    {
      graphics::hist(P[[j]][,1],ann=FALSE)
      graphics::title(main=paste0("Hist source ",j," ",x.name))
    }

    if(dim(P[[j]])[2]==2)
    {
      graphics::hist(P[[j]][,1],ann=FALSE)
      graphics::title(main=paste0("Hist source ",j," ",x.name))
      graphics::hist(P[[j]][,2],ann=FALSE)
      graphics::title(main=paste0("Hist source ",j," ",y.name))
      graphics::plot(P[[j]][,2],P[[j]][,1],ann=FALSE)
      graphics::title(main=paste0("xPlot source ",j),xlab=y.name,ylab=x.name)
    }

    if(dim(P[[j]])[2]==3)
    {
      graphics::hist(P[[j]][,1],ann=FALSE)
      graphics::title(main=paste0("Hist source ",j," ",x.name))
      graphics::hist(P[[j]][,2],ann=FALSE)
      graphics::title(main=paste0("Hist source ",j," ",y.name))
      graphics::hist(P[[j]][,3],ann=FALSE)
      graphics::title(main=paste0("Hist source ",j," ",z.name))
      graphics::plot(P[[j]][,2],P[[j]][,1],ann=FALSE)
      graphics::title(main=paste0("xPlot source ",j),xlab=y.name,ylab=x.name)
      graphics::plot(P[[j]][,3],P[[j]][,1],ann=FALSE)
      graphics::title(main=paste0("xPlot source ",j),xlab=z.name,ylab=x.name)
      graphics::plot(P[[j]][,3],P[[j]][,2],ann=FALSE)
      graphics::title(main=paste0("xPlot source ",j),xlab=z.name,ylab=y.name)
    }


  }

  #PLOT ALL SOURCES

  if(dim(Q)[2]==(1+1))
  {
    graphics::hist(Q[,1],ann=FALSE)
    graphics::title(main=paste0("Hist ALL  ",x.name))
  }

  if(dim(Q)[2]==(2+1))
  {
    graphics::hist(Q[,1],ann=FALSE)
    graphics::title(main=paste0("Hist ALL  ",x.name))
    graphics::hist(Q[,2],ann=FALSE)
    graphics::title(main=paste0("Hist ALL  ",y.name))
    graphics::plot(Q[,2],Q[,1],ann=FALSE)
    graphics::title(main=paste0("xPlot ALL "),xlab=y.name,ylab=x.name)
  }

  if(dim(Q)[2]==(3+1))
  {
    graphics::hist(Q[,1],ann=FALSE)
    graphics::title(main=paste0("Hist ALL  ",x.name))
    graphics::hist(Q[,2],ann=FALSE)
    graphics::title(main=paste0("Hist ALL  ",y.name))
    graphics::hist(Q[,3],ann=FALSE)
    graphics::title(main=paste0("Hist ALL  ",z.name))
    graphics::plot(Q[,2],Q[,1],ann=FALSE)
    graphics::title(main=paste0("xPlot ALL "),xlab=y.name,ylab=x.name)
    graphics::plot(Q[,3],Q[,1],ann=FALSE)
    graphics::title(main=paste0("xPlot ALL "),xlab=z.name,ylab=x.name)
    graphics::plot(Q[,3],Q[,2],ann=FALSE)
    graphics::title(main=paste0("xPlot ALL "),xlab=z.name,ylab=y.name)
  }

  return(list(studyDF=extractDF,allDF=ALL.SOURCES.COMBINED.DF))

}

# ds.DANGERplot


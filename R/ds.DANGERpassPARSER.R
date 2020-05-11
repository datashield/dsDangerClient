#' @title determines how a given R object or character string is managed by the DataSHIELD parser
#' @description presents an R object or character string to the DataSHIELD R parser
#' and determines whether it is passed by the parser and, if so, what form it
#' takes after it has traversed the parser. 
#' @details Clientside function {ds.DANGERpassPARSER} calling serverside aggregate function
#' {DANGERpassPARSERDS} which presents an R object or character string to the DataSHIELD R parser
#' and determines whether it is passed or blocked by the parser. If it passes,
#' it then explores what form it
#' takes after it has traversed the parser. If it is blocked by the parser the call to
#' ds.DANGERpassPARSER returns an error (Error : Client error: (400) Bad Request).
#' In order to review a number of different objects (e.g. a range of different characters)
#' it is recommended that you add a try() wrapper to the call to the clientside function
#' within a recursive loop in a quality assurance (QA = testing) script. For example:
#' out.message<-try(ds.passParser(transmit.object=object.2.transmit)).
#' The output object (out.message) will then either be the error message,if the
#' parser blocked the object it was presented with, or the R object itself and its class
#' if the parser passed the object. This is currently classified as a DANGER function.
#' This is because it is primarily of value to developers
#' and for other users to be proactively presented with the details of
#' parser functioning may not be a good strategy from the perspective of optimising security.
#' In the future, if there is a demand, this could be converted to an open function.
#' @param transmit.object this specifies the R object or character string to be tested.
#' Note that if a given R object is converted to character format and is then converted back
#' to its original form using a call to eval(parse(text=.)) on the serverside then the way
#' that the parser will manage it may well differ from the way it would have dealt with
#' the R object in its original form.
#' @param datasources specifies the particular 'connection object(s)' to use.
#' Because every server should have the same parser, regardless which connection object(s)
#' you choose to use, this argument is not really important for this function. If
#' you wish to know more about this argument please see help for another clientside
#' function (e.g. the {ds.ls} function. 
#' @return information about whether the input object/string gets blocked or passed
#' by the parser. If it passes the parser, the object itself and a record of its class
#' are passed back to the clientside. 
#' @author Paul Burton (2020)
#' @export
ds.DANGERpassPARSER <- function(transmit.object=NULL, datasources=NULL){
  
   if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # call the server side function that does the job
  calltext <- call("DANGERpassPARSERDS",transmit.object=transmit.object)
  output <- DSI::datashield.aggregate(datasources, calltext)
  
  return(output)
  }
#ds.DANGERpassPARSER


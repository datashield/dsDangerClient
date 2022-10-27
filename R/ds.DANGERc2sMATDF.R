#' @title DANGER FUNCTION copying clientside matrix to serverside
#' @description Creates a matrix A on the serverside from matrix A on the clientside 
#' @details ds.DANGERc2sMATDF calling assign function DANGERc2sMATDFDS.
#' This is a DANGER FUNCTION because it gives the clientside analyst
#' a lot of power to create a data structure that is precise and complex
#' on the severside. In fact this may be absolutely fine - the general rule is that
#' concerns about disclosure relate entirely to aggregate functions rather than
#' assign functions. However, for the moment this wil be kept as a DANGER FUNCTION
#' and could later be considered for conversion to a standard function, if requested.
#' This DANGER function was created from the ds.matrix function.
#' A number of the parameters specifying the matrix to be generated
#' are fixed by the nature of the clientside matrix
#' itself rather than by explicitly stating them as arguments. In consequence, 
#' they have been removed from the list of arguments and are instead given invariant
#' values in the first few lines of code. These include: from<-"clientside.matdf",
#' nrows.scalar=NULL, ncols.scalar=NULL, byrow = FALSE. The <from> argument
#' was originally a character string specifying the source and nature of <mdata>. Because
#' this DANGER function is only intended to copy a clientside matrix or data.frame to the
#' serverside, its value is fixed to "clientside.matdf". The <nrows.scalar>
#' and <ncols.scalar> are fixed empirically by the number of rows and columns of
#' the matrix to be transferred. <byrow> specifies writing the serverside matrix by
#' columns or rows and this is always set as by column (i.e. FALSE).  
#' @param mdata is a character string that specifies the name of the matrix or
#' data.frame to be copied from the clientside to the serverside
#' @param dimnames A dimnames attribute for the matrix: NULL or a list of length 2 giving
#' the row and column names respectively. An empty list is treated as NULL,
#' and a list of length one as row names only. If NULL (the default) the column names
#' of the original <mdata> data.frame or matrix are taken across to specify the 
#' column names of the serverside matrix 
#' @param newobj A character string specifying the name of the matrix to which the output
#' is to be written. If no <newobj> argument is specified or it is NULL
#' the output matrix names defaults to "new_matrix"
#' @param datasources specifies the particular 'connection object(s)' to use.
#' e.g. if you have several data sets in the sources you are working with
#' called opals.a, opals.w2, and connection.xyz, you can choose which of
#' these to work with. The call 'datashield.connections_find()' lists all of
#' the different datasets available and if one of these is called 'default.connections'
#' that will be the dataset used by default if no other dataset is specified. If you
#' wish to change the connections you wish to use by default the call
#' datashield.connections_default('opals.a') will set 'default.connections'
#' to be 'opals.a' and so in the absence of specific instructions to the contrary
#' (e.g. by specifiying a particular dataset to be used via the <datasources>
#' argument) all subsequent function calls will be to the datasets held in opals.a.
#' If the <datasources> argument is specified, it should be set without
#' inverted commas: e.g. datasources=opals.a or datasources=default.connections.
#' The <datasources> argument also allows you to apply a function solely to a subset
#' of the studies/sources you are working with. For example, the second source
#' in a set of three, can be specified using a call such as datasources=connection.xyz[2].
#' On the other hand, if you wish to specify solely the first and third sources, the
#' appropriate call will be datasources=connections.xyz[c(1,3)]
#' @return the object specified by the <newobj> argument (or default name "new_matrix")
#' which is written as a data.frame to the serverside. In addition,
#' two validity messages are returned indicating whether <newobj> has been created in each
#' data source and if so whether it is in a valid form.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
#'
ds.DANGERc2sMATDF<-function(mdata = NA,dimnames = NULL, newobj=NULL, datasources=NULL){
  
from<-"clientside.matdf"
nrows.scalar<-NULL
ncols.scalar<-NULL
byrow <- FALSE

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
#    datasources <- findLoginObjects()
	datasources <- DSI::datashield.connections_find()
  }
  
  # check if a value has been provided for mdata
  if(is.null(mdata)){
    return("Error: mdata must be a character string, a numeric vector or a scalar")
  }

  # if no value spcified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- "new_matrix"
  }

  #process mdata to make transmittable depending on <from>
  #Check that valid from has been specified
#  if(from!="serverside.vector"&&from!="serverside.scalar"&&from!="clientside.scalar"&&from!="clientside.matdf")
#  {
#  cat("            FAILED: <from> must be specified as one of the following - 'serverside.vector',
#        'serverside.scalar', 'clientside.scalar'\n\n")
#  return('Please respecify')
#  }

#  if(from=="serverside.vector"||from=="serverside.scalar")
#  {
#  mdata.transmit<-mdata
#  }
 
  
#  if(from=="clientside.scalar")
#  { 
# mdata.transmit<-paste0(as.character(mdata),collapse=",")
#  }


if(from=="clientside.matdf")
  {
  if(is.data.frame(mdata))
	{
      mdata.mat<-as.matrix(mdata)
	}

  if(is.matrix(mdata))
	{
      mdata.mat<-mdata
	}
	
	
  if(!is.matrix(mdata.mat))
	{
	cat("\n            FAILED: <mdata> must either be a data.frame or a matrix")	
  return('Please respecify')
	}
	
	
  
#  if(!is.null(nrows.scalar)||!is.null(ncols.scalar))
#		{
#return.message<-"matrix dimensions set by dim(mdata), nrows.scalar and ncols.scalar ignored" 
#		return(return.message)
#		}

	nrows.scalar<-dim(mdata.mat)[1]
	ncols.scalar<-dim(mdata.mat)[2]	 

nrows.transmit<-paste0(as.character(nrows.scalar),collapse=",")
ncols.transmit<-paste0(as.character(ncols.scalar),collapse=",")

colclass.vector<-rep("",ncols.scalar)

for(k in 1:ncols.scalar)
{
colclass.vector[k]<-class(mdata.mat[,k])
	if(all(c("data.frame") %in% class(mdata)))
	{
	colclass.vector[k]<-class(mdata[,k])
	}
}

colclass.transmit<-paste0(as.character(colclass.vector),collapse=",")

colnames.vector<-colnames(mdata)
colnames.transmit<-paste0(as.character(colnames.vector),collapse=",")

	
		
mdata.mat.transmit<-paste0(as.character(mdata.mat),collapse=",")

#identify and strip " " (i.e. spaces which get blocked by parser)

#convert all of mdata.mat.transmit into single elements along a character vector
	mdata.mat.transmit.elements<-unlist(strsplit(mdata.mat.transmit, split=""))

length.all<-length(mdata.mat.transmit.elements)
length.no.spaces<-length(mdata.mat.transmit.elements[mdata.mat.transmit.elements!=" "])


mdata.mat.transmit.elements.no.spaces<-rep("",length.no.spaces)

string.pos<-0

for(n in 1:length.all)
{
  if(mdata.mat.transmit.elements[n]==" ")
  {
  string.pos<-string.pos
  }
  else
  {
  string.pos<-string.pos+1
  mdata.mat.transmit.elements.no.spaces[string.pos]<-mdata.mat.transmit.elements[n]
  }

}

#convert to single long string with no gaps
mdata.mat.transmit.elements.no.spaces.split<-strsplit(mdata.mat.transmit.elements.no.spaces,split="")
mdata.mat.transmit<-paste0(mdata.mat.transmit.elements.no.spaces.split,collapse="")

#rename back to mdata.mat.transmit
mdata.mat.transmit

}


# CALL THE MAIN SERVER SIDE FUNCTION
 
  calltext <- call("DANGERc2sMATDFDS", mdata.mat.transmit, from,
           nrows.transmit, ncols.transmit, colnames.transmit, colclass.transmit, byrow, dimnames)
  
  DSI::datashield.assign(datasources, newobj, calltext)


#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
#TRACER																									 	#
#return(test.obj.name)																					 	#
#}                                                                                   					 	#
																											#
																											#							
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS", test.obj.name)													 	#
																											#
object.info<-DSI::datashield.aggregate(datasources, calltext)												 	#
																											#
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
num.datasources<-length(object.info)																	 	#
																											#
																											#
obj.name.exists.in.all.sources<-TRUE																	 	#
obj.non.null.in.all.sources<-TRUE																		 	#
																											#
for(j in 1:num.datasources){																			 	#
	if(!object.info[[j]]$test.obj.exists){																 	#
		obj.name.exists.in.all.sources<-FALSE															 	#
		}																								 	#
	if(is.null(object.info[[j]]$test.obj.class) || object.info[[j]]$test.obj.class=="ABSENT"){														 	#
		obj.non.null.in.all.sources<-FALSE																 	#
		}																								 	#
	}																									 	#
																											#
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 	#
																											#
	return.message<-																					 	#
    paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
																											#
																											#
	}else{																								 	#
																											#
    return.message.1<-																					 	#
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
																											#
	return.message.2<-																					 	#
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
																											#
	return.message.3<-																					 	#
	paste0("Please use ds.ls() to identify where missing")												 	#
																											#
																											#
	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
																											#
	}																										#
																											#
	calltext <- call("messageDS", test.obj.name)															#
    studyside.message<-DSI::datashield.aggregate(datasources, calltext)											#
																											#	
	no.errors<-TRUE																							#
	for(nd in 1:num.datasources){																			#
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
		no.errors<-FALSE																					#
		}																									#
	}																										#	
																											#
																											#
	if(no.errors){																							#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(is.object.created=return.message,validity.check=validity.check))						    #
	}																										#
																											#
if(!no.errors){																								#
	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
	            studyside.messages=studyside.message))			                                            #
	}																										#
																											#
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################

}
#ds.DANGERc2sMATDF




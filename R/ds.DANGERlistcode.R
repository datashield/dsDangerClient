#' 
#' @title list the code for a function
#' @description list the code for a function deployed on the serverside.
#' @details list the code for a function deployed on the serverside.
#' The function is specified by package name and functions name.
#' @param datasources specifies the particular 'connection object(s)' to use.
#' @param package.name a vector of character containing a package name
#' @param function.name a vector of character containing a function.name
#' @return a vector of character strings giving the code listing from studies.
#' @author Stuart Wheater, DataSHIELD Team, 2020
#' @export
ds.DANGERlistcode <- function(package.name, function.name, datasources = NULL)
{
    if (is.null(datasources))
        datasources <- DSI::datashield.connections_find()

    # Check value of package.name
    if (is.null(package.name))
        stop('Error: Parameter "package.name" is NULL', call.=FALSE);

    if (!('character' %in% class(package.name)))
        stop('Error: Parameter "package.name" is not a character vector', call.=FALSE);

    # Check value of function.name
    if (is.null(function.name))
        stop('Error: Parameter "function.name" is NULL', call.=FALSE);

    if (!('character' %in% class(function.name)))
        stop('Error: Parameter "function.name" is not a character vector', call.=FALSE);

    # call the server side function
    calltext <- call("DANGERlistcodeDS", quote(package.name), quote(function.name))

    output <- DSI::datashield.aggregate(datasources, calltext)

    return(output)
}
# ds.DANGERlistcode
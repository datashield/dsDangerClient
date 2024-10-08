% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.DANGERdmtEXTRACT.R
\name{ds.DANGERdmtEXTRACT}
\alias{ds.DANGERdmtEXTRACT}
\title{DANGER function returning data.frame, Matrix or tibble (DMT) from
processing servers to clientside}
\usage{
ds.DANGERdmtEXTRACT(
  dfName = NULL,
  dfClass = "df",
  extract.study.specific = TRUE,
  extract.all.studies.combined = TRUE,
  datasources = NULL
)
}
\arguments{
\item{dfName}{is a character string that specifies the name of the DMT 
to be copied from the serverside to the clientside}

\item{dfClass}{is an optional character string specifying that, once the 
serverside dmt has been returned to the clientside, you force
ds.DANGERdmtEXTRACT to configure it as a 'data.frame' 'matrix' or 'tibble'
with aliases 'df', 'mat' and 'tbl'. If no value is specified it treats the
incoming dmt as a data.frame.}

\item{extract.study.specific}{logical defaulted to TRUE which means that the
dmt from each processing source will be combined and returned as a single object.
If FALSE this will not be returned. If the <extract.study.specific> and the
<extract.all.studies.combined> arguments are both TRUE separate and
combined dmts will be returned.}

\item{extract.all.studies.combined}{logical defaulted to TRUE which means that the
dmts from each processing source will be returned as a separate object. If FALSE
these will not be returned}

\item{datasources}{specifies the particular 'connection object(s)' to use.}
}
\value{
the serverside dmt specified by name dfName as a
data.frame/matrix/tibble on the clientside.
}
\description{
Copies a DMT from data servers to clientside.
}
\details{
ds.DANGERdmtEXTRACT calling serverside aggregate function DANGERdmtEXTRACTDS.
DANGER FUNCTION: must not be installed on data processing servers holding
real data. ONLY TO BE USED FOR DEVELOPMENT PURPOSES
}
\author{
Paul Burton for DataSHIELD Development Team - 4th June, 2021
}

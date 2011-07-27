\name{SSSR-package}
\Rdversion{1.1}
\alias{SSSR-package}
\alias{SSSR}
\docType{package}
\title{
SSSR - Server Side Scripting with R
}
\description{
SSSR is an R package for writing server side R scripts with form data handling; cookie, session and application
variables support.
}
\details{
\tabular{ll}{
Package: \tab SSSR\cr
Type: \tab Package\cr
Version: \tab 1.0.5\cr
Date: \tab 2011-07-27\cr
License: \tab LGPL\cr
}

}
\author{
Mehmet Hakan Satman

Maintainer: Mehmet Hakan Satman <mhsatman@istanbul.edu.tr>

}
\references{
Package uses RSQLite package for storing session and application parameters.
}
\keyword{ package }

\examples{
~#!/usr/bin/Rscript
#require("SSSR")
response.setContentType()
session.start()
response.startDocument()

name<-request.getGETParameter("name")
surname<-request.getGETParameter("surname")

if (nchar(name)>0 && nchar(surname)>0){
	session.setParameter("name",name)
	session.setParameter("surname",surname)
	response.print("Session variables were set to ",name," and ", surname)
}
}

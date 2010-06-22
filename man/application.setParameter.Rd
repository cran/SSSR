\name{application.setParameter}
\Rdversion{1.1}
\alias{application.setParameter}

\title{
application.setParameter
}
\description{
Sets value of a given application parameter
}
\usage{
application.setParameter(paramName, paramValue)
}

\arguments{
  \item{paramName}{
Name of the application variable to set
}
  \item{paramValue}{
Value of the application variable
}
}
\details{
Application variables are visible and accessible for all clients. Changing value of a given application
parameter reflects all of the application that uses this parameter.
}

\author{
Mehmet Hakan Satman
}




\seealso{
\link{application.getParameter}
\link{application.getAllParameters}
}

\examples{
	#!/usr/bin/Rscript
	require("SSSR");
	response.setContentType();
	response.startDocument();
	
	m<-as.integer(application.getParameter("pageCounter"))
	if (is.na(m)) m<-0
	m<-m+1
	application.setParameter("pageCounter",m)
	response.println("This page was visited ",m," times")
}


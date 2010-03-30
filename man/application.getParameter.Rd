\name{application.getParameter}
\Rdversion{1.1}
\alias{application.getParameter}

\title{
application.getParameter
}
\description{
Returns values of a given application parameter.
}
\usage{
application.getParameter(paramName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paramName}{
	Name for application variable.
}
}

\author{
Mehmet Hakan Satman
}

\seealso{
\link{application.setParameter}
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


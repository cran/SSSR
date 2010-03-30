\name{session.start}
\Rdversion{1.1}
\alias{session.start}

\title{
session.start
}
\description{
Initializes database, catches or generates cookies for session operations.
}
\usage{
session.start()
}

\details{
Must be called before response.startDocument
}

\author{
Mehmet Hakan Satman
}




\seealso{
\link{response.startDocument}
\link{session.getParameter}
\link{session.setParameter}
\link{session.getAllParameters}
\link{session.delete}
}


\examples{
#!/usr/bin/Rscript
require("SSSR")
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


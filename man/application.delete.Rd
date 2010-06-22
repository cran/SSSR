\name{application.delete}
\Rdversion{1.1}
\alias{application.delete}

\title{
application.delete
}
\description{
Deletes all application variables.
}
\usage{
application.delete()
}
%- maybe also 'usage' for other objects documented here.

\author{
Mehmet Hakan Satman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{session.delete}
\link{application.setParameter}
\link{application.getParameter}
\link{application.getAllParameters}
}

\examples{
#!/usr/bin/Rscript
response.setContentType()
session.start()
session.delete()
}


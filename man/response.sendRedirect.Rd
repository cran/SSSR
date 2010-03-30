\name{response.sendRedirect}
\Rdversion{1.1}
\alias{response.sendRedirect}

\title{
response.sendRedirect
}
\description{
Redirects to another page
}
\usage{
response.sendRedirect(aUrl)
}

\arguments{
  \item{aUrl}{
An url to browse
}
}
\details{
response.sendRedirect function sends a redirect directive to browser using the HTTP headers.
So, it must be used before response.startDocument function.
}

\author{
Mehmet Hakan Satman
}




\seealso{
\link{response.setCookie}
}

\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
session.start()
x<-session.getParameter("site");
if (nchar(x)<1) response.sendRedirect("http://www.google.com")

response.startDocument()

}


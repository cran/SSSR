\name{response.setContentType}
\Rdversion{1.1}
\alias{response.setContentType}

\title{
response.setContentType
}
\description{
Defines content type of web document.
}
\usage{
response.setContentType(contentType = "text/html")
}

\arguments{
  \item{contentType}{
Default value is "text/html". If produced content is not text but an image, value can be set to "image/png"
}
}

\author{
Mehmet Hakan Satman
}




\seealso{
\link{response.sendRedirect}
}
\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType("text/csv")
response.startDocument()

response.println("1;2;3;")
response.println("3;5;1;")
}


\name{response.print}
\Rdversion{1.1}
\alias{response.print}

\title{
response.print
}
\description{
Sends values of given parameters and strings that seperated with commas.
}
\usage{
response.print(...)
}

\arguments{
  \item{\dots}{
Comma seperated values, variables, etc.
}
}

\author{
Mehmet Hakan Satman
}



\seealso{
\link{response.println}
}
\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
response.startDocument()

x<-rnorm(10)
s<-"hi this is server side R"
response.print(x,"<br>",s,"<br>",quantile(x,0.95))
}


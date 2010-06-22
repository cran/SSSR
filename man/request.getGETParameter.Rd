\name{request.getGETParameter}
\Rdversion{1.1}
\alias{request.getGETParameter}

\title{
request.getGETParameter
}
\description{
Returns value of a given form variable that sent with GET method.
Beside using forms, this function can be used to handle variables defined in URL.
For instance, variables var1 and var2 in http://host/cgi-bin/file.r?var1=val1&var2=val2
can be catched using request.getGETParameter("var1") and request.getGETParameter("var2")
}
\usage{
request.getGETParameter(paramName)
}

\arguments{
  \item{paramName}{
Parameter name defined in HTML form element or URL.
}
}

\author{
Mehmet Hakan Satman
}




\examples{
#!/usr/bin/Rscript
require("SSSR")
response.setContentType()
response.startDocument()

#Suppose that URL is http://host/file.r?var1=hello
response.println(request.getGETParameter("var1"))

}


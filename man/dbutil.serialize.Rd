\name{dbutil.serialize}
\Rdversion{1.1}
\alias{dbutil.serialize}

\title{
dbutil.serialize
}
\description{
Serializes an R object to store it in SQLite database.
}
\usage{
dbutil.serialize(obj)
}

\arguments{
  \item{obj}{
Object to serialize
}
}

\author{
Mehmet Hakan Satman
}



\seealso{
\link{dbutil.unSerialize}
}

\examples{
x<-rnorm(3)
z<-dbutil.serialize(x)
cat(z)
}

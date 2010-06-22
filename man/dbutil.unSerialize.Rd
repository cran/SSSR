\name{dbutil.unSerialize}
\Rdversion{1.1}
\alias{dbutil.unSerialize}

\title{
dbutil.unSerialize
}
\description{
Decodes an R object that serialized using dbutil.serialize function.
}
\usage{
dbutil.unSerialize(serializedObject)
}

\arguments{
  \item{serializedObject}{
An R object that serialized using dbutil.serialize
}
}

\author{
Mehmet Hakan Satman
}




\seealso{
\link{dbutil.serialize}
}

\examples{
x<-rnorm(3)
cat (x)


z<-dbutil.serialize(x)
cat(dbutil.unSerialize (z))

}

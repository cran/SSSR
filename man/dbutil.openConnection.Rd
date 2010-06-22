\name{dbutil.openConnection}
\Rdversion{1.1}
\alias{dbutil.openConnection}

\title{
dbutil.openConnection
}
\description{
Function for loading driver and opening a connection to SQLite database for SSSR's session and application variables functionality.
Sets values of the public variables db_Connection and db_Driver.
}
\usage{
dbutil.openConnection()
}

\details{
Function initializes the SQLite driver for 128 concurrent connections. Default path for database is /tmp/rsessions.
This folder is accessible with read and write rights for all users in Unix systems.
}

\author{
Mehmet Hakan Satman
}
\note{
RSQlite package must be pre-installed.
}



\examples{
	dbutil.openConnection()
}


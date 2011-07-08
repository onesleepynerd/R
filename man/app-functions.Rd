\name{App functions}
\alias{create.app}
\alias{load.app}
\alias{run.app}
\alias{stop.app}
\alias{get.result}
\title{Interact with Opani apps}
\description{These functions create, download and run Opani apps from your R interpreter.}
\usage{
  create.app(app = NULL, question, private = NULL)
  
  load.app(app = NULL)
  
  run.app(keys = NULL, app = NULL, cluster = NULL)
  stop.app(jobid = NULL, app = NULL)

  get.result(jobid = NULL, app = NULL, format = "text")
}
\arguments{
  \item{app}{
    The name of the app. This can be a simple name like \code{"stock-history"} or can include the name
    of an opani user, i.e. \code{"ryan/stock-explorer"}. If you leave it out, the package will try
    to use the name of your current directory (provided you have a \code{"run.r"} script in the directory).
  }
  \item{question}{
  The question this app is trying to answer.
  }
  \item{private}{
  TRUE if you want the app to be private, otherwise, the app will be public.
  }
  \item{keys}{
  A vector or list of keys for the app to paralellize over.
  }
  \item{cluster}{
  The name of the cluster you want this app to run on i.e. 'local', 'Free_Cluster', 'Amazon', or 'Rackspace'.
  Defaults to the last cluster the app ran on.
  }
  \item{jobid}{
  The number of the job operate on. Deafults to the latest job. This number is returned by \code{run.app}.
  }
  \item{format}{
  The format for job results. \code{"text"} is the default, but you can also get the results in
  \code{"json"} or \code{"html"} format.
  }
}
\value{
For \code{run.app}, the return value is the \code{jobid} of running job. This can be used to call
\code{get.result} for a particular job.
}
\seealso{
  \link{load.file}, \link{save.file}, \link{load.files}, \link{save.files},
  \link{load.s3}, \link{save.s3}, \link{load.cloudfile}, and \link{save.cloudfile}.
}
\examples{
# Create a working directory with the same name as our app
dir.create('stock-history')
setwd('stock-history')

# Create a run.r file that examines the history of a single stock
writeLines(c(
 "url <- paste('http://ichart.finance.yahoo.com/table.csv?s=', key, sep='')",
 "stock <- read.csv(url, colClasses=c('Date',NA,NA,NA,NA,NA,NA))",
 "print(summary(stock))",
 "plot(Close~Date, stock, type='l', main='Closing price over time')",
 "show()"), 'run.r'
)

# Create the app
create.app(question='What does the history of each stock look like?')

# Test the app out locally
run.app.local(keys = c('AAPL'))

# Run the app in parallel on the cloud
run.app(keys = c('AAPL','GOOG','MSFT','AMZN'))

# Look at the reusult (use opani.com to view the results in real-time)
get.result()

}

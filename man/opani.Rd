\name{opani}
\alias{opani}
\docType{package}
\title{Opani Toolbox for R}
\description{Download, update and run Opani cloud apps.}
\usage{}
\details{
Opani apps help you gain insight quickly by scaling your R code to hundereds of
computers in the cloud or in your office. This package lets you interact quickly
with Opani, testing your code locally while debugging, then scaling it to many
computers when appropriate.

For documentation on topics not covered here, see https://opani.com/help/
}
\author{Opani LLC <info@opani.com>}
\references{
}
\keyword{ package }
\seealso{
  \code{\link{create.app}, \link{load.app}, \link{run.app}, \link{stop.app}, \link{get.result},
  \link{load.file},\link{save.file},\link{load.files},\link{save.files},
  \link{load.s3}, \link{save.s3}, \link{load.cloudfile}, and \link{save.cloudfile}.
  }
}
\examples{
# Load an existing Opani app locally and test a single key
load.app('ryan/aapl-goog-correlation')
key <- 'AAPL,GOOG,Volume'; source('run.r')

# Create a simple app
dir.create('stock-history'); setwd('stock-history')
create.app('stock-history', question='What does the history of each stock look like?')
writeLines(c(
 "url <- paste('http://ichart.finance.yahoo.com/table.csv?s=', key, sep='')",
 "stock <- read.csv(url, colClasses=c('Date',NA,NA,NA,NA,NA,NA))",
 "print(summary(stock))",
 "plot(Close~Date, stock, type='l', main='Closing price over time')",
 "show.image()"), 'run.r'
)

# Test the app out locally
key <- 'AAPL'; source('run.r')

# Run the app in parallel on the cloud
run.app(c('AAPL','GOOG','MSFT','AMZN'))

# Save local files to Amazon S3 or Rackspace CloudFiles
# Uses your own S3 bucket or CloudFiles container (https://opani.com/account/)
save.s3('data.csv')
save.cloudfile('data.csv')

# Load previously saved files to the local directory
load.s3('data.csv')
load.cloudfile('data.csv')
data <- read.csv('data.csv') # Use the file normally once it's downloaded

# Save and load source files for the current app
load.file('filename.r')  # download a file from Opani to the local directory
load.files()             # download all files form this app
save.file('filename.r')  # upload a local file to the Opani app
save.files()             # uplod all local files (including new .R files)
}


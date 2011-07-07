\name{cloud-storage}
\alias{load.s3}
\alias{save.s3}
\alias{load.cloudfile}
\alias{save.cloudfile}
\title{Work with files stored in the cloud}
\description{
These functions upload and download files from the Amazon S3 and Rackspace CloudFiles 
storage services. They also have built-in data locality when running apps across
multiple computers.
}
\usage{
  load.s3(filename)
  save.s3(filename)
  load.cloudfile(filename)
  save.cloudfile(filename)
}
\arguments{
  \item{filename}{}
}
\details{
The \code{load.s3} and \code{load.cloudfile} commands look for the \code{filename}
relative to the S3 bucket or CloudFiles container you've specified on your
https://opani.com/account/ page. If the file exists, they download it to the local
directory to a file of the same name.

Simillarly, the \code{save.s3} and \code{save.cloudfile} take a local file and
upload them to your bucket/container.

If the file exists in the local cache, the \code{load} variants will use that file
instead of downloading one from the network. Since Opani runs keys on the same
machines across multiple runs of an app, this caching step results in a nice
speedup.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{create.app}, \link{load.app}, \link{run.app}, \link{get.result},
  \link{load.s3}, \link{save.s3}, \link{load.cloudfile}, and \link{save.cloudfile}.
  }
}
\examples{
# Write a local data set to a file
write.csv(cars, 'cars.csv')

# Upload the local file to Amazon S3
save.s3('cars.csv')

# Download the file (could do this from another machine, too)
load.s3('cars.csv')

# Read in the data
cars <- read.csv('cars.csv')
}
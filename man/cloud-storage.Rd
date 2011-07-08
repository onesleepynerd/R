\name{Cloud storage}
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
  \item{filename}{
    Name of the local file for \code{save} commands or name of the remote file
    for \code{load} commands.
  }
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
\seealso{
  \link{create.app}, \link{load.app}, \link{run.app}, \link{stop.app}, \link{get.result},
  \link{load.file}, \link{save.file}, \link{load.files}, and \link{save.files}.
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

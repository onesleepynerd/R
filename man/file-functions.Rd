\name{File functions}
\alias{load.file}
\alias{save.file}
\alias{load.files}
\alias{save.files}
\title{Synchronize files with Opani apps}
\description{
These functions help you synchronize your source files between your local computer and Opani apps.
}
\usage{
  load.file(filename = NULL, app = NULL)
  save.file(filename = NULL, app = NULL)
  load.files(app = NULL)
  save.files(app = NULL)
}
\arguments{
  \item{filename}{
  The name of the source file to be saved or retrieved (i.e. \code{"run.r"}, \code{"README"})
}
  \item{app}{
    The name of the app. This can be a simple name like \code{"stock-history"} or can include the name
    of an opani user, i.e. \code{"ryan/stock-explorer"}. If you leave it out, the package will try
    to use the name of your current directory (provided you have a \code{"run.r"} script in the directory).
  }
}
\details{
  The \code{load} functions pull the latest files from an Opani app to your computer, overwriting the
  corresponding file in your local directory. The \code{save} functions take a local file and store it
  in the Opani app, automatically keeping track of each change.
  
  For more advanced synchronization of local code to Opani apps using git, see 
  http://opani.com/help/git-notes
}
\seealso{
 \link{create.app}, \link{load.app}, \link{run.app}, \link{get.result},
 \link{load.s3}, \link{save.s3}, \link{load.cloudfile}, and \link{save.cloudfile}.
}
\examples{
# Make some local changes, then:
save.file('run.r')
run.app()

# Debug the app on the web interface, then update your local copy with:
load.files()
}

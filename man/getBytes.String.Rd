%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getBytes.String}
\alias{getBytes.String}
\alias{String.getBytes}
\alias{getBytes.String}
\alias{getBytes,String-method}


\title{Converts the string into a vector of bytes}

\usage{\method{getBytes}{String}(this, ...)}

\description{
 Converts the string into a vector of bytes (integers) according to the
 ASCII table.
}

\value{
  Returns a vector of integers in the range [0,255]. If the string is empty
  an empty vector is returned.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  bfr <- getBytes(s)  # c(72,101,108,108,111,32,119,111,114,108,100,33)
  bfr <- getBytes("Hello world!")
}

\seealso{
  \code{\link[R.lang:toCharArray.String]{*toCharArray}()}, \code{\link[R.oo]{charToInt}},
  \code{\link[R.oo]{intToChar}}
}
\keyword{internal}
\keyword{methods}

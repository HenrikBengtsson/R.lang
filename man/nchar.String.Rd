%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{nchar.String}
\alias{nchar.String}
\alias{String.nchar}
\alias{nchar.String}
\alias{nchar,String-method}

\alias{String.length}

\title{Gets the number of characters in the string}

\usage{\method{nchar}{String}(this, ...)}

\description{
 Gets the number of \code{\link[base]{character}}s in the string.
}

\value{
  Returns the number of \code{\link[base]{character}}s. If the string is empty, 0 is returned.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  nchar(s)               # 12
  nchar("Hello world!")  # 12
}
\keyword{internal}
\keyword{methods}

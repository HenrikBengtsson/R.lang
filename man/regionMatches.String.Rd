%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{regionMatches.String}
\alias{regionMatches.String}
\alias{String.regionMatches}
\alias{regionMatches.String}
\alias{regionMatches,String-method}


\title{Tests if two string regions are equal}

\usage{\method{regionMatches}{String}(this, toffset, other, ooffset, len, ignoreCase=FALSE, ...)}

\arguments{
 \item{toffset}{The starting offset of the subregion in this string.}
 \item{other}{The other string.}
 \item{ooffset}{The starting offset of the subregion in the other string.}
 \item{len}{The number of \code{\link[base]{character}}s to compare.}
 \item{ignoreCase}{If \code{\link[base:logical]{TRUE}} case is ignored.}
}

\description{
 Tests if two string regions are equal.
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if the specified subregion of this string matches
  the specified subregion of the other string, \code{\link[base:logical]{FALSE}} otherwise.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s1 <- String("Hello world!")
  s2 <- String("Oh, hello there!")
  regionMatches(s1, 1, s2, 5, 5)        # TRUE
  regionMatches(s2, 5, s1, 1, 5)        # TRUE
  regionMatches(s1, 0, s2, 4, 5)        # FALSE
  regionMatches(s1, 0, s2, 4, 5, TRUE)  # TRUE

  s1 <- "Hello world!"
  s2 <- "Oh, hello there!"
  regionMatches(s1, 1, s2, 5, 5)        # TRUE
  regionMatches(s2, 5, s1, 1, 5)        # TRUE
  regionMatches(s1, 0, s2, 4, 5)        # FALSE
  regionMatches(s1, 0, s2, 4, 5, TRUE)  # TRUE
}

\seealso{
 \code{\link[R.lang:compareTo.String]{*compareTo}()},
 \code{\link[R.lang:equals.String]{*equals}()}, \code{\link[R.lang:equalsIgnoreCase.String]{*equalsIgnoreCase}()}
}
\keyword{internal}
\keyword{methods}

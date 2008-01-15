%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{indexOf.String}
\alias{indexOf.String}
\alias{String.indexOf}
\alias{indexOf.String}
\alias{indexOf,String-method}


\title{Gets the first position of a character or a substring}

\usage{\method{indexOf}{String}(this, s, fromIndex=0, ...)}

\arguments{
 \item{fromIndex}{The index to start the search from.}
}

\description{
 Gets the first position of the character or the substring \code{s}
 starting from \code{fromIndex}.
}

\value{
  Returns the position. If no such \code{\link[base]{character}} exists, \code{-1} is returned.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  indexOf(s, "l")               #  2
  indexOf(s, "l", fromIndex=5)  #  9
  indexOf(s, "world")           #  6
  indexOf(s, "a")               # -1
  indexOf("Hello world!", "l")  #  2
}

\seealso{\code{\link[R.lang:lastIndexOf.String]{*lastIndexOf}()}, \code{\link[R.lang:charAt.String]{*charAt}()}}
\keyword{internal}
\keyword{methods}

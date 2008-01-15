%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{lastIndexOf.String}
\alias{lastIndexOf.String}
\alias{String.lastIndexOf}
\alias{lastIndexOf.String}
\alias{lastIndexOf,String-method}


\title{Gets the last position of a character or a substring}

\usage{\method{lastIndexOf}{String}(this, s, fromIndex=0, ...)}

\arguments{
 \item{fromIndex}{The index to start the search from.}
}

\description{
 Gets the last position of the character or the substring \code{s}
 starting from \code{fromIndex}.
}

\value{
  Returns the position. If no such \code{\link[base]{character}} exists, \code{-1} is returned.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  lastIndexOf(s, " ")               # 5
  lastIndexOf(s, "l")               # 9
  lastIndexOf(s, "l", fromIndex=5)  # 3
  lastIndexOf("Hello world!", " ")  # 5
}

\seealso{\code{\link[R.lang:indexOf.String]{*indexOf}()}, \code{\link[R.lang:charAt.String]{*charAt}()}}
\keyword{internal}
\keyword{methods}

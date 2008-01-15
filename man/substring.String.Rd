%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{substring.String}
\alias{substring.String}
\alias{String.substring}
\alias{substring.String}
\alias{substring,String-method}


\title{Gets a substring of this string}

\usage{\method{substring}{String}(this, beginIndex, endIndex=NULL, ...)}

\arguments{
 \item{beginIndex}{The beginning index, inclusive.}
 \item{endIndex}{The ending index, exclusive.}
}

\description{
 Gets a substring of this string. The substring begins at the position
 \code{beginIndex} and extends to the \code{\link[base]{character}} at position
 \code{endIndex-1}.
}

\value{
  Returns the substring.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  substring(s, 6,11)                    # "world"
  substring(s, 6)                       # "world!"
  substring(String("Hello world!"), 6)  # "world!"
}
\keyword{internal}
\keyword{methods}

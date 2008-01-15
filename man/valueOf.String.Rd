%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{valueOf.String}
\alias{valueOf.String}
\alias{String.valueOf}
\alias{valueOf.String}
\alias{valueOf,String-method}


\title{Returns the string representation of the specified object}

\synopsis{valueOf.String(this, x, ...)}
\usage{String$valueOf(x, ...)}

\arguments{
 \item{x}{The object to be represented as a string.}
}

\description{
 Returns the string representation of the specified object.
}

\value{
  Returns the string representation of the specified object.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  String$valueOf(TRUE)    # "TRUE"
  String$valueOf(42)      # "42"
  String$valueOf(42.01)   # "42.01"
  String$valueOf("same")  # "same"
  String$valueOf(NA)      # "NA"
  String$valueOf(NaN)     # "NaN"
  String$valueOf(Inf)     # "Inf"
  String$valueOf(0/0)     # "0/0"
}
\keyword{internal}
\keyword{methods}

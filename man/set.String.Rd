%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{set.String}
\alias{set.String}
\alias{String.set}
\alias{set.String}
\alias{set,String-method}


\title{Sets a new value of this string}

\usage{\method{set}{String}(this, str, ...)}

\arguments{
 \item{value}{The new string to used.}
}

\description{
 Sets a new value of this string.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s1 <- String("Hello world!")
  s1$set("Hello there!")
}
\keyword{internal}
\keyword{methods}

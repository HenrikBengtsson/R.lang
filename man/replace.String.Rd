%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{replace.String}
\alias{replace.String}
\alias{String.replace}
\alias{replace.String}
\alias{replace,String-method}


\title{Replaces all occurrences of oldChar in this string with newChar}

\usage{\method{replace}{String}(this, oldChar, newChar, ...)}

\arguments{
 \item{oldChar}{The old \code{\link[base]{character}}.}
 \item{newChar}{The new \code{\link[base]{character}}.}
}

\description{
 Replaces all occurrences of \code{oldChar} in this string with
 \code{newChar}.
}

\value{
  Returns the new string.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  replace(s, "!", ".")                        # "Hello world."
  replace(String("Hello world!"), "!", ".")   # "Hello world."
}
\keyword{internal}
\keyword{methods}

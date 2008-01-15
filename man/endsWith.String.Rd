%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{endsWith.String}
\alias{endsWith.String}
\alias{String.endsWith}
\alias{endsWith.String}
\alias{endsWith,String-method}


\title{Tests if the string ends with specified suffix}

\usage{\method{endsWith}{String}(this, suffix, ...)}

\arguments{
 \item{suffix}{The suffix.}
}

\description{
 Tests if the string ends with a suffix or not.
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if the string ends with the given suffix, otherwise
  \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  endsWith(s, "world")              # FALSE
  endsWith(s, "ld!")                # TRUE
  endsWith("Hello world!", "ld!")   # TRUE
}
\keyword{internal}
\keyword{methods}

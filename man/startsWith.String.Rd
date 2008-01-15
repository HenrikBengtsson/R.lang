%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{startsWith.String}
\alias{startsWith.String}
\alias{String.startsWith}
\alias{startsWith.String}
\alias{startsWith,String-method}


\title{Tests if the string starts with specified prefix}

\usage{\method{startsWith}{String}(this, prefix, ...)}

\arguments{
 \item{prefix}{The prefix.}
}

\description{
 Tests if the string starts with a prefix or not.
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if the string starts with the given prefix, otherwise
  \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  startsWith(s, "Hello")               # TRUE
  startsWith(s, "hello")               # FALSE
  startsWith("Hello world!", "Hello")  # TRUE
}
\keyword{internal}
\keyword{methods}

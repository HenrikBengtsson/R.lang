%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{concat.String}
\alias{concat.String}
\alias{String.concat}
\alias{concat.String}
\alias{concat,String-method}


\title{Concatenates two strings}

\usage{\method{concat}{String}(this, s, ...)}

\arguments{
 \item{s}{The string to be concatenated to this string.}
}

\description{
 Concatenates two strings into a third string.
}

\value{
  Returns the concatenated string.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s1 <- String("Hello")
  s2 <- String("world!")
  s3 <- concat(s1, " ")         # "Hello "
  s4 <- concat(s3, s2)          # "Hello world!"
  concat("Hello", " world!")    # "Hello world!"
}
\keyword{internal}
\keyword{methods}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{equals.String}
\alias{equals.String}
\alias{String.equals}
\alias{equals.String}
\alias{equals,String-method}


\title{Compares this string to the specified object}

\usage{\method{equals}{String}(this, obj, ...)}

\arguments{
 \item{obj}{The object to compared to.}
}

\description{
 Compares this string to the specified object. The result is \code{\link[base:logical]{TRUE}}
 if and only if the argument is not null and is a \code{String} object or
 a character string that represents the same sequence of characters as
 this object.
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if the the strings are equal, otherwise \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s1 <- String("Hello world!")
  s2 <- String("hello world!")
  s3 <- String("Hello world!")

  equals(s1, s1)         # TRUE
  equals(s1, s2)         # FALSE
  equals(s1, s3)         # TRUE
  equals("abc", "ABC")   # FALSE
}

\seealso{
 \code{\link[R.lang:equalsIgnoreCase.String]{*equalsIgnoreCase}()}, \code{\link[R.lang:compareTo.String]{*compareTo}()},
 \code{\link[R.lang:regionMatches.String]{*regionMatches}()}
}
\keyword{internal}
\keyword{methods}

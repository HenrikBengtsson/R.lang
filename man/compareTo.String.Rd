%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{compareTo.String}
\alias{compareTo.String}
\alias{String.compareTo}
\alias{compareTo.String}
\alias{compareTo,String-method}


\title{Compares this string to another Object}

\usage{\method{compareTo}{String}(this, obj, ...)}

\arguments{
 \item{index}{the index of the \code{\link[base]{character}}.}
}

\description{
 Compares this string to another Object (or \code{\link[base]{character}} string) lexically.
}

\value{
  Returns \code{-1} if this String is lexicographically less than the other
  string, \code{+1} if this String is lexicographically greater than the other
  string, and \code{0} if they are lexicographically equal.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s1 <- String("abc")
  s2 <- String("ABC")
  s3 <- String("def")
  s4 <- String(" def")

  compareTo(s1, s1)         #  0
  compareTo(s1, s2)         # +1
  compareTo(s2, s1)         # -1
  compareTo(s1, s3)         # -1
  compareTo(s3, s4)         # +1
  compareTo("def", " def")  # +1
}

\seealso{
 \code{\link[R.lang:equals.String]{*equals}()}, \code{\link[R.lang:equalsIgnoreCase.String]{*equalsIgnoreCase}()},
 \code{\link[R.lang:compareTo.String]{*compareTo}()}, \code{\link[R.lang:regionMatches.String]{*regionMatches}()}
}
\keyword{internal}
\keyword{methods}

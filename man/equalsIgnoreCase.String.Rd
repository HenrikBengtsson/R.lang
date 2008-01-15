%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{equalsIgnoreCase.String}
\alias{equalsIgnoreCase.String}
\alias{String.equalsIgnoreCase}
\alias{equalsIgnoreCase.String}
\alias{equalsIgnoreCase,String-method}


 \title{Compares this String to another String, ignoring case considerations}

 \usage{\method{equalsIgnoreCase}{String}(this, obj, ...)}

 \arguments{
  \item{obj}{The object to compared to.}
 }

 \description{
  Compares this String to another object, ignoring case considerations.
  The result is \code{\link[base:logical]{TRUE}}
  if and only if the argument is not null and is a \code{String} object or
  a character string that represents the same sequence of characters as
  this object ignoring the case.
 }

 \value{
   Returns \code{\link[base:logical]{TRUE}} if the the strings are equal, otherwise \code{\link[base:logical]{FALSE}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \examples{
   s1 <- String("Hello world!")
   s2 <- String("hello world!")
   s3 <- String("Hello world!")

   equals(s1, s2)                   # FALSE
   equalsIgnoreCase(s1, s2)         # TRUE
   equalsIgnoreCase("abc", "ABC")   # TRUE
 }

 \seealso{
  \code{\link[R.lang:equals.String]{*equals}()}, \code{\link[R.lang:compareTo.String]{*compareTo}()},
  \code{\link[R.lang:regionMatches.String]{*regionMatches}()}
 }
\keyword{internal}
\keyword{methods}

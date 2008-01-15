%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{trim.String}
\alias{trim.String}
\alias{String.trim}
\alias{trim.String}
\alias{trim,String-method}


\title{Removes white space from both ends of this string}

\usage{\method{trim}{String}(this, ...)}

\description{
 Removes white space from both ends of this string. The following character
 values are all white spaces:
 \code{"\\t"} (0x09 - HORIZONTAL TABULATION),
 \code{"\\n"} (0x0A - NEW LINE),
 \code{"\\v"} (0x0B - VERTICAL SPACE),
 \code{"\\f"} (0x0C - FORM FEED),
 \code{"\\r"} (0x0D  - CARRIAGE RETURN), and
 \code{" "}   (0x20 - SPACE).
}

\value{
  Returns this string, with white space removed from the front and end.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("\t  Hello world!\n")
  print(trim(s))                           # "Hello world!"
  print(trim("\t  Hello world!\n"))        # "Hello world!"
}
\keyword{internal}
\keyword{methods}

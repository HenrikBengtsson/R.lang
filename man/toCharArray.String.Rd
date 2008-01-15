%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{toCharArray.String}
\alias{toCharArray.String}
\alias{String.toCharArray}
\alias{toCharArray.String}
\alias{toCharArray,String-method}


\title{Converts this string to a vector of characters}

\usage{\method{toCharArray}{String}(this, ...)}

\description{
 Converts this string to a vector of \code{\link[base]{character}}s.
}

\value{
  Returns the vector of \code{\link[base]{character}}s.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  toCharArray(s)  # c("H","e","l","l","o"," ","w","o","r","l","d","!")
  toCharArray("Hello world!")
}

\seealso{\code{\link[R.lang:getBytes.String]{*getBytes}()}}
\keyword{internal}
\keyword{methods}

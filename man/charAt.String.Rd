%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{charAt.String}
\alias{charAt.String}
\alias{String.charAt}
\alias{charAt.String}
\alias{charAt,String-method}


\title{Gets the character at given position}

\usage{\method{charAt}{String}(this, index, ...)}

\arguments{
 \item{index}{the index of the \code{\link[base]{character}}.}
}

\description{
 Gets the \code{\link[base]{character}} at given position. First position has index 0.
}

\value{
  Returns one \code{\link[base]{character}} (as a \code{\link[base]{character}} string with length one).
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")

  charAt(s, 0)               # "H"
  charAt(s, length(s)-1)     # "!"
  # charAt(s, length(s))     # ERROR: Index of bounds.
  charAt("Hello world!", 4)  # "o"
}

\seealso{\code{\link[R.lang:indexOf.String]{*indexOf}()}, \code{\link[R.lang:lastIndexOf.String]{*lastIndexOf}()}}
\keyword{classes}
\keyword{internal}
\keyword{methods}

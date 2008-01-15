%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{toUpperCase.String}
\alias{toUpperCase.String}
\alias{String.toUpperCase}
\alias{toUpperCase.String}
\alias{toUpperCase,String-method}


\title{Converts all of the characters in this String to upper case}

\usage{\method{toUpperCase}{String}(this, ...)}

\description{
 Converts all of the characters in this String to upper case.
}

\value{
  Returns the string, converted to upper case.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  toUpperCase(s)                # "HELLO WORLD!"
  toUpperCase("Hello world!")   # "HELLO WORLD!"
}

\seealso{\code{\link[R.lang:toLowerCase.String]{*toLowerCase}()}}
\keyword{internal}
\keyword{methods}

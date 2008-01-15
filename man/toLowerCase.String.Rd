%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{toLowerCase.String}
\alias{toLowerCase.String}
\alias{String.toLowerCase}
\alias{toLowerCase.String}
\alias{toLowerCase,String-method}


\title{Converts all of the characters in this String to lower case}

\usage{\method{toLowerCase}{String}(this, ...)}

\description{
 Converts all of the characters in this String to lower case.
}

\value{
  Returns the string, converted to lower case.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
  s <- String("Hello world!")
  toLowerCase(s)                     # "hello world!"
  toLowerCase("Hello world!")        # "hello world!"
}

\seealso{\code{\link[R.lang:toUpperCase.String]{*toUpperCase}()}}
\keyword{internal}
\keyword{methods}

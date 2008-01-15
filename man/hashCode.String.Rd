%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  String.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hashCode.String}
\alias{hashCode.String}
\alias{String.hashCode}
\alias{hashCode.String}
\alias{hashCode,String-method}


 \title{Returns a hash code for this string}

 \usage{\method{hashCode}{String}(this, ...)}

 \description{
  Returns a hash code for this string. The hashcode for a \code{String} object
  is computed as

   \code{s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]}

  using int arithmetic, where \code{s[i]} is the \emph{i}th character of the
  string, \code{n} is the length of the string, and \code{^} indicates
  exponentiation. (The hash value of the empty string is zero.)
 }

 \value{
   Returns the hash code value of this String.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \examples{
   s1 <- String("Hello world!")
   s2 <- String("Hello world!")
   hashCode(s1)              # -52967168
   hashCode(s2)              # -52967168
   hashCode("Hello world!")  # -52967168
 }
\keyword{internal}
\keyword{methods}

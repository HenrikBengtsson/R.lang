###########################################################################/**
# @RdocClass String
#
# \title{The String class}
#
# \description{
#  @classhierarchy
#
#   The methods in the String class have all, except \code{length}, 
#   \code{replace} and \code{substring} (they are already defined elsewhere
#   in [R]), corresponding and equally named functions that operates on
#   regular [R] character strings.
# }
#
# @synopsis
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @examples "../incl/StringExample.Rex"
#
# @author
#*/###########################################################################
setConstructorS3("String", function(str="") {
  if (length(str) > 1) {
    if (is.numeric(str))
      str <- intToChar(str);
    str <- paste(str, collapse="");
  }
  extend(Object(), "String",
    str = as.character(str)
  );
}, deprecated=TRUE)



###########################################################################/**
# @RdocMethod charAt
#
# \title{Gets the character at given position}
#
# @synopsis
#
# \arguments{
#  \item{index}{the index of the @character.}
# }
#
# \description{
#  Gets the @character at given position. First position has index 0.
# }
#
# \value{
#   Returns one @character (as a @character string with length one).
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#
#   charAt(s, 0)               # "H"
#   charAt(s, length(s)-1)     # "!"
#   # charAt(s, length(s))     # ERROR: Index of bounds.
#   charAt("Hello world!", 4)  # "o"
# }
#
# \seealso{@seemethod "indexOf", @seemethod "lastIndexOf"}
#*/###########################################################################
setMethodS3("charAt", "String", function(this, index, ...) {
  str <- as.character(this);
  len <- nchar(str);
  if (index < 0 || index > len-1)
    stop("Index out of bounds.");
  s <- substr(str, index+1, index+1);
  String(s);
})


# This will make the function available for regular @character strings too.
setMethodS3("charAt", "default", function(obj, ...) {
  charAt(String(obj), ...);
})


###########################################################################/**
# @RdocMethod compareTo
#
# \title{Compares this string to another Object}
#
# @synopsis
#
# \arguments{
#  \item{index}{the index of the @character.}
# }
#
# \description{
#  Compares this string to another Object (or @character string) lexically. 
# }
#
# \value{
#   Returns \code{-1} if this String is lexicographically less than the other
#   string, \code{+1} if this String is lexicographically greater than the other
#   string, and \code{0} if they are lexicographically equal.
# }
#
# @author
#
# \examples{
#   s1 <- String("abc")
#   s2 <- String("ABC")
#   s3 <- String("def")
#   s4 <- String(" def")
#
#   compareTo(s1, s1)         #  0
#   compareTo(s1, s2)         # +1
#   compareTo(s2, s1)         # -1
#   compareTo(s1, s3)         # -1
#   compareTo(s3, s4)         # +1
#   compareTo("def", " def")  # +1
# }
#
# \seealso{
#  @seemethod "equals", @seemethod "equalsIgnoreCase",
#  @seemethod "compareTo", @seemethod "regionMatches"
# }
#*/###########################################################################
setMethodS3("compareTo", "String", function(this, obj, ...) {
  str <- as.character(this);
  s <- as.character(obj);
  if (str < s)
    -1
  else if (str > s)
    +1
  else
    0;
})

# This will make the function available for regular character strings too.
setMethodS3("compareTo", "default", function(obj, ...) {
  compareTo(String(obj), ...);
})


###########################################################################/**
# @RdocMethod concat
#
# \title{Concatenates two strings}
#
# @synopsis
#
# \arguments{
#  \item{s}{The string to be concatenated to this string.}
# }
#
# \description{
#  Concatenates two strings into a third string.
# }
#
# \value{
#   Returns the concatenated string.
# }
#
# @author
#
# \examples{
#   s1 <- String("Hello")
#   s2 <- String("world!")
#   s3 <- concat(s1, " ")         # "Hello "
#   s4 <- concat(s3, s2)          # "Hello world!"
#   concat("Hello", " world!")    # "Hello world!"
# }
#*/###########################################################################
setMethodS3("concat", "String", function(this, s, ...) {
  str <- as.character(this);
  s <- paste(str, s, sep="");
  String(s);
})

# This will make the function available for regular character strings too.
setMethodS3("concat", "default", function(obj, ...) {
  concat(String(obj), ...);
})



###########################################################################/**
# @RdocMethod endsWith
#
# \title{Tests if the string ends with specified suffix}
#
# @synopsis
#
# \arguments{
#  \item{suffix}{The suffix.}
# }
#
# \description{
#  Tests if the string ends with a suffix or not.
# }
#
# \value{
#   Returns @TRUE if the string ends with the given suffix, otherwise
#   @FALSE.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   endsWith(s, "world")              # FALSE
#   endsWith(s, "ld!")                # TRUE
#   endsWith("Hello world!", "ld!")   # TRUE
# }
#*/###########################################################################
setMethodS3("endsWith", "String", function(this, suffix, ...) {
  suffix <- as.character(suffix);
  res <- regexpr(paste(suffix,"$",sep=""), as.character(this));
  (res[[1]] != -1);
})


# This will make the function available for regular character strings too.
setMethodS3("endsWith", "default", function(obj, ...) {
  endsWith(String(obj), ...);
}, conflict="quiet")



###########################################################################/**
# @RdocMethod equals
#
# \title{Compares this string to the specified object}
#
# @synopsis
#
# \arguments{
#  \item{obj}{The object to compared to.}
# }
#
# \description{
#  Compares this string to the specified object. The result is @TRUE
#  if and only if the argument is not null and is a \code{String} object or
#  a character string that represents the same sequence of characters as
#  this object.
# }
#
# \value{
#   Returns @TRUE if the the strings are equal, otherwise @FALSE.
# }
#
# @author
#
# \examples{
#   s1 <- String("Hello world!")
#   s2 <- String("hello world!")
#   s3 <- String("Hello world!")
#
#   equals(s1, s1)         # TRUE
#   equals(s1, s2)         # FALSE
#   equals(s1, s3)         # TRUE
#   equals("abc", "ABC")   # FALSE
# }
#
# \seealso{
#  @seemethod "equalsIgnoreCase", @seemethod "compareTo",
#  @seemethod "regionMatches"
# }
#*/###########################################################################
setMethodS3("equals", "String", function(this, obj, ...) {
  (as.character(this) == as.character(obj));
})



###########################################################################/**
# @RdocMethod equalsIgnoreCase
#
# \title{Compares this String to another String, ignoring case considerations}
#
# @synopsis
# 
# \arguments{
#  \item{obj}{The object to compared to.}
# }
#
# \description{
#  Compares this String to another object, ignoring case considerations. 
#  The result is @TRUE
#  if and only if the argument is not null and is a \code{String} object or
#  a character string that represents the same sequence of characters as
#  this object ignoring the case.
# }
#
# \value{
#   Returns @TRUE if the the strings are equal, otherwise @FALSE.
# }
#
# @author
#
# \examples{
#   s1 <- String("Hello world!")
#   s2 <- String("hello world!")
#   s3 <- String("Hello world!")
#
#   equals(s1, s2)                   # FALSE
#   equalsIgnoreCase(s1, s2)         # TRUE
#   equalsIgnoreCase("abc", "ABC")   # TRUE
# }
#
# \seealso{
#  @seemethod "equals", @seemethod "compareTo",
#  @seemethod "regionMatches"
# }
#*/##########################################################################
setMethodS3("equalsIgnoreCase", "String", function(this, obj, ...) {
  str <- as.character(this);
  s <- as.character(obj);
  if (nchar(str) == nchar(s))
    FALSE
  else
    (length(grep(s, str, ignore.case=TRUE)) != 0);
})


# This will make the function available for regular character strings too.
setMethodS3("equalsIgnoreCase", "default", function(obj, ...) {
  equalsIgnoreCase(String(obj), ...);
})


###########################################################################/**
# @RdocMethod getBytes
#
# \title{Converts the string into a vector of bytes}
#
# @synopsis
#
# \description{
#  Converts the string into a vector of bytes (integers) according to the
#  ASCII table.
# }
#
# \value{
#   Returns a vector of integers in the range [0,255]. If the string is empty
#   an empty vector is returned.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   bfr <- getBytes(s)  # c(72,101,108,108,111,32,119,111,114,108,100,33)
#   bfr <- getBytes("Hello world!")
# }
#
# \seealso{
#   @seemethod "toCharArray", @see "R.oo::charToInt",
#   @see "R.oo::intToChar"
# }
#*/###########################################################################
setMethodS3("getBytes", "String", function(this, ...) {
  s <- toCharArray(this);
  if (length(s) == 0) 
    integer(0)
  else
    charToInt(s);
})


# This will make the function available for regular character strings too.
setMethodS3("getBytes", "default", function(obj, ...) {
  getBytes(String(obj), ...);
})



###########################################################################/**
# @RdocMethod indexOf
#
# \title{Gets the first position of a character or a substring}
#
# @synopsis
#
# \arguments{
#  \item{fromIndex}{The index to start the search from.}
# }
#
# \description{
#  Gets the first position of the character or the substring \code{s} 
#  starting from \code{fromIndex}.
# }
#
# \value{
#   Returns the position. If no such @character exists, \code{-1} is returned.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   indexOf(s, "l")               #  2
#   indexOf(s, "l", fromIndex=5)  #  9
#   indexOf(s, "world")           #  6
#   indexOf(s, "a")               # -1
#   indexOf("Hello world!", "l")  #  2
# }
#
# \seealso{@seemethod "lastIndexOf", @seemethod "charAt"}
#*/###########################################################################
setMethodS3("indexOf", "String", function(this, s, fromIndex=0, ...) {
  str <- as.character(this);
  s <- as.character(s);

  # Make the search string regexpr safe.
  s <- gsub("[*]", "[*]", s);
  s <- gsub("[.]", "[.]", s);

  if (fromIndex != 0) {
    len <- nchar(str);
    str <- substr(str, fromIndex+1, len);
  }

  res <- regexpr(s, str);
  pos <- res[[1]];
  if (pos == -1)
    -1
  else
    fromIndex+pos-1;
})


# This will make the function available for regular character strings too.
setMethodS3("indexOf", "default", function(obj, ...) {
  indexOf(String(obj), ...);
})



###########################################################################/**
# @RdocMethod lastIndexOf
#
# \title{Gets the last position of a character or a substring}
#
# @synopsis
#
# \arguments{
#  \item{fromIndex}{The index to start the search from.}
# }
#
# \description{
#  Gets the last position of the character or the substring \code{s} 
#  starting from \code{fromIndex}.
# }
#
# \value{
#   Returns the position. If no such @character exists, \code{-1} is returned.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   lastIndexOf(s, " ")               # 5
#   lastIndexOf(s, "l")               # 9
#   lastIndexOf(s, "l", fromIndex=5)  # 3
#   lastIndexOf("Hello world!", " ")  # 5
# }
#
# \seealso{@seemethod "indexOf", @seemethod "charAt"}
#*/###########################################################################
setMethodS3("lastIndexOf", "String", function(this, s, fromIndex=0, ...) { 
  str <- as.character(this);
  s <- as.character(s);
  s.len <- nchar(s);

  # Make the search string regexpr safe.
  s <- gsub("[*]", "[*]", s);
  s <- gsub("[.]", "[.]", s);

  # Add a STOP marker to the end of match string.
  if (str != "\1") {
    str <- paste(str, "\1", sep="");
  } else {
    str <- paste(str, "\2", sep="");
  }
  len <- nchar(str);
  if (fromIndex != 0)
    dummy <- strsplit(substr(str,fromIndex+1,len), s)
  else
    dummy <- strsplit(str, s);

  parts <- dummy[[1]];
  nparts <- length(parts);
  if (nparts > 1) {
    suffix.length <- nchar(parts[nparts]);
    pos <- nchar(str)-suffix.length-s.len;
    if (pos < len)
      return(pos);
  }
  
  -1;
})


# This will make the function available for regular character strings too.
setMethodS3("lastIndexOf", "default", function(obj, ...) {
  lastIndexOf(String(obj), ...);
})



###########################################################################/**
# @RdocMethod nchar
# \alias{String.length}
#
# \title{Gets the number of characters in the string}
#
# @synopsis
#
# \description{
#  Gets the number of @characters in the string.
# }
#
# \value{
#   Returns the number of @characters. If the string is empty, 0 is returned.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   nchar(s)               # 12
#   nchar("Hello world!")  # 12
# }
#*/###########################################################################
setMethodS3("nchar", "String", function(this, ...) {
  nchar(as.character(this), ...);
})



setMethodS3("length", "String", function(x) { 
  # To please R CMD check
  this <- x;

  nchar(this); 
}, appendVarArgs=FALSE)





###########################################################################/**
# @RdocMethod regionMatches
#
# \title{Tests if two string regions are equal}
#
# @synopsis
#
# \arguments{
#  \item{toffset}{The starting offset of the subregion in this string.}
#  \item{other}{The other string.}
#  \item{ooffset}{The starting offset of the subregion in the other string.}
#  \item{len}{The number of @characters to compare.}
#  \item{ignoreCase}{If @TRUE case is ignored.}
# }
#
# \description{
#  Tests if two string regions are equal. 
# }
#
# \value{
#   Returns @TRUE if the specified subregion of this string matches 
#   the specified subregion of the other string, @FALSE otherwise.
# }
#
# @author
#
# \examples{
#   s1 <- String("Hello world!")
#   s2 <- String("Oh, hello there!")
#   regionMatches(s1, 1, s2, 5, 5)        # TRUE
#   regionMatches(s2, 5, s1, 1, 5)        # TRUE
#   regionMatches(s1, 0, s2, 4, 5)        # FALSE
#   regionMatches(s1, 0, s2, 4, 5, TRUE)  # TRUE
#
#   s1 <- "Hello world!"
#   s2 <- "Oh, hello there!"
#   regionMatches(s1, 1, s2, 5, 5)        # TRUE
#   regionMatches(s2, 5, s1, 1, 5)        # TRUE
#   regionMatches(s1, 0, s2, 4, 5)        # FALSE
#   regionMatches(s1, 0, s2, 4, 5, TRUE)  # TRUE
# }
#
# \seealso{
#  @seemethod "compareTo", 
#  @seemethod "equals", @seemethod "equalsIgnoreCase"
# }
#*/###########################################################################
setMethodS3("regionMatches", "String", function(this, toffset, other, ooffset, len, ignoreCase=FALSE, ...) {
  if (toffset == -1 || ooffset == -1)
    return(FALSE);

  str <- as.character(this);
  str.length <- nchar(str);
  if (toffset+len > str.length)
    return(FALSE);

  other <- as.character(other);
  other.length <- nchar(other);
  if (ooffset+len > other.length)
    return(FALSE);

  ts <- substr(str, toffset+1, toffset+len);
  os <- substr(other, ooffset+1, ooffset+len);
  
  (length(grep(ts, os, ignore.case=ignoreCase)) != 0);
})


# This will make the function available for regular character strings too.
setMethodS3("regionMatches", "default", function(obj, ...) {
  regionMatches(String(obj), ...);
})



###########################################################################/**
# @RdocMethod replace
#
# \title{Replaces all occurrences of oldChar in this string with newChar}
#
# @synopsis
#
# \arguments{
#  \item{oldChar}{The old @character.}
#  \item{newChar}{The new @character.}
# }
#
# \description{
#  Replaces all occurrences of \code{oldChar} in this string with 
#  \code{newChar}.
# }
#
# \value{
#   Returns the new string.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   replace(s, "!", ".")                        # "Hello world."
#   replace(String("Hello world!"), "!", ".")   # "Hello world."
# }
#*/###########################################################################
setMethodS3("replace", "String", function(this, oldChar, newChar, ...) {
  if (nchar(oldChar) != 1)
    stop("oldChar is not a single character.");
  if (nchar(newChar) != 1)
    stop("newChar is not a single character.");
  s <- chartr(oldChar, newChar, as.character(this));
  String(s);
})




###########################################################################/**
# @RdocMethod set
#
# \title{Sets a new value of this string}
#
# @synopsis
#
# \arguments{
#  \item{value}{The new string to used.}
# }
#
# \description{
#  Sets a new value of this string.
# }
#
# @author
#
# \examples{
#   s1 <- String("Hello world!")
#   s1$set("Hello there!")
# }
#*/###########################################################################
setMethodS3("set", "String", function(this, str, ...) {
  if (is.null(str)) stop("setString: Can't set string to NULL.");
  this$str <- as.character(str);
  invisible(this);
})




###########################################################################/**
# @RdocMethod startsWith
#
# \title{Tests if the string starts with specified prefix}
#
# @synopsis
#
# \arguments{
#  \item{prefix}{The prefix.}
# }
#
# \description{
#  Tests if the string starts with a prefix or not.
# }
#
# \value{
#   Returns @TRUE if the string starts with the given prefix, otherwise
#   @FALSE.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   startsWith(s, "Hello")               # TRUE
#   startsWith(s, "hello")               # FALSE
#   startsWith("Hello world!", "Hello")  # TRUE
# }
#*/###########################################################################
setMethodS3("startsWith", "String", function(this, prefix, ...) {
  prefix <- as.character(prefix);
  res <- regexpr(paste("^",prefix,sep=""), as.character(this));
  (res[[1]] != -1);
})


# This will make the function available for regular character strings too.
setMethodS3("startsWith", "default", function(obj, ...) {
  startsWith(String(obj), ...);
})



###########################################################################/**
# @RdocMethod substring
#
# \title{Gets a substring of this string}
#
# @synopsis
#
# \arguments{
#  \item{beginIndex}{The beginning index, inclusive.}
#  \item{endIndex}{The ending index, exclusive.}
# }
#
# \description{
#  Gets a substring of this string. The substring begins at the position
#  \code{beginIndex} and extends to the @character at position 
#  \code{endIndex-1}. 
# }
#
# \value{
#   Returns the substring.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   substring(s, 6,11)                    # "world"
#   substring(s, 6)                       # "world!"
#   substring(String("Hello world!"), 6)  # "world!"
# }
#*/###########################################################################
setMethodS3("substring", "String", function(this, beginIndex, endIndex=NULL, ...) {
  s <- as.character(this);
  if (is.null(endIndex)) endIndex <- nchar(s);
  s <- substr(s, beginIndex+1, endIndex);
  String(s);  
})





###########################################################################/**
# @RdocMethod toCharArray
#
# \title{Converts this string to a vector of characters}
#
# @synopsis
#
# \description{
#  Converts this string to a vector of @characters.
# }
#
# \value{
#   Returns the vector of @characters.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   toCharArray(s)  # c("H","e","l","l","o"," ","w","o","r","l","d","!")
#   toCharArray("Hello world!")
# }
#
# \seealso{@seemethod "getBytes"}
#*/###########################################################################
setMethodS3("toCharArray", "String", function(this, ...) {
  unlist(strsplit(as.character(this), NULL))
})


# This will make the function available for regular character strings too.
setMethodS3("toCharArray", "default", function(obj, ...) {
  toCharArray(String(obj), ...);
})


###########################################################################/**
# @RdocMethod toLowerCase
#
# \title{Converts all of the characters in this String to lower case}
#
# @synopsis
#
# \description{
#  Converts all of the characters in this String to lower case.
# }
#
# \value{
#   Returns the string, converted to lower case.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   toLowerCase(s)                     # "hello world!"
#   toLowerCase("Hello world!")        # "hello world!"
# }
#
# \seealso{@seemethod "toUpperCase"}
#*/###########################################################################
setMethodS3("toLowerCase", "String", function(this, ...) {
  s <- as.character(this);
  s <- tolower(s);
  String(s);
})


# This will make the function available for regular character strings too.
setMethodS3("toLowerCase", "default", function(obj, ...) {
  toLowerCase(String(obj), ...);
})




###########################################################################/**
# @RdocMethod toUpperCase
#
# \title{Converts all of the characters in this String to upper case}
#
# @synopsis
#
# \description{
#  Converts all of the characters in this String to upper case.
# }
#
# \value{
#   Returns the string, converted to upper case.
# }
#
# @author
#
# \examples{
#   s <- String("Hello world!")
#   toUpperCase(s)                # "HELLO WORLD!"
#   toUpperCase("Hello world!")   # "HELLO WORLD!"
# }
#
# \seealso{@seemethod "toLowerCase"}
#*/###########################################################################
setMethodS3("toUpperCase", "String", function(this, ...) {
  s <- as.character(this);
  s <- toupper(s);
  String(s);
})


setMethodS3("as.character", "String", function(x, ...) {
  # To please R CMD check
  this <- x;

  this$str;
})


# This will make the function available for regular character strings too.
setMethodS3("toUpperCase", "default", function(obj, ...) {
  toUpperCase(String(obj), ...);
})


###########################################################################/**
# @RdocMethod trim
#
# \title{Removes white space from both ends of this string}
#
# @synopsis
#
# \description{
#  Removes white space from both ends of this string. The following character
#  values are all white spaces: 
#  \code{"\\t"} (0x09 - HORIZONTAL TABULATION),
#  \code{"\\n"} (0x0A - NEW LINE),
#  \code{"\\v"} (0x0B - VERTICAL SPACE),
#  \code{"\\f"} (0x0C - FORM FEED), 
#  \code{"\\r"} (0x0D  - CARRIAGE RETURN), and
#  \code{" "}   (0x20 - SPACE).
# }
#
# \value{
#   Returns this string, with white space removed from the front and end.
# }
#
# @author
#
# \examples{
#   s <- String("\t  Hello world!\n")
#   print(trim(s))                           # "Hello world!"
#   print(trim("\t  Hello world!\n"))        # "Hello world!"
# }
#*/###########################################################################
setMethodS3("trim", "String", function(this, ...) {
  # The difference between gsub() and sub() is that the latter only replaces
  # the first occurrence of a pattern whereas gsub() replaces all occurrences,
  # meaning that sub() should be faster. /HB 03-01-20
  # Whitespace is [ \t\n\r\f\v] (see above)
  s <- sub("^[ \t\n\r\f\v]*", "", as.character(this));
  s <- sub("[ \t\n\r\f\v]*$", "", s);
})





###########################################################################/**
# @RdocMethod valueOf
#
# \title{Returns the string representation of the specified object}
#
# @synopsis
#
# \arguments{
#  \item{x}{The object to be represented as a string.}
# }
#
# \description{
#  Returns the string representation of the specified object.
# }
#
# \value{
#   Returns the string representation of the specified object.
# }
#
# @author
#
# \examples{
#   String$valueOf(TRUE)    # "TRUE"
#   String$valueOf(42)      # "42"
#   String$valueOf(42.01)   # "42.01"
#   String$valueOf("same")  # "same"
#   String$valueOf(NA)      # "NA"
#   String$valueOf(NaN)     # "NaN"
#   String$valueOf(Inf)     # "Inf"
#   String$valueOf(0/0)     # "0/0"
# }
#*/###########################################################################
setMethodS3("valueOf", "String", function(this, x, ...) {
  if (inherits(x, "String"))
    x <- as.character(x)
  else if (length(x) > 1) {
    if (is.numeric(x))
      x <- intToChar(x);
    x <- paste(x, collapse="");
  }
  String(as.character(x));
}, static=TRUE);



###########################################################################/**
# @RdocMethod hashCode
#
# \title{Returns a hash code for this string}
#
# @synopsis
#
# \description{
#  Returns a hash code for this string. The hashcode for a \code{String} object
#  is computed as 
# 
#   \code{s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]}
# 
#  using int arithmetic, where \code{s[i]} is the \emph{i}th character of the
#  string, \code{n} is the length of the string, and \code{^} indicates
#  exponentiation. (The hash value of the empty string is zero.)
# }
#
# \value{
#   Returns the hash code value of this String.
# }
#
# @author
#
# \examples{
#   s1 <- String("Hello world!")
#   s2 <- String("Hello world!")
#   hashCode(s1)              # -52967168
#   hashCode(s2)              # -52967168
#   hashCode("Hello world!")  # -52967168
# }
#*/###########################################################################
setMethodS3("hashCode", "String", function(this, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  as.Java.int <- function(x, ...) {
    Integer.MIN.VALUE <- -2147483648;
    Integer.MAX.VALUE <-  2147483647;
    Integer.RANGE <- Integer.MAX.VALUE-Integer.MIN.VALUE + 1;
    x <- (x-Integer.MIN.VALUE) %% Integer.RANGE + Integer.MIN.VALUE;
    as.integer(x);
  }
 
  s <- getBytes(this);
  n <- length(s);
  if (n == 0)
    return(0);
  hashCode <- 0;
  for (k in 1:n)
    hashCode <- hashCode + s[k]*31^(n-k);
  # Convert into range of Java int.
  as.Java.int(hashCode);
})


setMethodS3("print", "String", function(x, ...) {
  # To please R CMD check...
  this <- x;

  print.default(as.character(this));
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Already defined in R.oo
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# This will make the function available for regular character strings too.
# setMethodS3("trim", "default", function(obj, ...) {
#  trim(String(obj), ...);
# })

# This will make the function available for regular character strings too.
# setMethodS3("hashCode", "default", function(obj, ...) {
#   hashCode(String(obj), ...);
# })


############################################################################
# HISTORY:
# 2006-01-31
# o Rdoc bug fix: 'example' to 'examples'.
# 2005-05-03
# o From R v2.1.0, nchar() takes two arguments.
# 2003-02-20
# o Updated trim() to include \v as a whitespace character. Added some 
#   extra comments to trim() too.
# 2002-12-09
# o Removed equals.default(), scince it is now in the R.oo package.
# 2002-12-01
# o Removed obsolete default functions for trim() and hashCode(). They are
#   now defined in R.oo.
# 2002-03-08
# * Updated the Rdoc comments.
# * Minor modification of the code of endsWith() to make it more similar to
#   startsWith().
# 2002-01-29
# * Rewritten to make use of setCl assS3 and setMethodS3.
# 2002-01-02
# * Removed length.default since it does not work with R v1.4.0!
# 2001-08-05
# * Update to the new static method calls.
# * Removed valueOf0.
# 2001-08-04 [v0.23]
# * Removed all get- and putObject().
# * Updated the UseMethods.
# 2001-08-03
# * Update equals.default so for instance equals(NULL, NULL) is TRUE.
# 2001-06-29
# * Made all as.String be smart; if asString is NULL, then if the object is
#   an Object then as.String=TRUE, else as.String=FALSE.
# 2001-06-28
# * Made a lot of *.default functions, which works with regular [R] strings.
# * Bug fix: Forgot to support white spaces in trim().
# * Updated most of the Rdoc comments.
# 2001-05-14
# * Added get_Internal_References() for improving gco() performance.
# 2001-05-13
# * Implemented getBytes() and hashCode().
# 2001-05-12
# * Added the argument as.String to several methods.
# 2001-05-05
# * Removed to all methods in the constructor.
# 2001-05-04
# * Now supports formal attributes.
# 2001-04-27
# * Added support for converting from byte vector to String.
# 2001-04-03
# * Forgot one this$self().
# * Added support for static method valueOf().
# 2001-03-16
# * Implemented most of the functionality found in java.lang.String.
# * Now making use of the new functionCaller in Object.
# 2001-03-14
# * Created.
############################################################################

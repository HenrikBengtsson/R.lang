############################################################################
# Author: Henrik Bengtsson, henrikb@braju.com
#
# See R.io::Java
############################################################################

setMethodS3("as.Java.int", "default", function(x, ...) {
  Integer.MIN.VALUE <- -2147483648;
  Integer.MAX.VALUE <-  2147483647;
  Integer.RANGE <- Integer.MAX.VALUE-Integer.MIN.VALUE + 1;
  x <- (x-Integer.MIN.VALUE) %% Integer.RANGE + Integer.MIN.VALUE;
  as.integer(x);
})


setMethodS3("as.Java.long", "default", function(x, ...) {
#  x <- as.integer(x);
  Long.MIN.VALUE <- -9223372036854775808;
  Long.MAX.VALUE <-  9223372036854775807;
  Long.RANGE <- Long.MAX.VALUE-Long.MIN.VALUE + 1;
  x <- (x-Long.MIN.VALUE) %% Long.RANGE + Long.MIN.VALUE;
  as.integer(x);
})


setMethodS3("as.Java.short", "default", function(x, ...) {
#  x <- as.integer(x);
  Short.MIN.VALUE <- -32768;
  Short.MAX.VALUE <-  32767;
  Short.RANGE <- Short.MAX.VALUE-Short.MIN.VALUE + 1;
  x <- (x-Short.MIN.VALUE) %% Short.RANGE + Short.MIN.VALUE;
  as.integer(x);
})


setMethodS3("as.Java.byte", "default", function(x, ...) {
  Byte.MIN.VALUE <- -128;
  Byte.MAX.VALUE <-  127;
  Byte.RANGE <- Byte.MAX.VALUE-Byte.MIN.VALUE + 1;
  x <- (x-Byte.MIN.VALUE) %% Byte.RANGE + Byte.MIN.VALUE;
  as.integer(x);
})


setMethodS3("getBits", "default", function(i, ...) {
  ready <- FALSE;
  bits <- c();
  while (!ready) {
    bit <- i %% 2;
    bits <- c(bits, bit);
    i <- i %/% 2;
    ready <- (i==0);
  }
  bits;
})


setMethodS3("getBit", "default", function(i, n, ...) {
  bits <- getBits(i);
  bit <- bits[n];
  if (is.na(bit))
    0
  else
    bit;
})



############################################################################
# HISTORY:
# 2002-10-23
# o Move all these methods to the Java class.
# 2002-01-29
# * Rewritten to make use of setMethodS3.
# 2001-08-05
# * Updated the UseMethods.
# 2001-07-28
# * Moved the ASCII stuff to R.base.
# 2001-07-13
# * Made all methods using UseMethod.
# 2001-06-28
# * In all as.Java.X as.integer(x) is now done in the end instead of the
#   beginning. When it was done first and a too large number was given,
#   as.integer(x) returned NA.
# 2001-06-07
# * Added [R] documents to ASCII, charToInt and intToChar.
# * Moved getBit() and getBits() from OOCore.R to this file.
# 2001-04-02
# * Created!
############################################################################

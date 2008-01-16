# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

substring <- appendVarArgs(substring)
replace <- appendVarArgs(replace)

# The base::nchar() of R v2.5.0 and R v2.6.0 are not compatible.
# The below is a workaround for this.
# nchar <- appendVarArgs(nchar)
nchar <- function(...) {
  base::nchar(...);
}


############################################################################
# HISTORY:
# 2008-01-15
# o Added workaround for appendVarArgs(nchar).
# 2005-02-20
# o Created to please R CMD check.
############################################################################

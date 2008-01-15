# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

substring <- appendVarArgs(substring)
replace <- appendVarArgs(replace)
nchar <- appendVarArgs(nchar)


############################################################################
# HISTORY:
# 2005-02-20
# o Created to please R CMD check.
############################################################################

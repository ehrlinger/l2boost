
#---------------------------------------------------------------------
# l2boost (FRIEDMAN/DISCRETE/HYBRID/LARS)
# 
# notes:
# nu is ignored in LARS/LIMIT
# rescales x
# y is centered --- > mean is returned as part of the object
# !!! remove x-columns with NA's !!! CAUTION
#---------------------------------------------------------------------
#' @rdname l2boost
#' @export l2boost
l2boost <-
function(x, ...)UseMethod("l2boost")

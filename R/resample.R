#' robust resample method allows resampling when length(x) <= 1.
resample <-
function(x, size = 1, ...) {
    if(length(x) <= 1) {
      if(!missing(size) && size == 0) x[FALSE] else x
    }
    else {
      sample(x, size, ...)
    }
}

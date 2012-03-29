
#' determines which index minimizes a vector, with some elements excluded
which.min.ind <-
function(vec, exclude = NULL) {
   if (!is.null(exclude)) {
     if (length(vec[-exclude]) == 0) return(NULL)
     min.ind <- which(vec == min(vec[-exclude], na.rm = TRUE))
     setdiff(min.ind, exclude)
   }
   else {
     which(vec == min(vec, na.rm = TRUE))
   }
}

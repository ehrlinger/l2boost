#' determines which index maximizes a vector, with some elements excluded
which.max.ind <-
function(vec, exclude = NULL) {
   if (!is.null(exclude)) {
     if (length(vec[-exclude]) == 0) return(NULL)
     max.ind <- which(vec == max(vec[-exclude], na.rm = TRUE))
     setdiff(max.ind, exclude)
   }
   else {
     which(vec == max(vec, na.rm = TRUE))
   }
}

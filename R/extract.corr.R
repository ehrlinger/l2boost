#' extract correlations: uses a smart reduction for elastic net modification
extract.corr <-
function(x, l, enet, n.org) {
  if (enet) {
    e.c <- c(t(x[1:n.org, ]) %*% x[1:n.org, l])
    e.c[l] <- 1
    e.c
  }
  else {
    c(t(x) %*% x[, l])
  }
}

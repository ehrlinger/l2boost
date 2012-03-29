
#---------------------------------------------------------------------
# l2boost plots
# ...  currently allows for rho/coef paths and will also plot cv objects
# ... standarized rho paths are gradient-correlation paths
# ... standarized coefficient paths are coefficient paths for standardized x
# SHOULD WORK FOR ALL l2boost variants
#---------------------------------------------------------------------

#' nice standard errors for plots
error.bars <-
function(x, upper, lower, width = 0.001, max.M = 100, ...) {
        M <- length(x)
        # thin-out x for presentable plots
        if (M > max.M) {
          pt <- unique(round(seq.int(1, max(x), length = max.M)))
        }
        else {
          pt <- 1:M
        }
        xlim <- range(x[pt])
        barw <- diff(xlim) * width
        segments(x[pt], upper[pt], x[pt], lower[pt], ...)
        segments(x[pt] - barw, upper[pt], x[pt] + barw, upper[pt], ...)
        segments(x[pt] - barw, lower[pt], x[pt] + barw, lower[pt], ...)
}

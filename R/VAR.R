VAR <-
function(x) {if (all(is.na(x))) return(NA) else return(var(x, na.rm=T))}

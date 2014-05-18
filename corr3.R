source("corr.R")
cr <- corr("specdata", 2000)
n <- length(cr)
cr <- corr("specdata", 1000)
cr <- sort(cr)
paste(c(n, round(cr, 4)), collapse = "\n"
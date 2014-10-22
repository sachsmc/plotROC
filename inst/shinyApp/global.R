D.tmp <- rbinom(250, 1, .5)
example <- data.frame(M1 = rnorm(250, mean = D.tmp, sd = .5), M2 = rnorm(250, mean = D.tmp, sd = 2), D = D.tmp)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

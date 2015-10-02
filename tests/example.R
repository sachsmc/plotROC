library(plotROC)


D.ex <- rbinom(100, 1, .5)

paired.data <- data.frame(M1 = rnorm(100, mean = D.ex),
                          
                          M2 = rnorm(100, mean = D.ex, sd = .4),
                          
                          M3 = runif(100), D = D.ex)


data <- data.frame(D = c(D.ex, D.ex), M = c(paired.data$M1, paired.data$M2), B = c(rep("A", 100), rep("B", 100)))

library(ggplot2)
ggroc_p <- ggplot(data, aes(m = M, d = D)) + geom_roc(n.cuts = 0) + geom_rocci(ci.at = seq(-1, 3, length.out = 25)) 
ggroc_p

svgString <- export_interactive_roc(ggroc_p)

cat("<html>\n", svgString, "\n</html>",  file = "~/Desktop/svgdevel.html")



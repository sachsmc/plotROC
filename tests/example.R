library(plotROC)

n <- 1e2
D.ex <- rbinom(n, 1, .5)

paired.data <- data.frame(M1 = rnorm(n, mean = D.ex),
                          
                          M2 = rnorm(n, mean = D.ex, sd = .4),
                          
                          M3 = runif(n), D = D.ex)


data <- data.frame(D = c(D.ex, D.ex), M = c(paired.data$M1, paired.data$M2), B = c(rep("A", n), rep("B", n)))

library(ggplot2)

ggroc_p <- ggplot(data, aes(m = M, d = D)) + geom_roc(n.cuts = 20) + facet_wrap(~ B)

ggroc_p + style_roc() + geom_rocci()

svgString <- export_interactive_roc(ggroc_p, width = 10, height = 7, hide.points = TRUE, add.cis = FALSE, sig.level = .001)

#plot_interactive_roc(ggroc_p)
cat("<html>\n", svgString, "\n</html>",  file = "~/Desktop/svgdevel.html")



library(plotROC)

n <- 1e3
D.ex <- rbinom(n, 1, .5)

paired.data <- data.frame(M1 = rnorm(n, mean = D.ex),
                          
                          M2 = rnorm(n, mean = D.ex, sd = .4),
                          
                          M3 = runif(n), D = D.ex)


data <- data.frame(D = c(D.ex, D.ex), M = c(paired.data$M1, paired.data$M2), B = c(rep("A", n), rep("B", n)))

library(ggplot2)

ggroc_p <- ggplot(data, aes(m = M, d = D, color = B)) + geom_roc(n.cuts = 20) 
direct_label(ggroc_p + style_roc(theme = theme_bw),  labels = c("A", "B"))

svgString <- export_interactive_roc(ggroc_p, width = 7, height = 7, hide.points = FALSE, add.cis = TRUE, sig.level = .001)

cat("<html>\n", svgString, "\n</html>",  file = "~/Desktop/svgdevel.html")



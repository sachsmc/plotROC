library(plotROC)

n <- 1e2
D.ex <- rbinom(n, 1, .5)

paired.data <- data.frame(M1 = rnorm(n, mean = D.ex),
                          
                          M2 = rnorm(n, mean = D.ex, sd = .4),
                          
                          M3 = round(runif(n), 1), D = D.ex)


data <- data.frame(D = c("A", "B")[c(D.ex, D.ex) + 1], M = c(paired.data$M1, paired.data$M2), B = c(rep("A", n), rep("B", n)))

library(ggplot2)

ggroc_p <- ggplot(data, aes(m = M, d = D, color = B)) + geom_roc(n.cuts = 10)
#ggroc_p <- ggplot(paired.data, aes(m = M3, d = D)) + geom_roc()
ggroc_p <- ggroc_p + style_roc() + geom_rocci()


svgString <- export_interactive_roc(ggroc_p, width = 10, height = 7, hide.points = TRUE, add.cis = TRUE, sig.level = .001)

cat("<html>\n", svgString, "\n</html>",  file = "~/Desktop/svgdevel.html")



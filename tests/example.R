library(plotROC)


D.ex <- rbinom(100, 1, .5)

paired.data <- data.frame(M1 = rnorm(100, mean = D.ex),
                          
                          M2 = rnorm(100, mean = D.ex, sd = .4),
                          
                          M3 = runif(100), D = D.ex)


data <- data.frame(D = c(D.ex, D.ex), M = c(paired.data$M1, paired.data$M2), B = c(rep("A", 100), rep("B", 100)))

library(ggplot2)
p1 <- ggplot(data, aes(m = M, d = D, color = B)) + geom_roc(n.cuts = 0) + geom_rocci() 
p1 



estimate.list <- calculate_multi_roc(paired.data, c("M2", "M1", "M3"), "D")

multi.rocplot <- multi_ggroc(estimate.list, label = c("M2", "M1", "M3"), legend = TRUE)

plot_journal_roc(multi.rocplot, lty = rep(1, 3), color = c("black", "purple", "orange"), legend = TRUE)



## ----inst, eval = FALSE--------------------------------------------------
#  devtools::install_github("sachsmc/plotROC")

## ----shiny, eval = FALSE-------------------------------------------------
#  shiny_plotROC()

## ----dataset-------------------------------------------------------------
library(plotROC)
D.ex <- rbinom(100, 1, .5)
M.ex <- rnorm(100, mean = D.ex)

## ----calc----------------------------------------------------------------
rocdata <- calculate_roc(M.ex, D.ex)
str(rocdata)

## ----test-a, fig.keep='none', results = 'asis', echo = TRUE, fig.width=6, fig.height=6----
myrocplot <- ggroc(rocdata, label = "Example")
cat(export_interactive_roc(myrocplot, cutoffs = rocdata$c, font.size = "12px", prefix = "a"))

## ----inter, eval = FALSE-------------------------------------------------
#  plot_interactive_roc(rocdata)

## ----print, fig.width = 6, fig.height = 6--------------------------------
plot_journal_roc(myrocplot, rocdata)

## ----test-a-ci, fig.keep='none', results = 'asis', echo = TRUE, fig.width=6, fig.height=6----

rocdata <- calculate_roc(M.ex, D.ex, ci = TRUE)
myrocplot <- ggroc(rocdata, label = "Example", ci = TRUE)
cat(export_interactive_roc(myrocplot, cutoffs = rocdata$c, font.size = "12px", prefix = "aci"))

## ----printci, fig.width = 6, fig.height = 6------------------------------
plot_journal_roc(myrocplot, rocdata, n.cuts = 10, ci.at = c(-.5, .5, 2.1))

## ----multistart----------------------------------------------------------
D.ex <- rbinom(100, 1, .5)

fakedata <- data.frame(M1 = rnorm(100, mean = D.ex), M2 = rnorm(100, mean = D.ex, sd = .4), M3 = runif(100), D = D.ex)
datalist <- calculate_multi_roc(fakedata, c("M1", "M2", "M3"), "D")
rocplot <- multi_ggroc(datalist)

## ----multi, fig.width = 6, fig.height = 6--------------------------------
plot_journal_roc(rocplot, datalist)

## ----test-multi, fig.keep='none', results = 'asis', echo = TRUE, fig.width=6, fig.height=6----
cat(export_interactive_roc(rocplot, cutoffs = lapply(datalist, function(d) d$c), font.size = "12px", prefix = "bmulti"))

## ----multi2, fig.width = 6, fig.height = 6-------------------------------
rocplot <- multi_ggroc(datalist, label = c("M1", "M2", "M3"))
plot_journal_roc(rocplot, datalist)

## ----test-b, fig.keep='none', results = 'asis', echo = TRUE, fig.width=6, fig.height=6----
library(ggthemes)
myplot2 <- myrocplot + theme_igray() + ggtitle("Click me")

cat(export_interactive_roc(myplot2, cutoffs = rocdata$c, font.size = "12px", prefix = "b"))

## ----print2, fig.width = 6, fig.height = 6-------------------------------
plot_journal_roc(myrocplot, rocdata) + theme_igray() + ggtitle("My awesome new biomarker")

## ----binormalsetup-------------------------------------------------------
D.ex <- rbinom(100, 1, .5)
M.ex <- rnorm(100, mean = D.ex, sd = .5)

mu1 <- mean(M.ex[D.ex == 1])
mu0 <- mean(M.ex[D.ex == 0])
s1 <- sd(M.ex[D.ex == 1])
s0 <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)

binorm_rocdata <- data.frame(c = c.ex, FPF = pnorm((mu0 - c.ex)/s0), TPF = pnorm((mu1 - c.ex)/s1))

## ----binormal, fig.keep='none', results = 'asis', echo = TRUE, fig.width=6, fig.height=6----
binorm_plot <- ggroc(binorm_rocdata, label = "Binormal")
cat(export_interactive_roc(binorm_plot, cutoffs = c.ex, font.size = "12px", prefix = "bin"))


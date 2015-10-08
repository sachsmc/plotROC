## ---- echo=FALSE---------------------------------------------------------
library(knitr)
knit_hooks$set(source = function(x, options){
  if (!is.null(options$verbatim) && options$verbatim){
    opts = gsub(",\\s*verbatim\\s*=\\s*TRUE\\s*", "", options$params.src)
    opts = gsub(",\\s*eval\\s*=\\s*FALSE\\s*", "", opts)
    bef = sprintf('\n\n    ```{r %s}\n', opts, "\n")
    stringr::str_c(
      bef, 
      knitr:::indent_block(paste(x, collapse = '\n'), "    "), 
      "\n    ```\n"
    )
  } else {
    stringr::str_c("\n\n```", tolower(options$engine), "\n", 
      paste(x, collapse = '\n'), "\n```\n\n"
    )
  }
})

## ----load, eval = FALSE--------------------------------------------------
#  devtools::install_github("hadley/ggplot2")
#  devtools::install_github("sachsmc/plotROC")
#  library(plotROC)

## ----shiny, eval = FALSE-------------------------------------------------
#  shiny_plotROC()

## ----dataset, echo = -1--------------------------------------------------
library(plotROC)
set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)

## ----calc----------------------------------------------------------------
basicplot <- ggplot(test, aes(d = D, m = M1)) + geom_roc()
basicplot

## ----warn----------------------------------------------------------------
ggplot(test, aes(d = D.str, m = M1)) + geom_roc()

## ----opts----------------------------------------------------------------
ggplot(test, aes(d = D, m = M1)) + geom_roc(n.cuts = 0)
ggplot(test, aes(d = D, m = M1)) + geom_roc(n.cuts = 5, labelsize = 5, labelround = 2)
ggplot(test, aes(d = D, m = M1)) + geom_roc(n.cuts = 50, labels = FALSE)

## ----style---------------------------------------------------------------
styledplot <- basicplot + style_roc()
styledplot
basicplot + style_roc(theme = theme_grey, xlab = "1 - Specificity")

direct_label(basicplot) + style_roc()
direct_label(basicplot, labels = "Biomarker", nudge_y = -.1) + style_roc()

## ----test-a-ci-----------------------------------------------------------
styledplot + geom_rocci()
styledplot + geom_rocci(sig.level = .01)
ggplot(test, aes(d = D, m = M1)) + geom_roc(n.cuts = 0) +
  geom_rocci(ci.at = quantile(M1, c(.1, .4, .5, .6, .9))) + style_roc()

## ----inter, eval = FALSE-------------------------------------------------
#  plot_interactive_roc(basicplot)

## ----int-no, fig.keep='none', results = 'asis', eval = FALSE, verbatim = TRUE----
#  cat(
#    export_interactive_roc(basicplot,
#                          prefix = "a")
#    )

## ----int-yes, fig.keep='none', results = 'asis', fig.width=7, fig.height=7----
cat(
  export_interactive_roc(basicplot, 
                        prefix = "a")
  )

## ----head----------------------------------------------------------------
head(test)

## ------------------------------------------------------------------------
longtest <- melt_roc(test, "D", c("M1", "M2"))
head(longtest)

## ----group---------------------------------------------------------------
ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
ggplot(longtest, aes(d = D, m = M)) + geom_roc() + facet_wrap(~ name) + style_roc()
ggplot(longtest, aes(d = D, m = M, linetype = name)) + geom_roc() + geom_rocci()
ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
pairplot <- ggplot(longtest, aes(d = D, m = M, color = name)) + 
  geom_roc(show.legend = FALSE) + style_roc()
direct_label(pairplot)

pairplot + geom_rocci()
pairplot + geom_rocci(linetype = 1)
pairplot + geom_rocci()

## ----grp1, fig.keep='none', results = 'asis', fig.width=7, fig.height=7----
cat(
  export_interactive_roc(direct_label(pairplot), 
                        prefix = "b")
  )

## ----grp3, fig.keep='none', results = 'asis', fig.width=12, fig.height=6----
cat(
  export_interactive_roc(
    ggplot(longtest, aes(d = D, m = M)) + geom_roc() + facet_wrap(~ name), 
                        prefix = "c", width = 10, height = 5)
  )

## ----ex2-----------------------------------------------------------------
D.cov <- rbinom(400, 1, .5)
gender <- c("Male", "Female")[rbinom(400, 1, .49) + 1]
M.diff <- rnorm(400, mean = D.cov, sd = ifelse(gender == "Male", .5, 1.5))

test.cov <- data.frame(D = D.cov, gender = gender, M = M.diff)

## ----covplot-------------------------------------------------------------
bygend <- ggplot(test.cov, aes(d = D, m = M, color = gender)) + geom_roc(show.legend = FALSE)
direct_label(bygend) + style_roc()

## ----themes--------------------------------------------------------------
basicplot + 
  style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))

## ----binorm--------------------------------------------------------------
D.ex <- test$D
M.ex <- test$M1
mu1 <- mean(M.ex[D.ex == 1])
mu0 <- mean(M.ex[D.ex == 0])
s1 <- sd(M.ex[D.ex == 1])
s0 <- sd(M.ex[D.ex == 0])
c.ex <- seq(min(M.ex), max(M.ex), length.out = 300)

binorm.roc <- data.frame(c = c.ex, 
                             FPF = pnorm((mu0 - c.ex)/s0), 
                             TPF = pnorm((mu1 - c.ex)/s1)
                             )

binorm.plot <- ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity") + style_roc(theme = theme_grey)
binorm.plot

## ----surv----------------------------------------------------------------
library(survivalROC)
survT <- rexp(350, 1/5)
cens <- rbinom(350, 1, .1)
M <- -8 * sqrt(survT) + rnorm(350, sd = survT)

sroc <- lapply(c(2, 5, 10), function(t){
  stroc <- survivalROC(Stime = survT, status = cens, marker = M, 
                       predict.time = t, method = "NNE", 
                       span = .25 * 350^(-.2))
  data.frame(TPF = stroc[["TP"]], FPF = stroc[["FP"]], 
             c = stroc[["cut.values"]], 
             time = rep(stroc[["predict.time"]], length(stroc[["FP"]])))
})

sroclong <- do.call(rbind, sroc)

ggplot(sroclong, aes(x = FPF, y = TPF, label = c, color = time)) + 
  geom_roc(labels = FALSE, stat = "identity") + style_roc(theme = theme_gray)


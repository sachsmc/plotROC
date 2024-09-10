context("Calculate ROC")

set.seed(420)

num_obs <- 1e3

D.ex <- rbinom(n = num_obs, 1, .5)
T.ex <- rnorm(n = num_obs, mean = D.ex, sd = .5)
D.str <- c("Healthy", "Ill")[D.ex + 1]


testdata <- data.frame(T1 = T.ex, T2 = T.ex + rnorm(n = num_obs), 
                       D = D.ex, D.str = D.str, 
                       grp = c("A", "B")[rbinom(num_obs, 1, .4) + 1], 
                       grp2 = c("X", "Y")[rbinom(num_obs, 1, .6) + 1])
testroc <- ggplot_build(ggplot(testdata, aes(m = T1, d = D)) + geom_roc())$data[[1]]
ggroc_p <- ggplot(testdata, aes(m = T1, d = D)) + geom_roc() + geom_rocci()

layers <- sapply(ggroc_p$layers, function(l) class(l$geom)[1])

test_that("Calculate ROC returns a valid ROC curve", {
  
  expect_true(max(testroc$true_positive_fraction) <= 1)
  expect_true(max(testroc$false_positive_fraction) <= 1)
  expect_true(min(testroc$true_positive_fraction) >= 0)
  expect_true(min(testroc$false_positive_fraction) >= 0)
  
  expect_warning(print(ggplot(testdata, aes(m = T1, d = D + 1)) + geom_roc()))
  expect_error(print(ggplot(testdata, aes(m = D, d = T1)) + geom_roc()))
  
})

test_that("ggroc returns a ggplot object", {
  
  expect_true(ggplot2::is.ggplot(ggroc_p))
  expect_true("GeomRoc" %in% layers)
  expect_true("GeomRocci" %in% layers)
  
})

test_that("Non 0/1 labels work, with warning", {
  
  expect_warning(print(ggplot(testdata, aes(m = T1, d = D.str)) + stat_roc()))
  
})


test_that(desc = "plotROC can be thinned when there is too much data",
          code = {
            
            g <- ggplot(data = testdata, mapping = aes(m = T1, d = D)) + stat_roc(max.num.points = 20)
            
            expect_is(object = g,
                      class = 'ggplot')
            
            whole_plot_data <- ggplot_build(ggplot(testdata, aes(m = T1, d = D)) + stat_roc())$data[[1]]
            thinned_plot_data <- ggplot_build(ggplot(testdata, aes(m = T1, d = D)) + stat_roc(max.num.points = 20))$data[[1]]
            
            expect_equal(object = nrow(whole_plot_data), expected = num_obs + 1)
            expect_equal(object = nrow(thinned_plot_data), expected = 21)
          })


test_that(desc = "Calc AUC modification works", 
          code = {
            
            expect_equal(nrow(calc_auc(ggroc_p)), 1)
            ggroc_2 <- ggplot(testdata, aes(m = T1, d = D, color = grp)) + 
              geom_roc() + geom_rocci()
            aucs <- calc_auc(ggroc_2)
            expect_equal(nrow(aucs), 2)
            autab2 <- calc_auc(ggplot(testdata, aes(m = T1, d = D, color = grp)) + geom_roc() + 
                                 facet_wrap(~ grp2))
            expect_equal(nrow(autab2), 4)
            expect_true("grp" %in% colnames(autab2))
            
          }
          )


test_that(desc = "Calc AUC works for data passed within geom_roc, and multiple rocs", {
  
  ## Create data
  set.seed(123)
  x <- rnorm(100)
  y <- round(plogis(3*x + rnorm(100, sd = 5)))
  df <- data.frame(x, y, gp = c("A", "B"), pan = sample(c("Z", "W"), 100, replace = TRUE))
  
  x2 <- rnorm(100)
  y2 <- round(plogis(3*x + rnorm(100, sd = 2)))
  df2 <- data.frame(x2, y2, gp = c("X", "Y"), pan = sample(c("Z", "W"), 100, replace = TRUE))
  # Here, aesthetics defined in ggplot
  p1 <- ggplot(df, aes(d = y, m = x, color = gp)) + geom_roc()
  
  # Here they are defined in geom_roc. The plots are the same
  p2 <- ggplot(df) + geom_roc(aes(d = y, m = x, color = gp))
  
  auc1 <- calc_auc(p1)
  auc2 <- calc_auc(p2) # Error
  
  expect_equal(auc1$AUC, auc2$AUC)
  
  p3 <- ggplot(df, aes(d = y, m = x, color = gp)) + geom_roc() + 
    geom_roc(data = df2, aes(d = y2, m = x2, color = gp))
  
  auc3 <- calc_auc(p3)
  expect_equal(nrow(auc3), 4)
  
  
  p4 <- ggplot(df, aes(d = y, m = x, color = gp)) + geom_roc() + 
    geom_roc(data = df2, aes(d = y2, m = x2, color = gp)) + 
    facet_wrap(~ pan)
  
  expect_equal(nrow(calc_auc(p4)), 8)
  
})

test_that(desc = "Calc AUC works for specific cut-offs", {
  set.seed(123)
  x <- runif(1000)
  y <- round(x)
  df <- data.frame(x, y, gp = c("A", "B"))

  p1 <- ggplot(df, aes(d = y, m = x, color = gp)) + geom_roc()

  auc1 <- calc_auc(p1)
  auc2 <- calc_auc(p1, 0.25)

  expect_true(all(auc1$AUC - auc2$AUC - 0.75 < 0.01))
})


test_that(desc = "Single class or empty group works", {
  set.seed(123)
  x <- runif(1)
  y <- round(x)
  df <- data.frame(x, y)
  p1 <- ggplot(df, aes(d = y, m = x)) + geom_roc()
  expect_warning(print(p1), "Single class or empty group detected. Proceeding with an empty plot.")
})

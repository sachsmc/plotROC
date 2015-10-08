context("Calculate ROC")

set.seed(420)
D.ex <- rbinom(100, 1, .5)
T.ex <- rnorm(100, mean = D.ex, sd = .5)
D.str <- c("Healthy", "Ill")[D.ex + 1]


testdata <- data.frame(T1 = T.ex, T2 = T.ex + rnorm(100), D = D.ex, D.str = D.str)
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

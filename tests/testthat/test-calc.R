context("Calculate ROC")

set.seed(420)
D.ex <- rbinom(100, 1, .5)
T.ex <- rnorm(100, mean = D.ex, sd = .5)
D.str <- c("Healthy", "Ill")[D.ex + 1]

exROC <- calculate_roc(T.ex, D.ex)

testdata <- data.frame(T1 = T.ex, T2 = T.ex + rnorm(100), D = D.ex, D.str = D.str)

test_that("Calculate ROC returns a valid ROC curve", {
  
  expect_true(max(exROC$TPF) <= 1)
  expect_true(max(exROC$FPF) <= 1)
  expect_true(min(exROC$TPF) >= 0)
  expect_true(min(exROC$FPF) >= 0)
  
  expect_warning(calculate_roc(T.ex, D.ex + 1))
  expect_error(calculate_roc(D.ex, T.ex))
  
})

test_that("ggroc returns a ggplot object", {
  
  expect_true(ggplot2::is.ggplot(ggroc(exROC)))
  expect_true(ggplot2::is.ggplot(multi_ggroc(list(exROC, exROC*.9))))
  
  expect_true(ggplot2::is.ggplot(plot_journal_roc(ggroc(exROC), exROC)))
  
})

test_that("Non 0/1 labels work, with warning", {
  

  expect_warning(calculate_roc(T.ex, D.str))
  expect_true(is.data.frame(calculate_roc(T.ex, D.str)))
  expect_true(is.list(calculate_multi_roc(testdata, c("T1", "T2"), "D.str")))
  
})

context("Calculate PRC")

set.seed(27599)

num_obs <- 1e3

D.ex <- rbinom(n = num_obs, 1, .5)
T.ex <- rnorm(n = num_obs, mean = D.ex, sd = .5)
D.str <- c("Healthy", "Ill")[D.ex + 1]


testdata <- data.frame(T1 = T.ex, T2 = T.ex + rnorm(n = num_obs), D = D.ex, D.str = D.str)
testprc <- ggplot_build(ggplot(testdata, aes(m = T1, d = D)) + geom_prc())$data[[1]]
ggprc_p <- ggplot(testdata, aes(m = T1, d = D)) + geom_prc()

layers <- sapply(ggprc_p$layers, function(l) class(l$geom)[1])

test_that("Calculate ROC returns a valid ROC curve", {
  
  expect_true(max(testprc$recall) <= 1)
  expect_true(max(testprc$precision) <= 1)
  expect_true(min(testprc$recall) >= 0)
  expect_true(min(testprc$precision) >= 0)
  
  expect_warning(print(ggplot(testdata, aes(m = T1, d = D + 1)) + geom_prc()))
  expect_error(print(ggplot(testdata, aes(m = D, d = T1)) + geom_prc()))
  
})

test_that("ggprc returns a ggplot object", {

  expect_true(ggplot2::is.ggplot(ggprc_p))
  expect_true("GeomPrc" %in% layers)

})

test_that("Non 0/1 labels work, with warning", {
  
  expect_warning(print(ggplot(testdata, aes(m = T1, d = D.str)) + stat_prc()))
  
})


test_that(desc = "plotROC can be thinned when there is too much data",
          code = {
            
            g <- ggplot(data = testdata, mapping = aes(m = T1, d = D)) + stat_prc(max.num.points = 20)
            
            expect_is(object = g,
                      class = 'ggplot')
            
            whole_plot_data <- ggplot_build(ggplot(testdata, aes(m = T1, d = D)) + stat_prc())$data[[1]]
            thinned_plot_data <- ggplot_build(ggplot(testdata, aes(m = T1, d = D)) + stat_prc(max.num.points = 20))$data[[1]]
            
            expect_equal(object = nrow(whole_plot_data), expected = num_obs + 1)
            expect_equal(object = nrow(thinned_plot_data), expected = 21)
          }
)

library('vkde')

test_that("dimensionality of ruleOfThumb output", {
  expect_equal(length(ruleOfThumb(numeric(10))),1)
  expect_equal(length(ruleOfThumb(array(0,dim = c(3,4)))), 3)
})

test_that("sample_dist_to_knn is correct", {
  expect_equal(sample_dist_to_knn(1:4,3), c(3,2,2,3))
  expect_equal(dist_to_knn(c(1,2),array(1:10,dim = c(2,5)),k = 1),0)
})

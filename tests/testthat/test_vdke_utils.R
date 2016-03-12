library('vkde')

test_that("sample_dist_to_knn is correct", {
  expect_equal(sample_dist_to_knn(1:4,3), c(3,2,2,3))
})

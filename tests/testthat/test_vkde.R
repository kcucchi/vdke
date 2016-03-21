library('vkde')

test_that("dimensionality of sgk output", {
  expect_equal(length(sgk(t=0,e=seq(from=-5,to=5,by=1),h=1,p=1)$y_pdf),11)
  expect_equal(length(sgk(t=c(0,1),e=array(1:10,dim=c(2,5)),h=1,p=2)$y_pdf),5)
})

test_that("output of sgk for univariate case", {
  expect_equal(sgk(t=0,e=0,h=1,p=1)$y_pdf,1/sqrt(2 * pi))
  expect_equal(sgk(t=0,e=0,h=2,p=1)$y_pdf,1/(2 * sqrt(2 * pi)))
})

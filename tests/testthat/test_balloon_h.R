library('vkde')

test_that("bw_balloon_LQ is correct", {
  expect_equal(length(bw_balloon_LQ(tdat = runif(sample(4:10,1),0,1),
                                    edat = runif(5,0,1),
                                    k = 3)),
               5)
  expect_equal(bw_balloon_LQ(tdat = 1:10,edat = c(2.8,5.1),k = 3), c(1.2,1.1))
  expect_equal(bw_balloon_LQ(tdat = array(1:20,dim=c(4,5)),edat = 1:4,k = 2),8)
})

test_that("kolonner_med_na() Antal na pr.", {
  data.frame("tal"=c(1,2,3), "bogstaver"= c(NA, "b", "c"))
  expect_equal(kolonner_med_na(data.frame("tal"=c(1,2,3), "bogstaver"= c(NA, "b", "c"))),
               setNames(c(0,1), c("tal", "bogstaver")))
})

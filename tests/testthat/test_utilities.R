context("Utilities")
library(rethinking)

#---------------
# tests for index
d <- data.frame(
  id1 = c("a","b",NA, "d","a"),
  id2 = c("b","a", "b","a","b"),
  id3 = factor(c("a","b","a",NA,"c"))
)
test_that("coerce_index gives the correct results",{
  expect_equal(coerce_index(d$id1),
               c(1L,2L,NA,3L,1L))
  expect_equal(coerce_index(d$id3),
               c(1L,2L,1L,NA,3L))
  expect_equal(coerce_index(d),
               list(id1 = c(1L, 2L, NA, 4L, 1L),
                    id2 = c(2L, 1L, 2L, 1L, 2L),
                    id3 = c(1L, 2L, 1L, NA, 3L)))
  # test the names
  expect_equal(coerce_index(d$id1, d$id2),
               list(d.id1 = c(1L, 2L, NA, 3L, 1L),
                    d.id2 = c(2L, 1L, 2L, 1L, 2L)))
  expect_equal(coerce_index(list(d$id1, d$id2) ),
               list(X1 = c(1L, 2L, NA, 3L, 1L),
                    X2 = c(2L, 1L, 2L, 1L, 2L)))
  # test the error
  expect_error(coerce_index(1:5, 1:4),
               "All factors need to be of the same length")
})
          

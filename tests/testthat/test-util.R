test_that("combine_factors", {
  # returns single factor input as given
  char_vec <- c(sample(LETTERS[1:3], 100, replace = 97), LETTERS[1:3])
  x <- factor(char_vec, levels = c("C", "A", "B"))
  y <- combine_factors(x)
  expect_equal(x, y)
  expect_equal(levels(x), levels(y))

  # converts input to factor
  x <- combine_factors(sample(LETTERS[1:3], 100, replace = TRUE))
  expect_true(is.factor(x))

  char_vec_lower <- tolower(char_vec)
  x <- combine_factors(char_vec, char_vec_lower, sep = ":")
  expect_equal(levels(x), c("A:a", "A:b", "A:c", "B:a", "B:b", "B:c", "C:a", "C:b", "C:c"))

  x <- combine_factors(char_vec, char_vec_lower, 1:2)
  expect_equal(levels(x), c(
    "A:a:1", "A:a:2", "A:b:1", "A:b:2", "A:c:1", "A:c:2", "B:a:1",
    "B:a:2", "B:b:1", "B:b:2", "B:c:1", "B:c:2", "C:a:1", "C:a:2",
    "C:b:1", "C:b:2", "C:c:1", "C:c:2"
  ))
})

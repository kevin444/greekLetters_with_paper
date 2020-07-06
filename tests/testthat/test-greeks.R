# This function tests, by comparing to a saved image of correctly loaded data

# source

# change dir to load fresh batch of data

# We expect that Greek letters be equal to unicode symbols

#generate tests
testthat::test_that('check if greeks is ok!', {
  testthat::expect_equivalent(greeks("sigma"), '\u03c3')

  testthat::expect_equivalent(greeks("sigma_2^3"), '\u03c3\u2082\u00B3')

  testthat::expect_equivalent(greeks("sigma^3_2"), '\u03c3\u2082\u00B3')
})

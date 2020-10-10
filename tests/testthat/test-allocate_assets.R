test_that("get_amount_bonds() returns a double", {
  expect_type(get_amount_bonds(1, 1, 1), "double")
})

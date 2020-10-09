test_that("allocate_assets() returns a list", {
  expect_type(allocate_assets(1, 1, 1), "list")
})

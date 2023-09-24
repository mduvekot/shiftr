test_that("result equals colour1 if amt = 0", {
  expect_equal(
    interpolate_cs(
      colour1 = "#ff7f23",
      colour2 = "#237f00",
      amt = 0,
      cs = "oklch") ,
    "#FF7F23" )
})

test_that("result equals colour2 if amt = 1", {
  expect_equal(
    interpolate_cs(
      colour1 = "#ff7f23",
      colour2 = "#237f00",
      amt = 1,
      cs = "oklch") ,
    "#237F00" )
})

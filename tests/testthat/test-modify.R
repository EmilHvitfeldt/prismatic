test_that("works", {
  expect_equal(modify_hcl("red", h = 160), color("#029071FF"))
  expect_equal(modify_hcl("red", h = h + 50), color("#9D7B07FF"))
  expect_equal(
    modify_hcl("red", h = h + 1:3),
    color(c("#FA1E02FF", "#F62B00FF", "#F23400FF"))
  )
  expect_equal(
    modify_hcl("red", c = c - 1:3),
    color(c("#FE0B0BFF", "#FE0B0BFF", "#FE0B0BFF"))
  )
  expect_equal(
    modify_hcl("red", l = l + 1:2),
    color(c("#FF1C1CFF", "#FF2A2AFF"))
  )
  expect_equal(
    modify_hcl(rainbow(4), l = 25),
    color(c("#7E0000FF", "#1E4401FF", "#064343FF", "#5000A4FF"))
  )
  expect_equal(
    modify_hcl(rainbow(3), h + h / 2, l = 70),
    color(c("#FF8A79FF", "#08BEBEFF", "#F79306FF"))
  )
})

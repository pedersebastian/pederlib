gg <- c(
  "tidyverse", "lubridate", "ggforce", "purrr",
  "colorspace", "particles", "ambient", "viridis",
  "ggtrace", "scico", "ggfx", "patchwork", "transformr", "farver", "scales", "ggdist", "ggpointdensity", "cowplot", "gghalves"
)



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("loads", {
  expect_equal(startup(), c("tidyverse", "lubridate"))
  # expect_equal(startup(6), gg)
})

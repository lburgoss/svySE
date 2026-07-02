test_that("svySE_cfg creates valid configuration", {
  cfg <- svySE_cfg(
    estimator = "prop",
    target = 1,
    valid_values = c(0, 1)
  )
  
  expect_s3_class(cfg, "svySE_cfg")
  expect_equal(cfg$estimator, "prop")
  expect_equal(cfg$target, 1)
})
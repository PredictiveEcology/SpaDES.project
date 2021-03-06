test_that("all exported functions have examples", {
  fns <- ls("package:SpaDES.project")
  omit <- which(fns == "cache") ## cache is deprecated, so omit it
  sapply(fns[-omit], function(x) {
    expect_warning(example(x, package = "SpaDES.project", character.only = TRUE,
                           echo = FALSE), NA)
  })
})

test_that("check all examples", {
  test_examples(path = "../../man")
})

test_that("test extractModName", {
  expect_true(extractModName("user/module@branch") == "module")
  expect_true(extractModName("user/module") == "module")
  expect_true(extractModName("user/repo@branch/module") == "module")
  expect_true(extractModName("user/repo/module") == "module")
  expect_true(extractModName("user/repo@branch/afolder/module") == "module")
  expect_error(extractModName("user/repo@branch/afolder/module/module.R"))
  expect_true(all(extractModName(c("user/module@branch",
                               "user/module",
                               "user/repo@branch/module")) %in% "module"))
})

## One module whose .R file does not parse (BiomeBGC_validationFluxTower had a missing comma in
## person()) stopped metadataInModules() for every module, and so the v-finding-other-modules vignette.

test_that("a module whose file does not parse is skipped with a message naming it", {
  mp <- withr::local_tempdir()
  mkVersionedModule(mp, "good", "1.0.0")
  dir.create(file.path(mp, "broken"))
  writeLines(c("defineModule(sim, list(",
               "  authors = c(",
               "    person(\"A\", \"B\", role = c(\"cre\", \"ctb\")",
               "    person(\"C\", \"D\")",
               "  ),",
               "  reqdPkgs = list(\"data.table\")",
               "))"), file.path(mp, "broken", "broken.R"))

  mess <- capture_messages(
    out <- metadataInModules(c("broken", "good"), "reqdPkgs", mp, verbose = 0))
  expect_true(any(grepl("Skipping module broken", mess)))
  expect_false(any(grepl("Skipping module good", mess)))
  expect_false("broken" %in% names(out))

  v <- NULL
  expect_message(v <- metadataInModules(c("broken", "good"), "version", mp), "Skipping module broken")
  expect_identical(as.character(v), "1.0.0")
  expect_message(packagesInModules(c("broken", "good"), mp), "Skipping module broken")
})

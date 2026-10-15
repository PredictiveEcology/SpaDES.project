## dataInventory() from R/dataInventory.R. Everything is on tiny temp modules and a
## hand-made simList ledger: no network, no simulation.

test_that("dataInventory: an input another module creates is a fallback; the rest are defaults", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "modA", inputs = c(diInput("x", "https://example.org/x.tif"),
                                       diInput("z", "https://example.org/z.tif")))
  diWriteModule(mp, "modB", outputs = diOutput("x"))

  inv <- dataInventory(c("modA", "modB"), list(modulePath = mp), verbose = 0)

  expect_s3_class(inv, "data.table")
  expect_true(all(c("module", "object", "url", "source", "event", "where", "role",
                    "observed", "cacheId") %in% names(inv)))
  a <- inv[inv$module == "modA", ]
  expect_identical(a$role[a$object == "x"], "fallback")
  expect_identical(a$role[a$object == "z"], "default")
  expect_identical(a$source[a$object == "x"], "metadata")
  expect_false(any(inv$observed))
  expect_false("modB" %in% inv$module)

  ## alone, modA's download is the default
  inv1 <- dataInventory("modA", list(modulePath = mp), verbose = 0)
  expect_identical(inv1$role[inv1$object == "x"], "default")
})

test_that("dataInventory: NA and empty sourceURL are skipped; a parent's children are included", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "parent", children = c("kid"))
  diWriteModule(mp, "kid", inputs = c(diInput("a", NA), diInput("b", ""),
                                      diInput("c", "https://example.org/c.zip")))

  inv <- dataInventory("parent", list(modulePath = mp), verbose = 0)

  expect_identical(inv$module, "kid")
  expect_identical(inv$url, "https://example.org/c.zip")
})

test_that("dataInventory: the code scan finds literal urls, extractURL and opaque helpers with file:line", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  code <- c(
    "doEvent.modC <- function(sim, eventTime, eventType) {",
    "  switch(eventType,",
    "    init = { sim <- Init(sim) })",
    "  invisible(sim)",
    "}",
    "Init <- function(sim) {",
    "  sim$lit <- prepInputs(targetFile = \"lit.zip\", url = \"https://example.org/lit.zip\",",
    "                        destinationPath = tempdir())",
    "  sim$meta <- prepInputs(url = extractURL(\"meta\", sim), destinationPath = tempdir())",
    "  sim$opaque <- LandR::prepInputs_Thing(to = sim$studyArea)",
    "  invisible(sim)",
    "}")
  f <- diWriteModule(mp, "modC", inputs = diInput("meta", "https://example.org/meta.tif"),
                     code = code)
  line <- function(pat) grep(pat, readLines(f), fixed = TRUE)[1]

  inv <- dataInventory("modC", list(modulePath = mp), verbose = 0)

  lit <- inv[!is.na(inv$url) & inv$url == "https://example.org/lit.zip", ]
  expect_identical(nrow(lit), 1L)
  expect_identical(lit$source, "code")
  expect_identical(lit$object, "lit")
  expect_identical(lit$event, "init")
  expect_identical(lit$where, paste0("modC.R:", line("sim$lit <-")))

  meta <- inv[!is.na(inv$url) & inv$url == "https://example.org/meta.tif", ]
  expect_identical(nrow(meta), 1L)                # one row per (url, module)
  expect_identical(meta$source, "metadata+code")
  expect_match(meta$where, paste0("modC.R:", line("sim$meta <-")), fixed = TRUE)

  op <- inv[is.na(inv$url), ]
  expect_identical(nrow(op), 1L)
  expect_identical(op$source, "code")
  expect_match(op$call, "prepInputs_Thing", fixed = TRUE)
  expect_identical(op$where, paste0("modC.R:", line("sim$opaque <-")))
})

test_that("dataInventory: a run's ledger marks rows observed and adds run-only rows", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("reproducible")
  mp <- withr::local_tempdir()
  diWriteModule(mp, "modA", inputs = c(diInput("x", "https://example.org/x.tif"),
                                       diInput("z", "https://example.org/z.tif")))
  rec <- function(module, url, cacheId, event = "init")
    list(time = "2026-10-14T10:00:00", fn = "prepInputs", url = url, cacheId = cacheId,
         module = module, event = event)
  sim <- diFakeSim(list(rec("modA", "https://example.org/x.tif", "abc123"),
                        rec("modA", "https://example.org/other.zip", "def456", "burn")))

  inv <- dataInventory("modA", list(modulePath = mp), sim = sim, verbose = 0)

  x <- inv[inv$url == "https://example.org/x.tif", ]
  expect_true(x$observed)
  expect_identical(x$cacheId, "abc123")
  expect_identical(x$source, "metadata+run")
  expect_false(inv$observed[inv$url == "https://example.org/z.tif"])
  o <- inv[inv$url == "https://example.org/other.zip", ]
  expect_identical(o$source, "run")
  expect_true(o$observed)
  expect_identical(o$event, "burn")
  expect_identical(o$module, "modA")
})

test_that("dataInventory: a module that cannot be found warns and is skipped", {
  skip_if_not_installed("SpaDES.core")
  mp <- withr::local_tempdir()
  expect_warning(inv <- dataInventory("nope", list(modulePath = mp), verbose = 0), "nope")
  expect_identical(nrow(inv), 0L)
})

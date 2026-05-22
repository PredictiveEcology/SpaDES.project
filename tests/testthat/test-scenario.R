# Tests for the generic scenario record system in R/scenario.R.
#
# Covers: as_scenario coercion, default value-only pathBuild/pathParse,
# trailing-NA round-trip, mid-NA documented lossiness,
# register_scenario_format override, schema-agnostic behaviour, and the
# positional pathBuild call style.

# Each test resets registry + cached fields so order-of-execution is irrelevant.
.reset <- function() {
  scenarioFieldsSet(NULL)
  register_scenario_format(build = NULL, parse = NULL,
                           withFieldLabel = NULL)  # passes NULL -> clears
}

queue_firesense <- function() {
  data.frame(
    dotELFind        = "6.3.1",
    dotsamplingRange = "2071:2100",
    dotGCM           = "CNRM-ESM2-1",
    dotSSP           = "370",
    dotrep           = 5L,
    started_at       = NA,
    status           = "PENDING",
    stringsAsFactors = FALSE
  )
}

test_that("default pathBuild emits values only, in queue column order", {
  .reset(); on.exit(.reset(), add = TRUE)
  s <- as_scenario(queue_firesense())
  expect_equal(as_path(s),    "outputs/6.3.1/2071-2100/CNRM-ESM2-1/370/5")
  expect_equal(as_tarname(s), "6.3.1_2071-2100_CNRM-ESM2-1_370_5.tar.gz")
})

test_that("path/tarname round-trip preserves field types", {
  .reset(); on.exit(.reset(), add = TRUE)
  s   <- as_scenario(queue_firesense())
  p   <- as_path(s)
  s2  <- as_scenario(p)
  expect_equal(unclass(s)$.rep,           unclass(s2)$.rep)
  expect_equal(unclass(s)$.samplingRange, unclass(s2)$.samplingRange)
  expect_equal(as_path(s2),               p)
  expect_equal(as_tarname(s2),            as_tarname(s))
})

test_that("trailing-NA fields are dropped on build and recovered on parse", {
  .reset(); on.exit(.reset(), add = TRUE)
  q <- data.frame(dotELFind = "X", dotsamplingRange = "1990:2000",
                  dotGCM = "G", dotSSP = NA, dotrep = NA,
                  started_at = NA, stringsAsFactors = FALSE)
  s  <- as_scenario(q)
  p  <- as_path(s)
  expect_equal(p, "outputs/X/1990-2000/G")

  s2 <- as_scenario(p)
  expect_true(is.na(unclass(s2)$.SSP))
  expect_true(is.na(unclass(s2)$.rep))
  expect_equal(unclass(s2)$.GCM, "G")
})

test_that("mid-list NA round-trip is positionally lossy (documented)", {
  .reset(); on.exit(.reset(), add = TRUE)
  q <- data.frame(dotELFind = "X", dotsamplingRange = "1990:2000",
                  dotGCM = NA, dotSSP = "126", dotrep = 1L,
                  started_at = NA, stringsAsFactors = FALSE)
  s  <- as_scenario(q)
  p  <- as_path(s)
  expect_equal(p, "outputs/X/1990-2000/126/1")

  # Without label markers the parser cannot know .GCM was the dropped one;
  # values shift left, .GCM gets "126" (decoded as integer), .SSP gets 1,
  # .rep is NA.
  s2 <- as_scenario(p)
  expect_equal(unclass(s2)$.GCM, 126L)
  expect_equal(unclass(s2)$.SSP, 1L)
  expect_true(is.na(unclass(s2)$.rep))
})

test_that("register_scenario_format(build=...) wins over the default", {
  .reset(); on.exit(.reset(), add = TRUE)
  s <- as_scenario(queue_firesense())

  fsBuild <- function(.ELFind, .samplingRange, .GCM, .SSP, .rep,
                     pre = "outputs") {
    sr <- if (is.numeric(.samplingRange)) .samplingRange
          else                            eval(parse(text = .samplingRange))
    file.path(pre, .ELFind,
              paste(range(sr), collapse = "-"),
              paste0(.GCM, ifelse(is.na(.SSP), "", paste0("_ssp", .SSP))),
              paste0("rep", .rep))
  }
  register_scenario_format(build = fsBuild)

  expect_equal(as_path(s), "outputs/6.3.1/2071-2100/CNRM-ESM2-1_ssp370/rep5")
})

test_that("non-FireSense schema works without any project hooks", {
  .reset(); on.exit(.reset(), add = TRUE)
  q <- data.frame(experiment  = "X1",
                  year_window = "2030:2040",
                  model       = "M",
                  iter        = 2L,
                  started_at  = NA,
                  stringsAsFactors = FALSE)
  s <- as_scenario(q)
  expect_setequal(scenarioFields(),
                  c("experiment", "year_window", "model", "iter"))
  p <- as_path(s)
  expect_equal(p, "outputs/X1/2030-2040/M/2")
  expect_equal(as_path(as_scenario(p)), p)
})

test_that("pathBuild accepts list, named-args, and positional calls", {
  .reset(); on.exit(.reset(), add = TRUE)
  scenarioFieldsSet(c(".A", ".B"))

  # As a single named list
  expect_equal(pathBuild(list(.A = "foo", .B = 1L)), "outputs/foo/1")
  # As named args
  expect_equal(pathBuild(.A = "foo", .B = 1L),       "outputs/foo/1")
  # As positional args (uses cached fields)
  expect_equal(pathBuild("foo", 1L),                 "outputs/foo/1")
})

test_that("positional pathBuild infers fields from bare-symbol args", {
  .reset(); on.exit(.reset(), add = TRUE)
  .ELFind        <- "6.3.1"
  .samplingRange <- 2071:2100
  .GCM           <- "CNRM-ESM2-1"
  .SSP           <- "370"
  .rep           <- 5L

  # No cached fields, no named args -- inferred from the symbol names.
  expect_equal(pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep),
               "outputs/6.3.1/2071-2100/CNRM-ESM2-1/370/5")
  # And the inferred set gets cached.
  expect_equal(scenarioFields(),
               c(".ELFind", ".samplingRange", ".GCM", ".SSP", ".rep"))
})

test_that("positional pathBuild with literals still requires cached fields", {
  .reset(); on.exit(.reset(), add = TRUE)
  expect_error(pathBuild("foo", 1L),
               "positional call needs cached fields")
})

test_that("as_scenario.character vectorises", {
  .reset(); on.exit(.reset(), add = TRUE)
  scenarioFieldsSet(c(".A", ".B"))
  ss <- as_scenario(c("outputs/x/1", "outputs/y/2"))
  expect_length(ss, 2L)
  expect_equal(unclass(ss[[1L]])$.A, "x")
  expect_equal(unclass(ss[[2L]])$.B, 2L)
})

test_that("withFieldLabel prefixes selected fields with their label", {
  .reset(); on.exit(.reset(), add = TRUE)
  s <- as_scenario(queue_firesense())
  p <- as_path(s, withFieldLabel = c(".rep", ".SSP"))
  expect_equal(p, "outputs/6.3.1/2071-2100/CNRM-ESM2-1/.SSP370/.rep5")
  t <- as_tarname(s, withFieldLabel = c(".rep", ".SSP"))
  expect_equal(t, "6.3.1_2071-2100_CNRM-ESM2-1_.SSP370_.rep5.tar.gz")
})

test_that("withFieldLabel registry slot persists across calls", {
  .reset(); on.exit(.reset(), add = TRUE)
  s <- as_scenario(queue_firesense())
  register_scenario_format(withFieldLabel = c(".rep", ".SSP"))
  expect_equal(as_path(s),
               "outputs/6.3.1/2071-2100/CNRM-ESM2-1/.SSP370/.rep5")
})

test_that("withFieldLabel as named vector uses mapped labels (.rep -> rep)", {
  .reset(); on.exit(.reset(), add = TRUE)
  s <- as_scenario(queue_firesense())
  p <- as_path(s, withFieldLabel = c(.rep = "rep", .SSP = "_ssp"))
  expect_equal(p, "outputs/6.3.1/2071-2100/CNRM-ESM2-1/_ssp370/rep5")

  # And it round-trips
  s2 <- as_scenario(p, withFieldLabel = c(.rep = "rep", .SSP = "_ssp"))
  expect_equal(unclass(s2)$.rep, 5L)
  expect_equal(unclass(s2)$.SSP, 370L)
  expect_equal(unclass(s2)$.GCM, "CNRM-ESM2-1")
})

test_that("named-form withFieldLabel via register_scenario_format", {
  .reset(); on.exit(.reset(), add = TRUE)
  s <- as_scenario(queue_firesense())
  register_scenario_format(withFieldLabel = c(.rep = "rep", .SSP = "_ssp"))
  expect_equal(as_path(s),
               "outputs/6.3.1/2071-2100/CNRM-ESM2-1/_ssp370/rep5")
  expect_equal(as_tarname(s),
               "6.3.1_2071-2100_CNRM-ESM2-1__ssp370_rep5.tar.gz")
})

test_that("withFieldLabel makes mid-list NA round-trip clean", {
  .reset(); on.exit(.reset(), add = TRUE)
  q <- data.frame(dotELFind = "X", dotsamplingRange = "1990:2000",
                  dotGCM = NA, dotSSP = "126", dotrep = 1L,
                  started_at = NA, stringsAsFactors = FALSE)
  s <- as_scenario(q)
  register_scenario_format(withFieldLabel = c(".SSP", ".rep"))

  p <- as_path(s)
  expect_equal(p, "outputs/X/1990-2000/.SSP126/.rep1")

  s2 <- as_scenario(p)
  expect_true(is.na(unclass(s2)$.GCM))
  expect_equal(unclass(s2)$.SSP, 126L)
  expect_equal(unclass(s2)$.rep, 1L)
  expect_equal(unclass(s2)$.ELFind, "X")
})

test_that("as_scenario(dribble) parses the `name` column", {
  .reset(); on.exit(.reset(), add = TRUE)
  scenarioFieldsSet(c(".ELFind", ".samplingRange", ".GCM", ".SSP", ".rep"))
  d <- data.frame(name = "6.3.1_2071-2100_CNRM-ESM2-1_370_5.tar.gz",
                  id   = "abc",
                  stringsAsFactors = FALSE)
  s <- as_scenario(d)
  expect_equal(as_path(s),
               "outputs/6.3.1/2071-2100/CNRM-ESM2-1/370/5")
})

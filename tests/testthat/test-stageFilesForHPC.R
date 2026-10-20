## stageFilesForHPC(): per-folder rsync list files from a filesUsedTrace()-style
## data.frame, with the sidecars that sit beside each used file. Only the real-copy
## test needs rsync; the others check the list file and the command text.

# A folder with one used file, its three sidecars, and two files that must not be staged.
mkFolder <- function(root) {
  dir.create(file.path(root, "sub"), recursive = TRUE, showWarnings = FALSE)
  for (f in c("sub/x.tif", "sub/x.tif.aux.xml", "sub/CHECKSUMS.txt",
              "sub/.x.tif_drive.google.com_abc.hash", "sub/.x.tif_other.hash",
              "sub/.y.tif_drive.google.com_abc.hash", "sub/y.tif", "CHECKSUMS.txt"))
    writeLines(f, file.path(root, f))
  root
}

sidecarsOf <- c("sub/x.tif", "sub/CHECKSUMS.txt", "sub/x.tif.aux.xml",
                "sub/.x.tif_drive.google.com_abc.hash", "sub/.x.tif_other.hash")

test_that("stageFilesForHPC lists the used file and its sidecars, and builds a dry-run rsync", {
  folder <- mkFolder(withr::local_tempdir())
  files <- data.frame(path = file.path(folder, "sub/x.tif"), folder = folder)
  dest <- c("host:/remote/inputs")
  names(dest) <- folder

  expect_message(res <- stageFilesForHPC(files, dest), "rsync -aH --dry-run")
  expect_invisible(stageFilesForHPC(files, dest) |> suppressMessages())

  expect_length(res, 1L)
  expect_identical(res[[1]]$folder, folder)
  expect_setequal(readLines(res[[1]]$listFile), sidecarsOf)
  expect_setequal(res[[1]]$files, sidecarsOf)
  expect_match(res[[1]]$command, "--dry-run", fixed = TRUE)
  expect_match(res[[1]]$command, paste0("--files-from=", shQuote(res[[1]]$listFile)), fixed = TRUE)
  expect_match(res[[1]]$command, paste0(" ", shQuote(paste0(folder, "/")), " ", shQuote("host:/remote/inputs/")), fixed = TRUE)
})

test_that("stageFilesForHPC groups by folder and needs a destination for each", {
  f1 <- mkFolder(withr::local_tempdir())
  f2 <- withr::local_tempdir()
  writeLines("z", file.path(f2, "z.csv"))
  files <- data.frame(path = c(file.path(f1, "sub/x.tif"), file.path(f1, "sub/y.tif"), file.path(f2, "z.csv")),
                      folder = c(f1, f1, f2))

  dest <- c("host:/a")
  names(dest) <- f1
  expect_error(suppressMessages(stageFilesForHPC(files, dest)), f2, fixed = TRUE)

  dest <- c("host:/a", "host:/b")
  names(dest) <- c(f1, f2)
  res <- suppressMessages(stageFilesForHPC(files, dest))
  expect_length(res, 2L)
  expect_setequal(readLines(res[[1]]$listFile),
                  c(sidecarsOf, "sub/y.tif", "sub/.y.tif_drive.google.com_abc.hash"))
  expect_identical(readLines(res[[2]]$listFile), "z.csv")
  expect_match(res[[2]]$command, shQuote("host:/b/"), fixed = TRUE)
})

test_that("stageFilesForHPC copies with rsync when dryRun = FALSE", {
  skip_if(!nzchar(Sys.which("rsync")), "rsync not available")
  skip_on_os("windows") # rsync reads the colon in "C:/..." as a remote host
  folder <- mkFolder(withr::local_tempdir())
  dest <- file.path(withr::local_tempdir(), "staged")
  names(dest) <- folder
  files <- data.frame(path = file.path(folder, "sub/x.tif"), folder = folder)

  res <- suppressMessages(stageFilesForHPC(files, dest, dryRun = FALSE))
  expect_false(grepl("--dry-run", res[[1]]$command, fixed = TRUE))
  expect_setequal(list.files(dest, recursive = TRUE, all.files = TRUE, no.. = TRUE), sidecarsOf)
  expect_false(file.exists(file.path(dest, "sub/y.tif")))
})

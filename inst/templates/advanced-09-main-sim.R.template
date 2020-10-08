## run the simulation

data.table::setDTthreads(useParallel)

tryCatch({
  mySimOut <- Cache(simInitAndSpades, times = times3, #cl = cl,
                    params = parameters3,
                    modules = modules3,
                    outputs = outputs3,
                    objects = objects3,
                    paths = paths3,
                    loadOrder = unlist(modules3),
                    debug = list(file = list(file = file.path(Paths$outputPath, "sim.log"),
                                             append = TRUE), debug = 1),
                    useCloud = FALSE,
                    cloudFolderID = cloudCacheFolderID,
                    omitArgs = c("debug", "paths", ".plotInitialTime"),
                    .plotInitialTime = .plotInitialTime)
}, error = function(e) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::text_slackr(
      paste0("ERROR in simulation `", runName, "` on host `", Sys.info()[["nodename"]], "`.\n",
             "```\n", e$message, "\n```"),
      channel = config::get("slackchannel"), preformatted = FALSE
    )
    stop(e$message)
  }
})

cat(capture.output(warnings()), file = file.path(Paths$outputPath, "warnings.txt"))

fsim <- simFile("mySimOut", Paths$outputPath, SpaDES.core::end(mySimOut), "qs")
message("Saving simulation to: ", fsim)
saveSimList(sim = mySimOut, filename = fsim)

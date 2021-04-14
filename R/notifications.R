#' Send slack notification about completed simulation
#'
#' Requires the suggested \pkg{slackr} package to be installed, and configured.
#' (You need a webhook setup, and a config file at `~/.slackr`. See their package documentation.)
#'
#' @param runName character. The simulation run name.
#' @param channel character. The channel name in which to post the message.
#'
#' @export
notify_slack <- function(runName, channel) {
  if (requireNamespace("slackr") & file.exists("~/.slackr")) {
    slackr::slackr_setup()
    slackr::slackr_msg(
      paste0("Simulation `", runName, "` completed on host `", Sys.info()[["nodename"]], "`",
             if (nzchar(Sys.getenv("STY"))) paste0(" (screen `", Sys.getenv("STY"), "`)"), "."),
      channel = channel, preformatted = FALSE
    )
  }
}

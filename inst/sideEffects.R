print(mode)
quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

httr::set_config(httr::config(http_version = 0))

if (user("achubaty")) {
  googledrive::drive_auth(email = "alex.chubaty@gmail.com")
} else if (user("emcintir")) {
  googledrive::drive_auth(email = "eliotmcintire@gmail.com")
} else {
  googledrive::drive_auth(use_oob = quickPlot::isRstudioServer())
}

message(crayon::silver("Authenticating as: "), crayon::green(googledrive::drive_user()$emailAddress))

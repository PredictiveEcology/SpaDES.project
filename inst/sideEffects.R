# This is an example 'sideEffects' file that can be called from `setupProject`.
# The objective of this file is to run anything that for which side effects are
# required, such as cloud authentication. No objects will be returned to the user;
# this should be strictly limited to side effects. Although nothing will be returned,
# there is no other checking, so a user could include many types of sideEffects.

# This file will have access to the arguments passed into `setupParams` and `setupProject`,
# such as `paths`, `times`, or any other named argument passed to the `...`.
# Example -- local variables that can be used after in this file; they will not persist

# If R packages are needed, it is likely wise to prefix the function with the package name;
# any package that is needed can be added to the `require` argument in `setupProject`.

httr::set_config(httr::config(http_version = 0))

if (requireNamespace("googledrive", quietly = TRUE)) {
  if (!googledrive::drive_has_token())
    if (user("achubaty")) {
      googledrive::drive_auth(email = "alex.chubaty@gmail.com")
    } else if (user("emcintir")) {
      googledrive::drive_auth(email = "eliotmcintire@gmail.com", cache = "~/.secret")
    } else {
      googledrive::drive_auth(use_oob = quickPlot::isRstudioServer())
    }

  message(crayon::silver("Authenticating as: "), crayon::green(googledrive::drive_user()$emailAddress))

}

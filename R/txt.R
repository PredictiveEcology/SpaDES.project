.txtUpdateProfileIsTRUE <- paste0("updateRprofile is TRUE, but the projectPath is the tempdir(), which means ",
               "the .Rprofile won't be read upon restart. ",
               "Change the paths$projectPath to a non-temporary path")

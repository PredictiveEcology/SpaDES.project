% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setupProject.R
\name{setupPaths}
\alias{setupPaths}
\alias{setupFunctions}
\alias{setupSideEffects}
\alias{setupOptions}
\alias{setupModules}
\alias{setupPackages}
\alias{setupParams}
\alias{setupGitIgnore}
\alias{setupStudyArea}
\title{Individual \verb{setup*} functions that are contained within \code{setupProject}}
\usage{
setupPaths(
  name,
  paths,
  inProject,
  standAlone = TRUE,
  libPaths = NULL,
  updateRprofile = getOption("SpaDES.project.updateRprofile", TRUE),
  Restart = getOption("SpaDES.project.Restart", FALSE),
  overwrite = FALSE,
  envir = parent.frame(),
  useGit = getOption("SpaDES.project.useGit", FALSE),
  verbose = getOption("Require.verbose", 1L),
  dots,
  defaultDots,
  ...
)

setupFunctions(
  functions,
  name,
  sideEffects,
  paths,
  overwrite = FALSE,
  envir = parent.frame(),
  verbose = getOption("Require.verbose", 1L),
  dots,
  defaultDots,
  ...
)

setupSideEffects(
  name,
  sideEffects,
  paths,
  times,
  overwrite = FALSE,
  envir = parent.frame(),
  verbose = getOption("Require.verbose", 1L),
  dots,
  defaultDots,
  ...
)

setupOptions(
  name,
  options,
  paths,
  times,
  overwrite = FALSE,
  envir = parent.frame(),
  verbose = getOption("Require.verbose", 1L),
  dots,
  defaultDots,
  useGit = getOption("SpaDES.project.useGit", FALSE),
  updateRprofile = getOption("SpaDES.project.updateRprofile", TRUE),
  ...
)

setupModules(
  name,
  paths,
  modules,
  inProject,
  useGit = getOption("SpaDES.project.useGit", FALSE),
  overwrite = FALSE,
  envir = parent.frame(),
  gitUserName,
  verbose = getOption("Require.verbose", 1L),
  dots,
  defaultDots,
  updateRprofile = getOption("SpaDES.project.updateRprofile", TRUE),
  ...
)

setupPackages(
  packages,
  modulePackages = list(),
  require = list(),
  paths,
  libPaths,
  setLinuxBinaryRepo = TRUE,
  standAlone,
  envir = parent.frame(),
  verbose = getOption("Require.verbose"),
  dots,
  defaultDots,
  ...
)

setupParams(
  name,
  params,
  paths,
  modules,
  times,
  options,
  overwrite = FALSE,
  envir = parent.frame(),
  verbose = getOption("Require.verbose", 1L),
  dots,
  defaultDots,
  ...
)

setupGitIgnore(
  paths,
  gitignore = getOption("SpaDES.project.gitignore", TRUE),
  verbose
)

setupStudyArea(
  studyArea,
  paths,
  envir,
  verbose = getOption("Require.verbose", 1L)
)
}
\arguments{
\item{name}{Optional. If supplied, the name of the project. If not supplied, an
attempt will be made to extract the name from the \code{paths[["projectPath"]]}.
If this is a GitHub project, then it should indicate the full Github
repository and branch name, e.g., \code{"PredictiveEcology/WBI_forecasts@ChubatyPubNum12"}}

\item{paths}{a list with named elements, specifically, \code{modulePath}, \code{projectPath},
\code{packagePath} and all others that are in \code{SpaDES.core::setPaths()}
(i.e., \code{inputPath}, \code{outputPath}, \code{scratchPath}, \code{cachePath}, \code{rasterTmpDir}).
Each of these has a sensible default, which will be overridden but any user
supplied values.
See \link{setup}.}

\item{inProject}{A logical. If \code{TRUE}, then the current directory is
inside the \code{paths[["projectPath"]]}.}

\item{standAlone}{A logical. Passed to \code{Require::standAlone}. This keeps all
packages installed in a project-level library, if \code{TRUE}. Default is \code{TRUE}.}

\item{libPaths}{Deprecated. Use \code{paths = list(packagePath = ...)}.}

\item{updateRprofile}{Logical. Should the \code{paths$packagePath} be set in the \code{.Rprofile}
file for this project. Note: if \code{paths$packagePath} is within the \code{tempdir()},
then there will be a warning, indicating this won't persist. If the user is
using \code{Rstudio} and the \code{paths$projectPath} is not the root of the current
Rstudio project, then a warning will be given, indicating the .Rprofile may not
be read upon restart.}

\item{Restart}{Logical or character. If either \code{TRUE} or a character,
and if the \code{projectPath} is not the current path, and the session is in
RStudio and interactive, it will try to restart Rstudio in the projectPath with
a new Rstudio project. If character, it should represent the filename
of the script that contains the \code{setupProject} call that should be copied to
the new folder and opened. If \code{TRUE}, it will use the active file as the one
that should be copied to the new projectPath and opened in the Rstudio project.
If successful, this will create an RStudio Project file (and .Rproj.user
folder), restart with a new Rstudio session with that new project and with a root
path (i.e. working directory) set to \code{projectPath}. Default is \code{FALSE}, and no
RStudio Project is created.}

\item{overwrite}{Logical vector or character vector, however, only \code{getModule} will respond
to a vector of values. If length-one \code{TRUE}, then all files that were previously downloaded
will be overwritten throughout the sequence of \code{setupProject}. If a vector of
logical or character, these will be passed to \code{getModule}: only the named
modules will be overwritten or the logical vector of the modules.
NOTE: if a vector, no other file specified anywhere in \code{setupProject} will be
overwritten except a module that/those names, because
only \code{setupModules} is currently responsive to a vector. To have fine grained control,
a user can just manually delete a file, then rerun.}

\item{envir}{An environment within which to look for objects. If called alone,
the function should use its own internal environment. If called from another
function, e.g., \code{setupProject}, then the \code{envir} should be the internal
transient environment of that function.}

\item{useGit}{(if not FALSE, then experimental still). There are two levels at which a project
can use GitHub, either the \code{projectPath} and/or the \code{modules}. Any given
project can have one or the other, or both of these under git control. If "both",
then this function will assume that git submodules will be used for the \code{modules}.
A logical or \code{"sub"} for \emph{submodule}. If \code{"sub"}, then this function
will attempt to clone the identified \code{modules} \emph{as git submodules}. This will only
work if the \code{projectPath} is a git repository. If the project is already a git
repository because the user has set that up externally to this function call, then
this function will add the \code{modules} as git submodules. If it is not already,
it will use \verb{git clone} for each module. After git clone or submodule add are run,
it will run \verb{git checkout} for the named branch and then \verb{git pull}
to get and change branch for each module, according to its specification in
\code{modules}. If \code{FALSE}, this function  will download modules with \code{getModules}.
NOTE: \emph{CREATING} A
GIT REPOSITORY AT THE PROJECT LEVEL AND SETTING MODULES AS GIT SUBMODULES IS
EXPERIMENTAL. IT IS FINE IF THE PROJECT HAS BEEN MANUALLY SET UP TO BE
A GIT REPOSITORY WITH SUBMODULES: THIS FUNCTION WILL ONLY EVALUTE PATHS. This can
be set with the \code{option(SpaDES.project.useGit = xxx)}.}

\item{verbose}{Numeric or logical indicating how verbose should the function
be. If -1 or -2, then as little verbosity as possible. If 0 or FALSE,
then minimal outputs; if \code{1} or TRUE, more outputs; \code{2} even more. NOTE: in
\code{Require} function, when \code{verbose >= 2}, also returns details as if
\code{returnDetails = TRUE} (for backwards compatibility).}

\item{dots}{Any other named objects passed as a list a user might want for other elements.}

\item{defaultDots}{A named list of any arbitrary R objects.
These can be supplied to give default values to objects that
are otherwise passed in with the \code{...}, i.e., not specifically named for these
\verb{setup*} functions. If named objects are supplied as top-level arguments, then
the \code{defaultDots} will be overridden. This can be particularly useful if the
arguments passed to \code{...} do not always exist, but rely on external e.g., batch
processing to optionally fill them. See examples.}

\item{...}{further named arguments that acts like \code{objects}, but a different
way to specify them. These can be anything. The general use case
is to create the \code{objects} that are would be passed to
\code{SpaDES.core::simInit}, or \code{SpaDES.core::simInitAndSpades},
(e.g. \code{studyAreaName} or \code{objects}) or additional objects to be passed to the simulation
(in older versions of \code{SpaDES.core}, these were passed as a named list
to the \code{objects} argument). \strong{Order matters}. These are sequentially evaluated,
and also any arguments that are specified before the named arguments
e.g., \code{name}, \code{paths}, will be evaluated prior to any of the named arguments,
i.e., "at the start" of the \code{setupProject}.
If placed after the first named argument, then they will be evaluated at the
end of the \code{setupProject}, so can access all the packages, objects, etc.}

\item{functions}{A set of function definitions to be used within \code{setupProject}.
These will be returned as a list element. If function definitions require non-base
packages, prefix the function call with the package e.g., \code{terra::rast}. When
using \code{setupProject}, the \code{functions} argument is evaluated after \code{paths}, so
it cannot be used to define functions that help specify \code{paths}.}

\item{sideEffects}{Optional. This can be an expression or one or more file names or
a code chunk surrounded by \code{{...}}.
If a non-text file name is specified (e.g., \emph{not .txt or .R} currently),
these files will simply be downloaded, using their relative path as specified
in the github notation. They will be downloaded or accessed locally at that
relative path.
If these file names represent scripts (*.txt or .R), this/these will be parsed and evaluated,
but nothing is returned (i.e., any assigned objects are not returned). This is intended
to be used for operations like cloud authentication or configuration functions
that are run for their side effects only.}

\item{times}{Optional. This will be returned if supplied; if supplied, the values
can be used in e.g., \code{params}, e.g., \code{params = list(mod = list(startTime = times$start))}.
See help for \code{SpaDES.core::simInit}.}

\item{options}{Optional. Either a named list to be passed to \code{options}
or a character vector indicating one or more file(s) to source,
in the order provided. These will be parsed locally (not
the \code{.GlobalEnv}), so they will not create globally accessible objects. NOTE:
\code{options} is run 2x within \code{setupProject}, once before \code{setupPaths} and once
after \code{setupPackages}. This occurs because many packages use options for their
behaviour (need them set before e.g., \code{Require::require} is run; but many packages
also change \code{options} at startup. See details.
See \link{setup}.}

\item{modules}{a character string of modules to pass to \code{getModule}. These
should be one of: simple name (e.g., \code{fireSense}) which will be searched for locally
in the \code{paths[["modulePath"]]}; or a GitHub repo with branch (\code{GitHubAccount/Repo@branch} e.g.,
\code{"PredictiveEcology/Biomass_core@development"}); or a character vector that identifies
one or more module folders (local or GitHub) (not the module .R script).
If the entire project is a git repository,
then it will not try to re-get these modules; instead it will rely on the user
managing their git status outside of this function.
See \link{setup}.}

\item{gitUserName}{The GitHub account name. Used with git clone git@github.com:\emph{gitHuserName}/name}

\item{packages}{Optional. A vector of packages that must exist in the \code{libPaths}.
This will be passed to \code{Require::Install}, i.e., these will be installed, but
not attached to the search path. See also the \code{require} argument. To force skip
of package installation (without assessing modules), set \code{packages = NULL}}

\item{modulePackages}{A named list, where names are the module names, and the elements
of the list are packages in a form that \code{Require::Require} accepts.}

\item{require}{Optional. A character vector of packages to install \emph{and} attach
(with \code{Require::Require}). These will be installed and attached at the start
of \code{setupProject} so that a user can use these during \code{setupProject}.
See \link{setup}}

\item{setLinuxBinaryRepo}{Logical. Should the binary RStudio Package Manager be used
on Linux (ignored if Windows)}

\item{params}{Optional. Similar to \code{options}, however, this named list will be
returned, i.e., there are no side effects.
See \link{setup}.}

\item{gitignore}{Logical. Only has an effect if the \code{paths$projectPath}
is a git repositories without submodules. This case is ambiguous what a user
wants. If \code{TRUE}, the default, then \code{paths$modulePath} will be added to
the \code{.gitignore} file. Can be controled with \code{options(SpadES.project.gitignore = ...)}.}

\item{studyArea}{Optional. If a list, it will be passed to
\code{geodata::gadm}. To specify a country other than the default \code{"CAN"},
the list must have a named element, \code{"country"}. All other named elements
will be passed to \code{gadm}. 2 additional named elements can be passed for
convenience, \code{subregion = "..."}, which will be grepped with the column
\code{NAME_1}, and \code{epsg = "..."}, so a user can pass an \code{epsg.io} code to
reproject the \code{studyArea}. See examples.}
}
\value{
\code{setupPaths} returns a list of paths that are created. \code{projectPath} will be
assumed to be the base of other non-temporary and non-R-library paths. This means
that all paths that are directly used by \code{simInit} are assumed to be relative
to the \code{projectPath}. If a user chooses to specify absolute paths, then they will
be returned as is. It is also called for its
side effect which is to call \code{setPaths}, with each of these paths as an argument.
See table for details. If a user supplies extra paths not useable by \code{SpaDES.core::simInit},
these will added as an attribute ("extraPaths") to the \code{paths} element
in the returned object. These will still exist directly in the returned list
if a user uses \code{setupPaths} directly, but these will not be returned with
\code{setupProject} because \code{setupProject} is intended to be used with \code{SpaDES.core::simInit}.
In addition, three paths will be added to this same attribute automatically:
\code{projectPath}, \code{packagePath}, and \code{.prevLibPaths} which is the previous value for
\code{.libPaths()} before changing to \code{packagePath}.

\code{setupFunctions} returns NULL. All functions will be placed in \code{envir}.

\code{setupSideEffects} is run for its side effects (e.g., web authentication, custom package
options that cannot use \code{base::options}), with deliberately nothing returned to user.
This, like other parts of this function, attempts to prevent unwanted outcomes
that occur when a user uses e.g., \code{source} without being very careful about
what and where the objects are sourced to.

\code{setupOptions} is run for its side effects, namely, changes to the \code{options()}. The
list of modified options will be added as an attribute (\code{attr(out, "projectOptions")}),
e.g., so they can be "unset" by user later.

\code{setupModules} is run for its side effects, i.e., downloads modules and puts them
into the \code{paths[["modulePath"]]}. It will return a named list, where the names are the
full module names and the list elemen.ts are the R packages that the module
depends on (\code{reqsPkgs})

\code{setupPackages} is run for its side effects, i.e., installing packages to
\code{paths[["packagePath"]]}.

\code{setupParams} prepares a named list of named lists, suitable to be passed to
the \code{params} argument of \code{simInit}.

\code{setupGitIgnore} is run for its side effects, i.e., adding either \code{paths$packagePath}
and/or \code{paths$modulePath} to the
\code{.gitignore} file. It will check whether \code{packagePath} is located inside the
\code{paths$projectPath} and will add this folder to the \code{.gitignore} if \code{TRUE}.
If the project is a git repository with git submodules, then it will add nothing else.
If the project is a git repository without git submodules, then the \code{paths$modulePath}
will be added to the \code{.gitignore} file. It is assumed that these modules are
used in a \verb{read only} manner.

\code{setupStudyArea} will return an \code{sf} class object coming from \code{geodata::gadm},
with subregion specification as described in the \code{studyArea} argument.fsu
}
\description{
These functions will allow more user control, though in most circumstances,
it should be unnecessary to call them directly.
}
\details{
\code{setPaths} will fill in any paths that are not explicitly supplied by the
user as a named list. These paths that can be set are:
\code{projectPath}, \code{packagePath}, \code{cachePath}, \code{inputPath},
\code{modulePath}, \code{outputPath}, \code{rasterPath}, \code{scratchPath}, \code{terraPath}.
These are grouped thematically into three groups of paths:
\code{projectPath} and \code{packagePath} affect the project, regardless
of whether a user uses SpaDES modules. \code{cachePath}, \code{inputPath}, \code{outputPath} and
\code{modulePath} are all used by SpaDES within module contexts. \code{scratchPath},
\code{rasterPath} and \code{terraPath} are all "temporary" or "scratch" directories.

\code{setupFunctions} will source the functions supplied, with a parent environment being
the internal temporary environment of the \code{setupProject}, i.e., they will have
access to all the objects in the call.

Most arguments in the family of \verb{setup*} functions are run \emph{sequentially}, even within
the argument. Since most arguments take lists, the user can set values at a first
value of a list, then use it in calculation of the 2nd value and so on. See
examples. This "sequential" evaluation occurs in the \code{...}, \code{setupSideEffects}, \code{setupOptions},
\code{setupParams} (this does not work for \code{setupPaths}) can handle sequentially
specified values, meaning a user can
first create a list of default options, then a list of user-desired options that
may or may not replace individual values. This can create hierarchies, \emph{based on
order}.

\code{setupOptions} can handle sequentially specified values, meaning a user can
first create a list of default options, then a list of user-desired options that
may or may not replace individual values. Thus final values will be based on the
order that they are provided.

\code{setupModules} will download all modules do not yet exist locally. The current
test for "exists locally" is simply whether the directory exists. If a user
wants to update the module, \code{overwrite = TRUE} must be set, or else the user can
remove the folder manually.

\code{setupPackages} will read the modules' metadata \code{reqdPkgs} element. It will combine
these with any packages passed manually by the user to \code{packages}, and pass all
these packages to \code{Require::Install(...)}.

\code{setupGitIgnore} will add.

\code{setupStudyArea} only uses \code{inputPath} within its \code{paths} argument, which will
be passed to \code{path} argument of \code{gadm}. User can pass any named list element
that matches the columns in the \code{sf} object, including e.g., \code{NAME_1} and, if \code{level = 2},
is specified, then \code{NAME_2}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{setupStudyArea(list(NAME_1 = "Alberta", "NAME_2" = "Division No. 17", level = 2))
}\if{html}{\out{</div>}}
}
\section{Paths}{

\tabular{lll}{
\strong{Path}     \tab \strong{Default if not supplied by user} \tab Effects \cr
\tab \emph{Project Level Paths}   \tab \cr
\code{projectPath}\tab if \code{getwd()} is \code{name}, then just \code{getwd}; if not
\code{file.path(getwd(), name)}  \tab If current project is not this project
and using \code{Rstudio}, then the current
project will close and a new project will
open in the same Rstudio session, unless
\code{Restart = FALSE}\cr
\code{packagePath}\tab \code{file.path(tools::R_user_dir("data"), name, "packages", version$platform, substr(getRversion(), 1, 3))}
\tab appends this path to \code{.libPaths(packagePath)},
unless \code{standAlone = TRUE}, in which case,
it will set \code{.libPaths(packagePath, include.site = FALSE)} to this path \cr
------       \tab -----------                 \tab  -----         \cr
\tab \emph{Module Level Paths}                 \tab \cr
\code{cachePath}  \tab \code{file.path(projectPath, "cache")} \tab \code{options(reproducible.cachePath = cachePath)}\cr
\code{inputPath}  \tab \code{file.path(projectPath, "inputs")} \tab \code{options(spades.inputPath = inputPath)}\cr
\code{modulePath} \tab \code{file.path(projectPath, "modules")} \tab \code{options(spades.inputPath = outputPath)} \cr
\code{outputPath} \tab \code{file.path(projectPath, "outputs")} \tab \code{options(spades.inputPath = modulePath)} \cr
------       \tab -----------                 \tab  -----         \cr
\tab \emph{Temporary Paths}                 \tab  \cr
\code{scratchPath}\tab \code{file.path(tempdir(), name)} \tab \cr
\code{rasterPath} \tab \code{file.path(scratchPath, "raster")} \tab sets (\code{rasterOptions(tmpdir = rasterPath)}) \cr
\code{terraPath}  \tab \code{file.path(scratchPath, "terra")} \tab   sets (\code{terraOptions(tempdir = terraPath)})       \cr
------       \tab -----------                 \tab  -----         \cr
\tab \emph{Other Paths}                 \tab  \cr
\code{logPath}    \tab \code{file.path(outputPath(sim), "log")} \tab sets \code{options("spades.logPath")} accessible by \code{logPath(sim)}\cr
\code{tilePath}   \tab Not implemented yet \tab Not implemented yet \cr
}
}

\examples{

\dontshow{origDir <- getwd()
          tmpdir <- Require::tempdir2() # for testing tempdir2 is better}
\dontshow{
if (is.null(getOption("repos"))) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  setwd(tmpdir)
}
 ## simplest case; just creates folders
out <- setupProject(
  paths = list(projectPath = ".") #
)
# specifying functions argument, with a local file and a definition here
tf <- tempfile(fileext = ".R")
fnDefs <- c("fn <- function(x) x\n",
            "fn2 <- function(x) x\n",
            "fn3 <- function(x) terra::rast(x)")
cat(text = fnDefs, file = tf)
funHere <- function(y) y
out <- setupProject(functions = list(a = function(x) return(x),
                                     tf,
                                     funHere = funHere), # have to name it
                    # now use the functions when creating objects
                    drr = 1,
                    b = a(drr),
                    q = funHere(22),
                    ddd = fn3(terra::ext(0,b,0,b)))
\dontshow{setwd(origDir)}
}

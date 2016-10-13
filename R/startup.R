#'washb Package loading startup function
#'
#'
#' @export

.onAttach <- function(...) {
  packageStartupMessage('Welcome to the washb package')
  packageStartupMessage('Version: ', utils::packageDescription('washb')$Version)
  packageStartupMessage('Created on ', utils::packageDescription('washb')$Date)
  packageStartupMessage("\nThe package's reference manual and vignette are also online:\nhttps://ben-arnold.github.io/washb\n")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/washb",
    devtools.install.args = "",
    devtools.name = "Ben Arnold",
    devtools.desc.author = '"Ben Arnold <benarnold@berkeley.edu> [aut, cre]"',
    devtools.desc.license = "Creative Commons Attribution 3.0 Unported License",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

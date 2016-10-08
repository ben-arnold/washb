#'washb Package loading startup function
#'
#'
#' @export

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the washb package\n(Version 0.1.1)\n\nUse the command browseVignettes(\"washb\") to open the package vignette\nand learn how to use all of the package functions.")
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

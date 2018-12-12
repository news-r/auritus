.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "auritus-css",
    system.file("www/css", package = "auritus")
  )
}
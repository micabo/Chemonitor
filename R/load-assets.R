.onLoad <- function(libname, pkgname) {
  addResourcePath(
    prefix = "assets",
    directoryPath = system.file(
      "assets",
      package = "chemonitor"
    )
  )
}


.onUnload <- function(libname, pkgname) {
  removeResourcePath("assets")
}

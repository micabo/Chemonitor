deploy_app <- function(target = c("dev", "prod")) {
  # versioning
  usethis::use_version()

  # deployment
  app_files <- c(
    "app.R",
    "renv.lock",
    list.files("inst", full.names = TRUE, recursive = TRUE),
    list.files("R", "\\.R", full.names = TRUE)
  )
}

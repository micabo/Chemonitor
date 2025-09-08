source("renv/activate.R")

if (interactive()) {
  # Check renv status with development option set
  message("-> Assessing renv status with dev = TRUE:\n")
  renv::status(dev = TRUE)

  # Source development files
  development_files <- list.files("dev", "\\.R", full.names = TRUE)
  if (length(development_files) > 0) cat("Sourcing development files:\n")
  for (file in development_files) {
    cat("- ", file, "\n", sep = "")
    source(file)
  }
  rm(file)
  rm(development_files)

  # Check availability of development env vars
  # env_vars <- c("CONNECT_SERVER", "CONNECT_API_KEY")
  # for (var in env_vars) {
  #   if (is.na(Sys.getenv(var, NA))) {
  #     warning("Undefined environment variable: ", var)
  #   }
  # }
  # rm(var, env_vars)

  # Load package
  if (requireNamespace("pkgload", quietly = TRUE)) pkgload::load_all()
}

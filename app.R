pkgload::load_all()

data <- run_data_getter(get_dummy_data)
data2 <- data
data2$product <- "Something Else"

config <- list(
  version = 1,
  data = list(
    Stability = data,
    PPQ = data2,
    Production = data
  )
)

chemonitor_app(config)

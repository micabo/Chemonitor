pkgload::load_all()

data <- run_data_getter(get_dummy_data)

config <- list(
  version = 1,
  data = list(
    PPQ = data
  )
)

chemonitor_app(config)

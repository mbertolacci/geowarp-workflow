library(logger)

if (Sys.getenv('GEOWARP_LOG_LEVEL') != '') {
  log_threshold(list(
    'trace' = TRACE,
    'debug' = DEBUG,
    'info' = INFO,
    'warn' = WARN,
    'error' = ERROR
  )[[Sys.getenv('GEOWARP_LOG_LEVEL')]])
}

if (Sys.getenv('GEOWARP_THREADS') != '') {
  options('geowarp.threads' = as.integer(Sys.getenv('GEOWARP_THREADS')))
} else {
  options('geowarp.threads' = -1L)
}

midpoint <- function(x) x[floor(length(x) / 2)]

datasets_3d <- c(
  'A1',
  'A2',
  'A3',
  'B1',
  'B2',
  'B3'
)
datasets_2d <- c(
  'A2-T',
  'B2-T'
)
datasets <- c(datasets_3d, datasets_2d)
datasets_with_all <- c(datasets_3d, 'All', datasets_2d)

models <- c(
  'GeoWarp',
  'GW-CV',
  'GW-Vert-CV',
  'Linear',
  'Binned',
  'BCS'
)

name_from_fit_path <- function(fit_path) {
  stringr::str_match(basename(fit_path), '(.+)_(.+)\\.qs')[3]
}

conditional_single_locations <- list(
  A1 = c(0.80, 0.50),
  A2 = c(0.80, 0.50),
  A3 = c(0.50, 0.50),
  B1 = c(0.50, 0.15),
  B2 = c(0.50, 0.20),
  B3 = c(0.50, 0.50)
)

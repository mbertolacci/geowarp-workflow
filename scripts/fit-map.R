source('scripts/partials/base.R')
source('scripts/partials/models.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--model')
parser$add_argument('--log-file')
parser$add_argument('--n-parents', type = 'integer', default = 50)
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Reading input data from {args$input}')
input_df <- readr::read_csv(args$input, show_col_types = FALSE)
model <- model_factories[[args$model]](input_df)

grain_size <- 32L
if (Sys.getenv('GEOWARP_GRAIN_SIZE') != '') {
  grain_size <- as.integer(Sys.getenv('GEOWARP_GRAIN_SIZE'))
}

if (!is.null(args$log)) {
  log_debug('Performing fit; log output directed to {args$log}')
  dir.create(dirname(args$log), recursive = TRUE, showWarnings = FALSE)
  sink(args$log)
} else {
  log_debug('Performing fit')
}
print(system.time({
  fit <- geowarp_optimise(
    input_df,
    model,
    n_parents = args$n_parents,
    best_of = 10,
    grain_size = grain_size,
    trace = 5
  )
}))

if (!is.null(args$log)) {
  sink()
}

log_debug('Saving fit to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(fit, args$output)

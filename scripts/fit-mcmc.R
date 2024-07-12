source('scripts/partials/base.R')
source('scripts/partials/models.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(rstan)

n_threads <- 1L
if (Sys.getenv('GEOWARP_THREADS') != '') {
  n_threads <- as.integer(Sys.getenv('GEOWARP_THREADS'))
}
grain_size <- 100L
if (Sys.getenv('GEOWARP_GRAIN_SIZE') != '') {
  grain_size <- as.integer(Sys.getenv('GEOWARP_GRAIN_SIZE'))
}
group_cores <- 1L
if (Sys.getenv('GEOWARP_GROUP_CORES') != '') {
  group_cores <- as.integer(Sys.getenv('GEOWARP_GROUP_CORES'))
}

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
parent_structure <- vecchia_parent_structure(
  input_df,
  model,
  args$n_parents
)
grouping <- vecchia_grouping(parent_structure)

stan_data <- geowarp_stan_data(
  input_df,
  model,
  n_threads,
  grain_size = grain_size,
  parent_structure = parent_structure,
  grouping = grouping
)

if (!is.null(args$log_file)) {
  log_debug('Performing fit; log output directed to {args$log_file}')
  dir.create(dirname(args$log_file), recursive = TRUE, showWarnings = FALSE)
  sink(args$log_file)
} else {
  log_debug('Performing fit')
}
set_tbb_threads(n_threads)
stan_fit <- sampling(
  geowarp_stan_model('full'),
  stan_data,
  warmup = 1000,
  iter = 2000,
  refresh = 5,
  chains = 4L,
  cores = group_cores,
  verbose = TRUE,
  control = list(
    max_treedepth = 5L
  )
)
if (!is.null(args$log_file)) {
  sink()
}

output <- list(
  model = model,
  observed_df = input_df,
  stan_fit = stan_fit,
  samples = extract(stan_fit),
  parent_structure = parent_structure,
  grouping = grouping
)

log_debug('Saving fit to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(output, args$output)

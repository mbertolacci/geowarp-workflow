source('scripts/partials/base.R')
source('scripts/partials/models.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--model')
parser$add_argument('--log-file-pattern')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Reading input data from {args$input}')
input_df <- readr::read_csv(args$input, show_col_types = FALSE)

model <- model_factories[[args$model]](input_df)

log_debug('Starting fits')
dir.create(dirname(args$log_file_pattern), recursive = TRUE, showWarnings = FALSE)
fit <- geowarp_cross_validation_fit(
  input_df,
  'name',
  model,
  show_progress = TRUE,
  grouping_cores = as.integer(Sys.getenv('GEOWARP_GROUP_CORES')),
  group_output_pattern = args$log_file_pattern,
  n_parents = 50,
  best_of = 10
)

log_debug('Saving fits to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(fit, args$output)

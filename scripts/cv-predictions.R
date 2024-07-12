source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--fits')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading data from {args$input}')
input_df <- readr::read_csv(args$input, show_col_types = FALSE)

log_debug('Reading fits from {args$fits}')
fits <- qs::qread(args$fits)

log_debug('Performing CV predictions')
output <- lapply(fits, function(fit_i) {
  predicted_df <- input_df %>%
    filter(name == fit_i$group)

  predictions <- predict(
    fit_i$fit,
    predicted_df,
    include_mean = TRUE,
    include_precision_V = TRUE,
    include_samples = TRUE,
    n_samples = 2000L,
    n_parents = 100,
    prediction_type = 'vertically_dense'
  )
  predicted_df$depth_has_input_data <- predicted_df$depth <= max(fit_i$fit$observed_df$depth)
  predicted_df$log_q_c_star <- predictions$mean

  V <- predictions$precision_V
  Q <- tcrossprod(V)
  stdev <- if (is(Q, 'ddiMatrix')) {
    sqrt(1 / diag(Q))[predictions$ordering]
  } else {
    sqrt(diag(sparseinv::Takahashi_Davis(Q)))[predictions$ordering]
  }
  predicted_df$log_q_c_star_sd <- stdev
  predicted_df$log_q_c_star_samples <- predictions$samples

  list(
    name = fit_i$group,
    fit = fit_i$fit,
    is_gaussian = TRUE,
    is_nonparametric = FALSE,
    prediction = predictions,
    predicted_df = predicted_df
  )
})

log_debug('Saving predictions to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(output, args$output)

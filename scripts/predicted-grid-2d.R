source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading fit from {args$fit}')
fit <- qs::qread(args$fit)

prediction_df <- expand.grid(
  horizontal = seq(
    min(fit$observed_df$horizontal) - 5.1,
    max(fit$observed_df$horizontal) + 5.1,
    length.out = 100
  ),
  depth = seq(
    min(fit$observed_df$depth),
    max(fit$observed_df$depth),
    by = 0.05
  )
) %>%
  arrange(horizontal, depth)

depth_delta <- max(diff(sort(unique(prediction_df$depth))))
horizontal_delta <- max(diff(sort(unique(prediction_df$horizontal))))
scaling <- c(depth_delta / horizontal_delta, 1)

log_debug('Computing parent structure')
parent_structure <- vecchia_parent_structure(
  fit$observed_df,
  fit$model,
  n_parents = 100,
  prediction_df = prediction_df,
  prediction_type = 'regular',
  prediction_within_options = list(
    scaling = scaling
  )
)

log_debug('Performing prediction')
predictions <- predict(
  fit,
  prediction_df,
  include_mean = TRUE,
  include_samples = TRUE,
  parent_structure = parent_structure,
  nugget = FALSE
)
prediction_df$log_q_c_mean <- predictions$mean
prediction_df$log_q_c_sd <- matrixStats::rowSds(predictions$samples)
prediction_df$log_q_c_samples <- predictions$samples

log_debug('Writing output to {args$output}')
qs::qsave(prediction_df, args$output)

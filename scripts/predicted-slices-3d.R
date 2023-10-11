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

depth_grid <- seq(
  min(fit$observed_df$depth),
  max(fit$observed_df$depth),
  by = 0.1
)
easting_grid <- seq(
  min(fit$observed_df$easting) - 1.1,
  max(fit$observed_df$easting) + 1.1,
  length.out = 100
)
northing_grid <- seq(
  min(fit$observed_df$northing) - 1.1,
  max(fit$observed_df$northing) + 1.1,
  length.out = 100
)

# Construct slices to visualise
prediction_df <- bind_rows(
  expand.grid(
    easting = easting_grid,
    northing = northing_grid
  ) %>%
    mutate(depth = max(depth_grid)),
  expand.grid(
    northing = northing_grid,
    depth = depth_grid
  ) %>%
    mutate(easting = min(easting_grid)),
  expand.grid(
    northing = northing_grid,
    depth = depth_grid
  ) %>%
    mutate(easting = midpoint(easting_grid)),
  expand.grid(
    easting = easting_grid,
    depth = depth_grid
  ) %>%
    mutate(northing = max(northing_grid)),
  expand.grid(
    easting = easting_grid,
    depth = depth_grid
  ) %>%
    mutate(northing = midpoint(northing_grid))
) %>%
  distinct(easting, northing, depth)

depth_delta <- depth_grid[2] - depth_grid[1]
easting_delta <- easting_grid[2] - easting_grid[1]
northing_delta <- northing_grid[2] - northing_grid[1]
scaling <- c(depth_delta / easting_delta, depth_delta / northing_delta, 1)

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
  nugget = FALSE,
  parent_structure = parent_structure
)
prediction_df$log_q_c_mean <- predictions$mean
prediction_df$log_q_c_sd <- matrixStats::rowSds(predictions$samples)
prediction_df$log_q_c_samples <- predictions$samples

log_debug('Writing output to {args$output}')
qs::qsave(prediction_df, args$output)

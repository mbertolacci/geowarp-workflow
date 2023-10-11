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

locations <- fit$observed_df %>%
  ungroup() %>%
  group_by(easting, northing) %>%
  summarise(depth = max(depth), .groups = 'drop')

horizontal_grid <- expand.grid(
  easting = easting_grid,
  northing = northing_grid
) %>%
  filter(
    (
      (northing <= midpoint(northing_grid))
      & (easting >= midpoint(easting_grid))
    )
  ) %>%
  mutate(depth = max(depth_grid))
nearest <- FNN::get.knnx(
  locations %>% as.matrix(),
  horizontal_grid %>% as.matrix(),
  1
)
far_point <- horizontal_grid[which.max(nearest$nn.dist), ]
threshold <- max(5, min(round(nearest$nn.dist)))
near_point <- horizontal_grid[
  tail(which(round(nearest$nn.dist) == threshold), 1),
]

# Construct slices to visualise
prediction_df <- bind_rows(
  data.frame(
    easting = near_point[1],
    northing = near_point[2],
    depth = depth_grid
  ),
  data.frame(
    easting = far_point[1],
    northing = far_point[2],
    depth = depth_grid
  )
)

log_debug('Computing parent structure')
parent_structure <- vecchia_parent_structure(
  fit$observed_df,
  fit$model,
  n_parents = 100,
  prediction_df = prediction_df,
  prediction_type = 'vertically_dense'
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

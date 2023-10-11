source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

set.seed(1)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading fit from {args$fit}')
fit <- qs::qread(args$fit)

depth_grid <- seq(
  min(fit$observed_df$depth),
  max(fit$observed_df$depth),
  by = 0.01
)
easting_grid <- seq(
  min(fit$observed_df$easting) - 1.1,
  max(fit$observed_df$easting) + 1.1,
  length.out = 50
)
northing_grid <- seq(
  min(fit$observed_df$northing) - 1.1,
  max(fit$observed_df$northing) + 1.1,
  length.out = 50
)

# Construct slices to visualise
simulated_slices <- bind_rows(
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
simulated_slices_parent_structure <- vecchia_parent_structure(
  simulated_slices,
  fit$model,
  n_parents = 100,
  observed_type = 'regular',
  observed_options = list(scaling = scaling)
)

log_debug('Simulating slices')
simulated_slices_df <- geowarp_simulate(
  fit,
  simulated_slices,
  parent_structure = simulated_slices_parent_structure,
  nugget = FALSE
)

log_debug('Writing simulated slices to {args$output}')
qs::qsave(simulated_slices_df, args$output)

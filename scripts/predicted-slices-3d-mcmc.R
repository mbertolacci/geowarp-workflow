source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

group_cores <- if (Sys.getenv('GEOWARP_GROUP_CORES') != '') {
  as.integer(Sys.getenv('GEOWARP_GROUP_CORES'))
} else {
  1
}

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--alpha-beta-samples')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading fit from {args$fit}')
fit <- qs::qread(args$fit)

log_debug('Loading alpha beta samples from {args$alpha_beta_samples}')
alpha_beta_samples <- qs::qread(args$alpha_beta_samples)

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
n_samples <- nrow(alpha_beta_samples)
samples <- fit$samples

selected_iterations <- round(seq(
  1,
  n_samples,
  length.out = min(n_samples, 4000L)
))
parts <- parallel::mclapply(selected_iterations, function(iteration) {
  log_trace('Performing prediction for sample {iteration} / {n_samples}')
  geowarp_predict(
    prediction_df = prediction_df,
    observed_df = fit$observed_df,
    parameters = list(
      alpha_beta = alpha_beta_samples[iteration, ],
      sigma_squared_nugget = samples$sigma_squared_nugget[iteration],
      eta_deviation = samples$eta_deviation[iteration, ],
      zeta_deviation = samples$zeta_deviation[iteration, ],
      gamma_deviation_horizontal = samples$gamma_deviation_horizontal[iteration, ],
      gamma_deviation_vertical = samples$gamma_deviation_vertical[iteration, ],
      L_deviation = samples$L_deviation[iteration, , ],
      tau_squared_mean_random = samples$tau_squared_mean_random[iteration]
    ),
    model = fit$model,
    include_mean = FALSE,
    include_samples = TRUE,
    n_samples = 1L,
    parent_structure = parent_structure,
    nugget = FALSE
  )$samples
}, mc.cores = group_cores)

prediction_samples <- do.call(rbind, parts)

prediction_df$log_q_c_mean <- colMeans(prediction_samples)
prediction_df$log_q_c_sd <- matrixStats::colSds(prediction_samples)

prediction_df$log_q_c_sample <- prediction_samples[2, ]
prediction_df$epsilon_sample <- rnorm(
  nrow(prediction_df),
  sd = sqrt(samples$sigma_squared_nugget[selected_iterations[2]])
)

log_debug('Writing output to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(prediction_df, args$output)

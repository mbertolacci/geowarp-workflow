source('scripts/partials/base.R')
source('scripts/partials/models.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

options(mc.cores = as.integer(Sys.getenv('GEOWARP_THREADS')))

parser <- ArgumentParser()
parser$add_argument('--map-fits', nargs = '+')
parser$add_argument('--mcmc-fits', nargs = '+')
parser$add_argument('--alpha-beta-samples', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

depth_grid <- seq(0, 41, length.out = 100)
depth_delta <- diff(depth_grid)[1]
input_grid <- data.frame(horizontal = 0, easting = 0, northing = 0, depth = depth_grid)

calculate_profile <- function(fn) {
  lapply(
    seq_along(args$map_fits),
    function(fit_index) {
      fit_map <- qs::qread(args$map_fits[fit_index])
      fit_mcmc <- qs::qread(args$mcmc_fits[fit_index])
      alpha_beta_samples <- qs::qread(args$alpha_beta_samples[fit_index])

      name <- name_from_fit_path(args$map_fit[fit_index])

      input_grid_i <- input_grid %>%
        filter(depth <= max(fit_map$observed_df$depth))

      base_value <- fn(
        df = input_grid_i,
        model = fit_map$model,
        parameters = fit_map$parameters
      )

      samples <- fit_mcmc$samples
      n_samples <- nrow(samples$alpha_beta_hat)
      parts <- parallel::mclapply(seq_len(n_samples), function(iteration) {
        parameters_i <- list(
          alpha_beta = alpha_beta_samples[iteration, ],
          sigma_squared_nugget = samples$sigma_squared_nugget[iteration],
          eta_deviation = samples$eta_deviation[iteration, ],
          zeta_deviation = samples$zeta_deviation[iteration, ],
          gamma_deviation_horizontal = samples$gamma_deviation_horizontal[iteration, ],
          gamma_deviation_vertical = samples$gamma_deviation_vertical[iteration, ],
          L_deviation = samples$L_deviation[iteration, , ],
          tau_squared_mean_random = samples$tau_squared_mean_random[iteration]
        )

        fn(
          df = input_grid_i,
          model = fit_mcmc$model,
          parameters = parameters_i
        )
      })
      value_samples <- do.call(rbind, parts)

      base_df <- tibble(
        name = name,
        depth = depth_grid
      ) %>%
        filter(depth <= max(fit_map$observed_df$depth))

      bind_rows(
        base_df %>%
          mutate(
            method = 'MAP',
            value = base_value
          ),
        base_df %>%
          mutate(
            method = 'MCMC',
            value = colMeans(value_samples),
            value_q025 = matrixStats::colQuantiles(value_samples, probs = 0.025),
            value_q975 = matrixStats::colQuantiles(value_samples, probs = 0.975)
          )
      )
  }) %>%
    bind_rows() %>%
    mutate(name = factor(name, datasets))
}

log_debug('Calculating mean profiles')
mean_profiles <- calculate_profile(mean_profile)
log_debug('Calculating warpings')
warpings <- calculate_profile(function(parameters, ...) {
  parameters$L_deviation <- diag(nrow(parameters$L_deviation))
  output <- warped_coordinates(parameters = parameters, ...)
  value <- output[, ncol(output)]
  value_diff <- diff(value) / depth_delta
  c(value_diff[1], value_diff)
})
log_debug('Calculating stdevs')
stdevs <- calculate_profile(marginal_variance_profile) %>%
  mutate(
    value = sqrt(value),
    value_q025 = sqrt(value_q025),
    value_q975 = sqrt(value_q975)
  )

log_debug('Saving output to {args$output}')
output <- list(
  mean_profiles = mean_profiles,
  warpings = warpings,
  stdevs = stdevs
)
qs::qsave(output, args$output)

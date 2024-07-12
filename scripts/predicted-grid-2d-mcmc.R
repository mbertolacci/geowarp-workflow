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

log_debug('Writing output to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(prediction_df, args$output)

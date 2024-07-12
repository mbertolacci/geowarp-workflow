source('scripts/partials/base.R')
source('scripts/partials/models.R')

library(argparse)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Reading input data from {args$input}')
n_threads <- as.integer(Sys.getenv('GEOWARP_THREADS'))

fit <- qs::qread(args$fit)
samples <- fit$samples
n_samples <- nrow(samples$alpha_beta_hat)

log_debug('Sampling')
parts <- parallel::mclapply(seq_len(n_samples), function(iteration) {
  log_trace('Drawing sample {iteration} / {n_samples}')
  parameters_i <- list(
    sigma_squared_nugget = samples$sigma_squared_nugget[iteration],
    eta_deviation = samples$eta_deviation[iteration, ],
    zeta_deviation = samples$zeta_deviation[iteration, ],
    gamma_deviation_horizontal = samples$gamma_deviation_horizontal[iteration, ],
    gamma_deviation_vertical = samples$gamma_deviation_vertical[iteration, ],
    L_deviation = samples$L_deviation[iteration, , ],
    tau_squared_mean_random = samples$tau_squared_mean_random[iteration]
  )

  predict_alpha_beta(
    fit,
    parameters = parameters_i,
    parent_structure = fit$parent_structure,
    threads = 1L,
    include_samples = TRUE
  )$samples
}, mc.cores = n_threads)

for (part in parts) {
  if (is(part, 'try-error')) {
    print(part)
    stop('error in mclapply')
  }
}

alpha_beta_samples <- do.call(rbind, parts)

log_debug('Saving samples to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
qs::qsave(alpha_beta_samples, args$output)

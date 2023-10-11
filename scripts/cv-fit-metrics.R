source('scripts/partials/base.R')

library(argparse)
library(scoringRules)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)
library(parallel)

options(mc.cores = as.integer(Sys.getenv('GEOWARP_THREADS')))

parser <- ArgumentParser()
parser$add_argument('--cv-predictions', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading CV predictions')
cv_predictions <- mclapply(
  args$cv_predictions,
  function(filename) {
    model_name <- strsplit(basename(filename), '_')[[1]][1]
    dataset_name <- strsplit(basename(filename), '_')[[1]][2] %>%
      gsub('.qs', '', .)

    if (!file.exists(filename)) return(NULL)

    output <- qs::qread(filename)
    for (i in seq_along(output)) {
      output[[i]]$model <- model_name
      output[[i]]$dataset <- dataset_name
      output[[i]]$predicted_df$model <- model_name
      output[[i]]$predicted_df$dataset <- dataset_name
    }
    output
  }
)

log_trace('Flattening CV predictions')
cv_prediction_flat <- do.call(c, lapply(cv_predictions, function(cv_prediction_i) {
  lapply(cv_prediction_i, getElement, 'predicted_df')
})) %>%
  mclapply(function(df) {
    df %>%
      mutate(
        log_q_c_star_q025 = matrixStats::rowQuantiles(log_q_c_star_samples, probs = 0.025),
        log_q_c_star_q975 = matrixStats::rowQuantiles(log_q_c_star_samples, probs = 0.975),
        log_q_c_star_sd = matrixStats::rowSds(log_q_c_star_samples)
      )
  })

mse_score <- function(observed, posterior_mean, posterior_sd, ...) {
  (observed - posterior_mean) ^ 2
}

crps_score <- function(observed, posterior_mean, posterior_sd, ...) {
  crps_norm(observed, posterior_mean, posterior_sd)
}

logs_score <- function(observed, posterior_mean, posterior_sd, ...) {
  logs_norm(observed, posterior_mean, posterior_sd)
}

interval_score <- function(observed, lower, upper, alpha) {
  upper - lower + (2 / alpha) * (
    (lower - observed) * (observed < lower)
    + (observed - upper) * (observed > upper)
  )
}

interval005_score <- function(observed, posterior_q025, posterior_q975, ...) {
  interval_score(observed, posterior_q025, posterior_q975, alpha = 0.05)
}

coverage95_score <- function(observed, posterior_q025, posterior_q975, ...) {
  1 * ((observed < posterior_q975) & (observed > posterior_q025))
}

dss_score <- function(observed, posterior_samples, ...) {
  dss_sample(observed, posterior_samples)
}

dss_p_score <- function(observed, posterior_samples, p) {
  sapply(seq_len(floor(length(observed) / p)), function(i) {
    indices <- p * (i - 1) + seq_len(p)
    samples <- posterior_samples[indices, ]
    mu <- rowMeans(samples)
    Sigma <- cov(t(samples))
    chol_Sigma <- chol(Sigma)
    (
      sum(log(diag(chol_Sigma)))
      + 0.5 * sum(
        backsolve(chol_Sigma, observed[indices] - mu, transpose = TRUE) ^ 2
      )
    )
  })
}

dss2_score <- function(observed, posterior_samples, ...) {
  dss_p_score(observed, posterior_samples, p = 2)
}

calculate_score <- function(df_list, score_fn, name) {
  mclapply(df_list, function(df) {
    df <- df %>% filter(depth_has_input_data)
    tibble(
      metric = name,
      model = df$model[1],
      dataset = df$dataset[1],
      name = df$name[1],
      value = score_fn(
        observed = df$log_q_c,
        posterior_mean = df$log_q_c_star,
        posterior_sd = df$log_q_c_star_sd,
        posterior_q025 = df$log_q_c_star_q025,
        posterior_q975 = df$log_q_c_star_q975,
        posterior_samples = df$log_q_c_star_samples
      )
    )
  }) %>%
    bind_rows()
}

log_debug('Calculating scores')
cross_validation_scores <- bind_rows(
  calculate_score(cv_prediction_flat, mse_score, 'MSE'),
  calculate_score(cv_prediction_flat, crps_score, 'CRPS'),
  calculate_score(cv_prediction_flat, interval005_score, 'Int05'),
  calculate_score(cv_prediction_flat, dss_score, 'DSS'),
  calculate_score(cv_prediction_flat, dss2_score, 'DSS2')
)

log_debug('Aggregating scores')
output <- bind_rows(
  cross_validation_scores %>%
    group_by(metric, model, dataset) %>%
    summarise(value = mean(value), .groups = 'drop'),
  cross_validation_scores %>%
    filter(dataset != 'B2-T') %>%
    group_by(metric, model) %>%
    summarise(value = mean(value), .groups = 'drop') %>%
    mutate(dataset = 'All')
)

log_debug('Saving scores to {args$output}')
qs::qsave(output, args$output)

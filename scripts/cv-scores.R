source('scripts/partials/base.R')

library(argparse)
library(scoringRules)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading CV predictions from {args$input}')

model_name <- strsplit(basename(args$input), '_')[[1]][1]
dataset_name <- strsplit(basename(args$input), '_')[[1]][2] %>%
  gsub('.qs', '', .)


cv_predictions <- qs::qread(args$input) %>%
  lapply(function(part) {
    if (part$is_gaussian) {
      part$predicted_df$log_q_c_star_q025 <- (
        part$predicted_df$log_q_c_star
        + qnorm(0.025) * part$predicted_df$log_q_c_star_sd
      )
      part$predicted_df$log_q_c_star_q975 <- (
        part$predicted_df$log_q_c_star
        + qnorm(0.975) * part$predicted_df$log_q_c_star_sd
      )
    } else {
      part$predicted_df$log_q_c_star_sd <- NA
    }

    part
  })


mse_score <- function(observed, posterior_mean, posterior_sd, ...) {
  (observed - posterior_mean) ^ 2
}

crps_score_gaussian <- function(observed, posterior_mean, posterior_sd, ...) {
  crps_norm(observed, posterior_mean, posterior_sd)
}

crps_score_sample <- function(observed, posterior_samples, ...) {
  crps_sample(observed, posterior_samples)
}

logs_score_gaussian <- function(observed, posterior_mean, posterior_sd, ...) {
  logs_norm(observed, posterior_mean, posterior_sd)
}

logs_score_sample <- function(observed, posterior_samples, ...) {
  logs_sample(observed, posterior_samples)
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

dss_score_gaussian <- function(observed, posterior_mean, posterior_sd, ...) {
  dss_norm(observed, posterior_mean, posterior_sd)
}

dss_score_sample <- function(observed, posterior_samples, ...) {
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

calculate_score <- function(metric_name, score_fn, score_fn_non_gaussian, allow_nonparametric = TRUE) {
  has_score_fn_non_gaussian <- !missing(score_fn_non_gaussian)
  log_trace('Calculating {metric_name}')
  lapply(cv_predictions, function(part) {
    if (part$is_nonparametric && !allow_nonparametric) {
      return(NULL)
    }

    score_fn_i <- score_fn
    if (!part$is_gaussian && has_score_fn_non_gaussian) {
      score_fn_i <- score_fn_non_gaussian
    }

    df <- part$predicted_df %>% filter(depth_has_input_data)
    tibble(
      metric = metric_name,
      model = model_name,
      dataset = dataset_name,
      name = part$name,
      value = score_fn_i(
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
output <- bind_rows(
  calculate_score('MSE', mse_score),
  calculate_score('CRPS', crps_score_gaussian, crps_score_sample),
  calculate_score('Int05', interval005_score),
  calculate_score('DSS', dss_score_gaussian, dss_score_sample, allow_nonparametric = FALSE),
  calculate_score('DSS2', dss2_score, allow_nonparametric = FALSE)
)

log_debug('Saving scores to {args$output}')
qs::qsave(output, args$output)

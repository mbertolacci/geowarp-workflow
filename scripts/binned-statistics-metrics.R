source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

interval_score <- function(observed, lower, upper, alpha) {
  upper - lower + (2 / alpha) * (
    (lower - observed) * (observed < lower)
    + (observed - upper) * (observed > upper)
  )
}

dss_p_score <- function(observed, posterior_samples, p) {
  sapply(seq_len(floor(length(observed) / p)), function(i) {
    indices <- p * (i - 1) + seq_len(p)
    samples <- posterior_samples[indices, ]
    mu <- rowMeans(samples)
    Sigma <- cov(t(samples))
    diag(Sigma) <- diag(Sigma) + 1e-6
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

parser <- ArgumentParser()
parser$add_argument('--data-directory')
parser$add_argument('--output')
args <- parser$parse_args()

filenames <- list.files(args$data_directory, full.names = TRUE)
datasets <- lapply(filenames, function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  name <- basename(filename) %>%
    stringr::str_remove('\\.csv$')
  list(name = name, df = df)
})

all_dataset_df <- lapply(datasets, function(dataset_i) {
  dataset_i$df %>%
    mutate(dataset = dataset_i$name)
}) %>%
  bind_rows() %>%
  mutate(depth_bin = 0.1 * floor(depth / 0.1))

binned_predictions <- all_dataset_df %>%
  group_by(dataset, name) %>%
  group_modify(~ {
    withheld_statistics <- all_dataset_df %>%
      filter(dataset == .y$dataset, name != .y$name) %>%
      group_by(depth_bin) %>%
      summarise(
        log_q_c_mean = mean(log_q_c),
        log_q_c_q025 = quantile(log_q_c, probs = 0.025),
        log_q_c_q975 = quantile(log_q_c, probs = 0.975),
        log_q_c_samples = t(sample(log_q_c, 100, replace = TRUE))
      )

    .x %>%
      left_join(withheld_statistics, by = 'depth_bin')
  }) %>%
  filter(!is.na(log_q_c_mean))

binned_scores <- bind_rows(
  binned_predictions %>%
    mutate(
      metric = 'MSE',
      value = mean((log_q_c - log_q_c_mean) ^ 2)
    ),
  binned_predictions %>%
    mutate(
      metric = 'CRPS',
      value = scoringRules::crps_sample(log_q_c, log_q_c_samples)
    ),
  binned_predictions %>%
    mutate(
      metric = 'Int05',
      value = interval_score(log_q_c, log_q_c_q025, log_q_c_q975, alpha = 0.05)
    )
)

binned_metrics <- bind_rows(
  binned_scores %>%
    group_by(metric, dataset) %>%
    summarise(value = mean(value)),
  binned_scores %>%
    group_by(metric) %>%
    summarise(value = mean(value)) %>%
    mutate(dataset = 'All')
) %>%
  ungroup() %>%
  mutate(model = 'Binned')

qs::qsave(binned_metrics, args$output)

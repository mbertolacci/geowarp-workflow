source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parents_1d <- function(n, k) {
  output <- matrix(NA, nrow = n, ncol = k + 1)
  for (i in seq_len(k + 1)) {
    output[, i] <- seq_len(n) - i + 1
  }
  output[output <= 0] <- NA
  output
}

parser <- ArgumentParser()
parser$add_argument('--datasets', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

datasets <- lapply(args$datasets, function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  name <- basename(filename) %>%
    stringr::str_remove('\\.csv$')
  list(name = name, df = df)
})

dataset_df <- lapply(datasets, function(dataset_i) {
  dataset_i$df %>%
    mutate(
      dataset = dataset_i$name
    ) %>%
    group_by(depth_bin = 0.1 * floor(depth / 0.1)) %>%
    mutate(log_q_c_resid = log_q_c - mean(log_q_c)) %>%
    ungroup()
}) %>%
  bind_rows()

covariance_functions <- c(
  'exponential_isotropic', 'matern15_isotropic', 'matern25_isotropic',
  'matern35_isotropic', 'matern45_isotropic'
)

results_df <- expand.grid(
  name = unique(dataset_df$name),
  covariance_function = covariance_functions,
  stringsAsFactors = FALSE
)

results_df$fit <- pbmcapply::pbmclapply(
  seq_len(nrow(results_df)),
  function(i) {
    df_i <- dataset_df %>%
      filter(name == results_df$name[i])

    GpGp::fit_model(
      df_i$log_q_c_resid,
      cbind(df_i$depth),
      cbind(rep(1, nrow(df_i))),
      results_df$covariance_function[i],
      reorder = FALSE,
      NNarray = parents_1d(nrow(df_i), 30),
      m_seq = c(10, 30),
      silent = TRUE
    )
  },
  mc.cores = parallel::detectCores(),
  ignore.interactive = TRUE
)

results_df$log_likelihood <- sapply(
  seq_len(nrow(results_df)),
  function(i) {
    df_i <- dataset_df %>%
      filter(name == results_df$name[i])
    fit_i <- results_df$fit[[i]]

    GpGp::vecchia_meanzero_loglik(
      fit_i$covparms,
      fit_i$covfun_name,
      df_i$log_q_c_resid,
      cbind(df_i$depth),
      parents_1d(nrow(df_i), 50)
    )$loglik
  }
)

qs::qsave(results_df, args$output)

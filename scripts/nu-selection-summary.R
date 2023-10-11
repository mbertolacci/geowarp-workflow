source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--datasets', nargs = '+')
parser$add_argument('--nu-selection-fits')
parser$add_argument('--output')
args <- parser$parse_args()

datasets <- lapply(args$datasets, function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  name <- basename(filename) %>%
    stringr::str_remove('\\.csv$')
  list(name = name, df = df)
})

dataset_df <- datasets %>%
  lapply(function(dataset_i) {
    dataset_i$df %>%
      mutate(
        dataset = dataset_i$name
      ) %>%
      group_by(depth_bin = 0.1 * floor(depth / 0.1)) %>%
      mutate(log_q_c_resid = log_q_c - mean(log_q_c)) %>%
      ungroup()
  }) %>%
  bind_rows()

fits <- qs::qread(args$nu_selection_fits)

name_to_dataset <- dataset_df$dataset
names(name_to_dataset) <- dataset_df$name

best_df <- fits %>%
  group_by(name) %>%
  summarise(
    covariance_function = covariance_function[which.max(log_likelihood)]
  )

sink(args$output)
cat('Cross tabs:\n')
best_df %>%
  group_by(covariance_function) %>%
  summarise(n = n()) %>%
  knitr::kable()
cat('\n')
best_df %>%
  mutate(dataset = name_to_dataset[name]) %>%
  group_by(dataset, covariance_function) %>%
  summarise(n = n()) %>%
  knitr::kable()
sink(NULL)

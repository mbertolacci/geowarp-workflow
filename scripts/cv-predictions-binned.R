source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading data from {args$input}')
input_df <- readr::read_csv(args$input, show_col_types = FALSE) %>%
  mutate(depth_bin = 0.1 * floor(depth / 0.1))

output <- lapply(unique(input_df$name), function(name_i) {
  retained_df <- input_df %>%
    filter(name != name_i)

  retained_statistics <- retained_df %>%
    group_by(depth_bin) %>%
    summarise(
      log_q_c_star = mean(log_q_c),
      log_q_c_star_q025 = quantile(log_q_c, probs = 0.025),
      log_q_c_star_q975 = quantile(log_q_c, probs = 0.975),
      log_q_c_star_samples = t(sample(log_q_c, 100, replace = TRUE))
    )

  predicted_df <- input_df %>%
    filter(name == name_i) %>%
    left_join(retained_statistics, by = 'depth_bin') %>%
    filter(!is.na(log_q_c_star)) %>%
    mutate(
      depth_has_input_data = depth <= max(retained_df$depth)
    )

  list(
    name = name_i,
    is_gaussian = FALSE,
    is_nonparametric = TRUE,
    predicted_df = predicted_df
  )
})

qs::qsave(output, args$output)

source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(boot)
library(parallel)

if (Sys.getenv('GEOWARP_THREADS') != '') {
  options('mc.cores' = as.integer(Sys.getenv('GEOWARP_THREADS')))
}

parser <- ArgumentParser()
parser$add_argument('--cv-scores', nargs = '+')
parser$add_argument('--method')
parser$add_argument('--output')
args <- parser$parse_args()

stopifnot(args$method %in% c('t_test', 'blocked_bootstrap'))

log_debug('Loading CV scores')
cv_scores_base <- lapply(args$cv_scores, qs::qread) %>%
  bind_rows()

cv_scores_all <- cv_scores_base %>%
  filter(dataset %in% datasets_for_all) %>%
  mutate(dataset = 'All')

cv_scores <- bind_rows(cv_scores_base, cv_scores_all)

output <- cv_scores %>%
  group_by(metric, dataset) %>%
  group_modify(~ {
    log_debug('Calculating best model for {.y$metric} and {.y$dataset}')
    models <- .x %>%
      group_by(model) %>%
      group_map(~ .y$model) %>%
      unlist()
    names <- (
      .x %>%
        group_by(model) %>%
        group_map(~ .x$name)
    )[[1]]
    score_matrix <- do.call(
      cbind,
      .x %>%
        group_by(model) %>%
        group_map(~ .x$value)
    )
    score_average <- colMeans(score_matrix)
    index_best <- which.min(score_average)
    is_best <- score_average == min(score_average)

    if (args$method == 'blocked_bootstrap') {
      # This resamples using block bootstrap within each CPT, then combines the
      # replicates across CPTs to generate a distribution of the difference
      bootstrap_parts <- mclapply(unique(names), function(name_i) {
        score_matrix_i <- score_matrix[names == name_i, ]

        tsboot(
          ts(score_matrix_i),
          function(x) {
            colSums(x - x[, index_best])
          },
          R = 1000,
          l = 50,
          sim = 'fixed'
        )$t
      })

      bootstrap_replicates <- Reduce(`+`, bootstrap_parts) / nrow(score_matrix)

      threshold <- matrixStats::colQuantiles(bootstrap_replicates, probs = 0.05)
      is_equal_best <- threshold <= 0
    } else if (args$method == 't_test') {
      is_equal_best <- sapply(seq_len(ncol(score_matrix)), function(i) {
        if (i == index_best) {
          TRUE
        } else {
          t.test(
            score_matrix[, index_best],
            score_matrix[, i],
            paired = FALSE,
            alternative = 'less'
          )$p.value > 0.05
        }
      })
    }

    tibble(
      model = models,
      value_mean = score_average,
      is_best = is_best,
      is_equal_best = is_equal_best
    )
  }) %>%
  ungroup()

log_debug('Saving scores to {args$output}')
qs::qsave(output, args$output)

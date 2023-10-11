source('scripts/partials/base.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--fit-metrics')
parser$add_argument('--binned-statistics-metrics')
parser$add_argument('--bcs-metrics', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

metrics <- qs::qread(args$fit_metrics) %>%
  bind_rows(
    qs::qread(args$binned_statistics_metrics),
    lapply(args$bcs_metrics, readRDS)
  ) %>%
  mutate(
    dataset = factor(dataset, levels = datasets_with_all),
    model = factor(model, levels = models)
  )

qs::qsave(metrics, args$output)

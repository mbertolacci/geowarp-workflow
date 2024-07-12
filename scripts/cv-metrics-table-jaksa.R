source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--cv-fit-metrics', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

cv_fit_metrics <- qs::qread(args$cv_fit_metrics)

metrics_to_show <- c('MSE', 'CRPS', 'Int05', 'DSS', 'DSS2')
datasets_to_show <- intersect(datasets_with_all, unique(cv_fit_metrics$dataset))
models_to_show <- intersect(models, unique(cv_fit_metrics$model))

input_data <- expand.grid(
  metric = metrics_to_show,
  dataset = datasets_to_show,
  model = models_to_show,
  stringsAsFactors = FALSE
) %>%
  left_join(cv_fit_metrics, by = c('metric', 'dataset', 'model')) %>%
  mutate(
    metric = factor(metric, levels = metrics_to_show),
    dataset = factor(dataset, levels = datasets_to_show),
    model = factor(model, levels = models_to_show)
  ) %>%
  arrange(metric, dataset, model)

cat_nl <- function(...) {
  cat(...)
  cat('\n')
}
printf <- function(...) cat(sprintf(...))
printf_nl <- function(...) {
  printf(...)
  cat('\n')
}
replace_na_empty <- function(x) {
  x[x == 'NA'] <- ''
  x
}
join_amp <- function(x) paste0(x, collapse = ' & ')

sink(args$output)
printf_nl(
  r'(\begin{tabular}{r|%s})',
  paste0(rep('r', length(metrics_to_show)), collapse = '')
)
cat_nl(r'(\hline \hline)')
printf_nl(r'(& %s \\)', join_amp(metrics_to_show))
cat_nl(r'(\hline)')
for (model_i in models_to_show) {
  printf_nl(r'(%s)', model_i)
  for (metric_j in metrics_to_show) {
    df_i_j <- input_data %>%
      filter(
        model == model_i,
        metric == metric_j
      )
    printf_nl(
      r'(& %s)',
      replace_na_empty(sprintf(
        '%s%.03f%s',
        case_when(
          df_i_j$is_best ~ '\\textbf{',
          df_i_j$is_equal_best ~ '\\textbf{',
          TRUE ~ ''
        ),
        df_i_j$value_mean,
        case_when(
          df_i_j$is_best ~ '}',
          df_i_j$is_equal_best ~ '}',
          TRUE ~ ''
        )
      ))
    )
  }
  cat_nl(r'(\\)')
}
cat_nl(r'(\hline \hline)')
cat_nl(r'(\end{tabular})')
sink(NULL)

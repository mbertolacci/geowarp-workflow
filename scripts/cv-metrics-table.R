source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

replace_na_false <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

parser <- ArgumentParser()
parser$add_argument('--cv-metrics', nargs = '+')
parser$add_argument('--models', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

metrics <- qs::qread(args$cv_metrics)

metric_order <- c('MSE', 'CRPS', 'Int05', 'DSS', 'DSS2')
input_data <- expand.grid(
  metric = metric_order,
  dataset = factor(datasets_with_all, levels = datasets_with_all),
  model = factor(unique(args$models), levels = models),
  stringsAsFactors = FALSE
) %>%
  left_join(metrics, by = c('metric', 'dataset', 'model')) %>%
  arrange(metric, dataset, model) %>%
  group_by(metric, dataset) %>%
  mutate(
    is_best = replace_na_false(value == min(value, na.rm = TRUE))
  )


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
  r'(\begin{tabular}{rr|%s|r|%s})',
  paste0(rep('r', length(datasets_3d)), collapse = ''),
  paste0(rep('r', length(datasets_2d)), collapse = '')
)
cat_nl(r'(\hline \hline)')
printf_nl(r'(& & %s \\)', join_amp(datasets_with_all))
for (metric_i in metric_order) {
  cat_nl(r'(\hline)')
  if (startsWith(metric_i, 'DSS')) {
    models_i <- args$models[args$models != 'Binned']
  } else {
    models_i <- args$models
  }
  printf_nl(r'(\multirow{%d}{*}{%s})', length(models_i), metric_i)
  for (model_j in models_i) {
    df_i_j <- input_data %>%
      filter(
        metric == metric_i,
        model == model_j
      ) %>%
      arrange(dataset)
    printf_nl(
      r'(& %s & %s \\)',
      model_j,
      join_amp(replace_na_empty(sprintf(
        '%s%.03f%s',
        ifelse(df_i_j$is_best, '\\textbf{', ''),
        df_i_j$value,
        ifelse(df_i_j$is_best, '}', '')
      )))
    )
  }
}
cat_nl(r'(\hline \hline)')
cat_nl(r'(\end{tabular})')
sink(NULL)

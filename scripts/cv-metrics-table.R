source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--cv-fit-metrics', nargs = '+')
parser$add_argument('--dash-after', nargs = '+')
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
  r'(\begin{tabular}{rr|%s|>{\columncolor[gray]{0.95}}r|%s})',
  paste0(rep('r', length(
    intersect(datasets_3d, datasets_to_show)
  )), collapse = ''),
  paste0(rep('r', length(
    intersect(datasets_2d, datasets_to_show)
  )), collapse = '')
)
cat_nl(r'(\hline \hline)')
printf_nl(r'(& & %s \\)', join_amp(datasets_to_show))
for (metric_i in metrics_to_show) {
  cat_nl(r'(\hline)')
  if (startsWith(metric_i, 'DSS')) {
    models_i <- models_to_show[models_to_show != 'Binned']
  } else {
    models_i <- models_to_show
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
      )))
    )
    if (model_j %in% args$dash_after) {
      printf_nl(r'(\cdashline{2-%d})', length(datasets) + 2)
    }
  }
}
cat_nl(r'(\hline \hline)')
cat_nl(r'(\end{tabular})')
sink(NULL)

source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggrepel)

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

dataset_plots <- lapply(datasets, function(dataset) {
  label_df <- dataset$df %>%
    group_by(short_name) %>%
    summarise(
      final_depth = tail(depth, 1),
      final_log_q_c = tail(log_q_c, 1)
    )
  hidden_df <- dataset$df %>%
    group_by(short_name, final_depth = 0.5 * round(depth / 0.5)) %>%
    summarise(
      final_log_q_c = mean(log_q_c)
    ) %>%
    mutate(short_name = '')

  ggplot() +
    geom_line(
      data = dataset$df,
      mapping = aes(depth, log_q_c, colour = short_name)
    ) +
    geom_label_repel(
      data = bind_rows(
        label_df,
        hidden_df
      ),
      mapping = aes(
        final_depth,
        final_log_q_c,
        label = short_name,
        colour = short_name
      ),
      segment.linetype = 'dotted',
      size = 3,
      max.overlaps = 100000,
      max.iter = 1e7,
      nudge_x = c(
        rep(2, nrow(label_df)),
        rep(0, nrow(hidden_df))
      ),
      nudge_y = c(
        rep(-2, nrow(label_df)),
        rep(0, nrow(hidden_df))
      ),
      min.segment.length = 0
    ) +
    guides(colour = 'none') +
    scale_colour_manual(
      values = c('black', NAME_PALETTE)
    ) +
    labs(x = 'Depth [m]', y = expression('log '*q[c]), colour = NULL) +
    scale_x_reverse() +
    ylim(
      if (!any(grepl('Jaksa', args$datasets))) -5 else -1.25,
      if (!any(grepl('Jaksa', args$datasets))) 2.5 else 2.75
    ) +
    coord_flip() +
    ggtitle(
      if (dataset$name != 'Jaksa') dataset$name else 'Measurements'
    )
})

output <- wrap_plots(
  dataset_plots,
  ncol = if (length(args$datasets) > 1) 2 else 1
)

if (length(args$datasets) > 1) {
  ggsave_fullwidth(
    args$output,
    output,
    height = 20
  )
} else {
  ggsave_base(
    args$output,
    output,
    width = 8,
    height = 10
  )
}

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
    ylim(-5, 2.5) +
    coord_flip() +
    ggtitle(dataset$name)
})

output <- wrap_plots(dataset_plots, ncol = 2)

ggsave_fullwidth(args$output, output, height = 20)

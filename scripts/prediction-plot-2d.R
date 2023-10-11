source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--predicted-grid')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading fit from {args$fit}')
fit <- qs::qread(args$fit)

log_debug('Loading predicted grid from {args$predicted_grid}')
predicted_grid <- qs::qread(args$predicted_grid)

pcpt_segments <- geom_segment(
  data = fit$observed_df %>%
    group_by(name, horizontal) %>%
    summarise(min_depth = min(depth), max_depth = max(depth)),
  mapping = aes(
    x = horizontal,
    xend = horizontal,
    y = min_depth,
    yend = max_depth
  ),
  colour = 'black',
  inherit.aes = FALSE
)

plot_grid <- function(df, variable) {
  scaling_horizontal <- diff(sort(unique(df$horizontal)))[1]
  scaling_depth <- diff(sort(unique(df$depth)))[1]
  ggplot() +
    geom_rect(
      data = df,
      mapping = aes(
        xmin = horizontal - scaling_horizontal / 2,
        # HACK(mgnb): by making the grid cells a bit too large, the border
        # between cells becomes invisible
        xmax = horizontal + 1.2 * scaling_horizontal / 2,
        ymin = depth - scaling_depth / 2,
        ymax = depth + 1.2 * scaling_depth / 2,
        fill = {{ variable }}
      )
    ) +
    pcpt_segments +
    scale_y_reverse() +
    guides(
      fill = guide_colourbar(
        title.position = 'top',
        barwidth = 12
      )
    ) +
    labs(x = 'Horizontal [m]', y = 'Depth [m]') +
    theme(
      legend.position = 'bottom'
    )
}

output <- wrap_plots(
  plot_grid(
    predicted_grid,
    log_q_c_mean
  ) +
    scale_fill_gradientn(
      colours = log_q_c_colours,
      limits = log_q_c_scale_limits
    ) +
    labs(fill = expression('Posterior mean of '*Y(bold(s), h))),
  plot_grid(
    predicted_grid,
    log_q_c_sd
  ) +
    scale_fill_gradientn(
      colours = log_q_c_sd_colours,
      limits = log_q_c_sd_scale_limits
    ) +
    labs(fill = expression('Posterior st. dev. of '*Y(bold(s), h))),
  nrow = 1
)

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_fullwidth(args$output, output, height = 11)

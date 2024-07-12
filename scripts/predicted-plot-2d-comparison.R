source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--data')
parser$add_argument('--predicted-grid-map')
parser$add_argument('--predicted-grid-mcmc')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading data from {args$data}')
input_df <- readr::read_csv(args$data)

log_debug('Loading MAP predicted grid from {args$predicted_grid_map}')
predicted_grid_map <- qs::qread(args$predicted_grid_map)

log_debug('Loading MCMC predicted grid from {args$predicted_grid_mcmc}')
predicted_grid_mcmc <- qs::qread(args$predicted_grid_mcmc)

pcpt_segments <- geom_segment(
  data = input_df %>%
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

no_y_axis <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

mean_plot <- wrap_plots(
  plot_grid(
    predicted_grid_mcmc,
    log_q_c_mean
  ) +
    scale_fill_gradientn(
      colours = log_q_c_colours,
      limits = log_q_c_scale_limits
    ) +
    guides(fill = guide_colourbar(
      title.position = 'top',
      barwidth = unit(5, 'cm')
    )) +
    labs(fill = expression('Posterior mean of '*Y(bold(s), h))) +
    ggtitle('MCMC') +
    theme(plot.title = element_text(hjust = 0.5)),
  plot_grid(
    predicted_grid_map,
    log_q_c_mean
  ) +
    scale_fill_gradientn(
      colours = log_q_c_colours,
      limits = log_q_c_scale_limits
    ) +
    guides(fill = guide_colourbar(
      title.position = 'top',
      barwidth = unit(5, 'cm')
    )) +
    labs(fill = expression('Posterior mean of '*Y(bold(s), h))) +
    ggtitle('MAP') +
    no_y_axis +
    theme(plot.title = element_text(hjust = 0.5)),
  nrow = 1,
  guides = 'collect'
) &
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(2, 'cm')
  )

predicted_grid_diff <- predicted_grid_mcmc %>%
  mutate(log_q_c_mean_diff = log_q_c_mean - predicted_grid_map$log_q_c_mean)

mean_diff_plot <- plot_grid(
  predicted_grid_diff,
  log_q_c_mean_diff
) +
  scale_fill_gradientn(
    colours = log_q_c_diff_colours,
    limits = log_q_c_diff_scale_limits
  ) +
  labs(y = NULL, fill = expression('MCMC '*Y(bold(s), h) - 'MAP '*Y(bold(s), h))) +
  guides(fill = guide_colourbar(
    title.position = 'top',
    barwidth = unit(5, 'cm')
  )) +
  no_y_axis +
  ggtitle('Comparison') +
  theme(plot.title = element_text(hjust = 0.5))

mean_row <- wrap_plots(
  mean_plot,
  mean_diff_plot,
  widths = c(2, 1),
  nrow = 1
)


sd_plot <- wrap_plots(
  plot_grid(
    predicted_grid_mcmc,
    log_q_c_sd
  ) +
    scale_fill_gradientn(
      colours = log_q_c_sd_colours,
      limits = log_q_c_sd_scale_limits
    ) +
    labs(fill = expression('Posterior st. dev. of '*Y(bold(s), h))) +
    guides(fill = guide_colourbar(
      title.position = 'top',
      barwidth = unit(5, 'cm')
    )),
  plot_grid(
    predicted_grid_map,
    log_q_c_sd
  ) +
    scale_fill_gradientn(
      colours = log_q_c_sd_colours,
      limits = log_q_c_sd_scale_limits
    ) +
    labs(fill = expression('Posterior st. dev. of '*Y(bold(s), h))) +
    guides(fill = guide_colourbar(
      title.position = 'top',
      barwidth = unit(5, 'cm')
    )) +
    no_y_axis,
  nrow = 1,
  guides = 'collect'
) &
  theme(legend.position = 'bottom')


predicted_grid_ratio <- predicted_grid_mcmc %>%
  mutate(log_q_c_sd_ratio = log_q_c_sd / predicted_grid_map$log_q_c_sd)

sd_ratio_plot <- plot_grid(
  predicted_grid_ratio,
  log_q_c_sd_ratio
) +
  scale_fill_gradientn(
    colours = log_q_c_sd_ratio_colours,
    limits = log_q_c_sd_ratio_scale_limits,
    breaks = log_q_c_sd_ratio_breaks,
    labels = log_q_c_sd_ratio_labels,
    trans = 'log10'
  ) +
  labs(y = NULL, fill = 'MCMC st. dev. / MAP st. dev.') +
  guides(fill = guide_colourbar(
    title.position = 'top',
    barwidth = unit(5, 'cm')
  )) +
  no_y_axis

sd_row <- wrap_plots(
  sd_plot,
  sd_ratio_plot,
  widths = c(2, 1),
  nrow = 1
)

output <- wrap_plots(
  mean_row,
  sd_row,
  ncol = 1
)

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_fullwidth(args$output, output, height = 20)

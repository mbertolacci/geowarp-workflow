source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)
library(parallel)

if (Sys.getenv('GEOWARP_THREADS') != '') {
  options('mc.cores' = as.integer(Sys.getenv('GEOWARP_THREADS')))
}

parser <- ArgumentParser()
parser$add_argument('--fits', nargs = '+')
parser$add_argument('--horizontal-coordinate')
parser$add_argument('--output')
args <- parser$parse_args()

stopifnot(args$horizontal_coordinate %in% c('easting', 'northing'))

fits <- lapply(args$fits, qs::qread)

circle_depth_grid <- seq(
  2,
  40,
  by = 2
)

circle_centres <- data.frame(
  easting = 0,
  northing = 0,
  depth = circle_depth_grid
) %>%
  mutate(index = seq_len(n()))

dimensions <- c(
  if (args$horizontal_coordinate == 'easting') 1 else 2,
  3
)

circle_df <- lapply(seq_along(fits), function(k) {
  log_debug('Processing {args$fits[k]}')
  name <- name_from_fit_path(args$fits[k])
  n_parents <- n_parents_from_fit_path(args$fits[k])

  fit <- fits[[k]]
  max_depth <- max(fit$observed_df$depth)
  circle_centres_k <- circle_centres %>% filter(depth <= max_depth)

  mclapply(seq_len(nrow(circle_centres_k)), function(i) {
    centre_i <- c(
      circle_centres_k$easting[i],
      circle_centres_k$northing[i],
      circle_centres_k$depth[i]
    )

    isocorrelation_contour(
      fit$model,
      fit$parameters,
      centre_i,
      dimensions
    ) %>%
      mutate(
        index = i,
        easting_centre = centre_i[1],
        northing_centre = centre_i[2],
        depth_centre = centre_i[3]
      )
  }) %>%
    bind_rows() %>%
    mutate(
      name = name,
      n_parents = n_parents
    )
}) %>%
  bind_rows() %>%
  mutate(n_parents = factor(n_parents))

colour_scale <- scale_colour_manual(
  values = c('red', '#377eb8', '#4daf4a')
)
linetype_scale <- scale_linetype_manual(
  values = c('solid', 'dashed', 'dotted')
)

circle_plot <- function(x_label, x_var, x_point_var) {
  output <- ggplot()
  for (index_i in unique(circle_df$index)) {
    output <- output +
      geom_path(
        data = circle_df %>% filter(index == index_i),
        mapping = aes({{ x_var }}, depth, colour = n_parents, linetype = n_parents),
        linewidth = 0.4
      )
  }
  output +
    geom_point(
      data = circle_df %>% distinct(name, index, {{ x_point_var }}, depth_centre),
      mapping = aes({{ x_point_var }}, depth_centre),
      size = 0.3
    ) +
    colour_scale +
    linetype_scale +
    scale_x_continuous(
      breaks = c(-60, 0, 60),
      limits = c(-62, 62)
    ) +
    scale_y_reverse() +
    facet_grid(~ name) +
    theme(
      legend.position = 'bottom',
      axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
      panel.grid.minor.x = element_line()
    ) +
    labs(
      x = x_label,
      y = 'Depth [m]',
      colour = '# Parents',
      linetype = '# Parents'
    )
}

output <- if (args$horizontal_coordinate == 'easting') {
  circle_plot('Easting [m]', easting, easting_centre)
} else {
  circle_plot('Northing [m]', northing, easting_centre)
}

ggsave_fullwidth(args$output, output, height = 20)

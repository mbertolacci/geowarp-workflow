source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

parser <- ArgumentParser()
parser$add_argument('--fits', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

fits <- lapply(args$fits, qs::qread)

circle_centre <- c(0, 0, 10)
radius <- uniroot(
  function(x) (1.0 + sqrt(3) * x) * exp(-sqrt(3) * x) - 0.5,
  c(0, 2)
)$root

circle_df <- lapply(seq_along(fits), function(k) {
  name <- name_from_fit_path(args$fits[k])
  n_parents <- n_parents_from_fit_path(args$fits[k])

  fit <- fits[[k]]

  isocorrelation_contour(
    fit$model,
    fit$parameters,
    c(0, 0, 10),
    c(1, 2)
  ) %>%
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

output_parts <- lapply(unique(circle_df$name), function(name_i) {
  df_i <- circle_df %>% filter(name == name_i)
  ggplot(df_i, aes(easting, northing, colour = n_parents, linetype = n_parents)) +
    geom_path(linewidth = 0.8) +
    annotate('point', x = 0, y = 0) +
    coord_fixed() +
    xlim(-60, 60) +
    ylim(-60, 60) +
    colour_scale +
    linetype_scale +
    labs(
      x = 'Easting [m]',
      y = 'Northing [m]',
      colour = '# Parents',
      linetype = '# Parents'
    ) +
    ggtitle(name_i)
})

output <- wrap_plots(
  output_parts,
  ncol = 3,
  widths = c(1, 1, 1),
  heights = c(1, 1),
  guides = 'collect'
) &
  theme(legend.position = 'bottom')

ggsave_fullwidth(args$output, output, height = 14)

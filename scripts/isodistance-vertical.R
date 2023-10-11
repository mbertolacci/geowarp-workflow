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
radius <- uniroot(
  function(x) (1.0 + sqrt(3) * x) * exp(-sqrt(3) * x) - 0.5,
  c(0, 2)
)$root

circle_df <- lapply(seq_along(fits), function(k) {
  name <- name_from_fit_path(args$fits[k])
  fit <- fits[[k]]
  max_depth <- max(fit$observed_df$depth)
  circle_centres_warped <- warped_coordinates(
    fit,
    circle_centres %>% filter(depth <= max_depth)
  )

  lapply(seq_len(nrow(circle_centres_warped)), function(i) {
    centre <- circle_centres_warped[i, ]
    combinations <- cbind(c(1, 3), c(2, 3))
    lapply(seq_len(ncol(combinations)), function(k) {
      # Skips circles that jump out of the bounds
      tryCatch({
        theta <- seq(0, 2 * pi, length.out = 200)
        x_warped <- matrix(0, nrow = 200, ncol = 3)
        x_warped[, combinations[1, k]] <- centre[combinations[1, k]] + radius * cos(theta)
        x_warped[, combinations[2, k]] <- centre[combinations[2, k]] + radius * sin(theta)
        x_warped[, setdiff(1:3, combinations[, k])] <- centre[setdiff(1:3, combinations[, k])]

        x_unwarped <- unwarped_coordinates(fit, x_warped)
        data.frame(
          index = i,
          perspective = k,
          easting_centre = circle_centres$easting[i],
          northing_centre = circle_centres$northing[i],
          depth_centre = circle_centres$depth[i],
          easting = x_unwarped[, 1],
          northing = x_unwarped[, 2],
          depth = x_unwarped[, 3]
        )
      }, error = function(e) return(NULL))
    }) %>%
      bind_rows()
  }) %>%
    bind_rows() %>%
    mutate(name = name)
}) %>%
  bind_rows()

circle_plot <- function(k, x_label, x_var, x_point_var) {
  circles_df_k <- circle_df %>% filter(perspective == k)

  ggplot() +
    geom_path(
      data = circles_df_k,
      mapping = aes({{ x_var }}, depth, group = index, colour = factor(index)),
      linewidth = 0.2
    ) +
    geom_point(
      data = circles_df_k %>% distinct(name, index, {{ x_point_var }}, depth_centre),
      mapping = aes({{ x_point_var }}, depth_centre, colour = factor(index)),
      size = 0.1
    ) +
    guides(colour = 'none') +
    scale_x_continuous(
      breaks = c(-60, 0, 60),
      limits = c(
        min(c(circle_df$easting, circle_df$northing)),
        max(c(circle_df$easting, circle_df$northing))
      )
    ) +
    scale_y_reverse() +
    facet_grid(~ name) +
    theme(panel.grid.minor.x = element_line()) +
    labs(x = x_label, y = 'Depth [m]')
}

x_z_plot <- circle_plot(1, 'Easting [m]', easting, easting_centre)
y_z_plot <- circle_plot(2, 'Northing [m]', northing, easting_centre)

output <- wrap_plots(x_z_plot, y_z_plot, ncol = 1)

ggsave_fullwidth(args$output, output, height = 18)

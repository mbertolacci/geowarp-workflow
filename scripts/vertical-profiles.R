source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

parser <- ArgumentParser()
parser$add_argument('--fits', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

depth_grid <- seq(0, 41, length.out = 100)

calculate_profile <- function(fn) {
  lapply(
    args$fits,
    function(fit_path) {
      fit <- qs::qread(fit_path)
      name <- name_from_fit_path(fit_path)

      tibble(
        name = name,
        depth = depth_grid,
        value = fn(
          fit,
          data.frame(horizontal = 0, easting = 0, northing = 0, depth = depth_grid)
        )
      ) %>%
        filter(depth <= max(fit$observed_df$depth))
  }) %>%
    bind_rows() %>%
    mutate(name = factor(name, datasets))
}

mean_profiles <- calculate_profile(mean_profile)
warpings <- calculate_profile(function(...) {
  output <- warped_coordinates(...)
  output[, ncol(output)]
})
variances <- calculate_profile(marginal_variance_profile)

lighter <- function(x) colorspace::lighten(x, amount = 0.4)
darker <- function(x) colorspace::darken(x, amount = 0.4)

colour_scale <- scale_colour_manual(values = c(
  lighter('#1b9e77'), '#1b9e77', darker('#1b9e77'),
  lighter('#d95f02'), '#d95f02', darker('#d95f02'),
  '#1b9e77', '#d95f02'
))

linetype_scale <- scale_linetype_manual(values = c(
  1, 1, 1,
  1, 1, 1,
  2, 2
))

mean_profile_plot <- ggplot(mean_profiles, aes(depth, value, colour = name, linetype = name)) +
  geom_line() +
  colour_scale +
  linetype_scale +
  scale_x_reverse() +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(mu(h)), colour = NULL, linetype = NULL)

warping_plot <- ggplot(warpings, aes(depth, value, colour = name, linetype = name)) +
  geom_line() +
  colour_scale +
  linetype_scale +
  scale_x_reverse() +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(u[3]*'*'*''*(h)), colour = NULL, linetype = NULL)

variance_plot <- ggplot(variances, aes(depth, sqrt(value), colour = name, linetype = name)) +
  geom_line() +
  colour_scale +
  linetype_scale +
  scale_x_reverse() +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(sqrt(sigma[delta]^2*''*(h))), colour = NULL, linetype = NULL)

output <- wrap_plots(
  mean_profile_plot,
  variance_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  warping_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  nrow = 1,
  guides = 'collect'
)

ggsave_fullwidth(args$output, output, height = 7)

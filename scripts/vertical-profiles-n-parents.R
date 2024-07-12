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
      n_parents <- n_parents_from_fit_path(fit_path)

      tibble(
        name = name,
        n_parents = n_parents,
        depth = depth_grid,
        value = fn(
          fit,
          data.frame(horizontal = 0, easting = 0, northing = 0, depth = depth_grid)
        )
      ) %>%
        filter(depth <= max(fit$observed_df$depth))
  }) %>%
    bind_rows() %>%
    mutate(
      name = factor(name, datasets),
      n_parents = factor(n_parents)
    )
}

mean_profiles <- calculate_profile(mean_profile)
warpings <- calculate_profile(function(fit, ...) {
  fit$parameters$L_deviation <- diag(nrow(fit$parameters$L_deviation))
  output <- warped_coordinates(fit, ...)
  value <- output[, ncol(output)]
  value_diff <- diff(value) / diff(depth_grid)
  c(value_diff[1], value_diff)
})
variances <- calculate_profile(marginal_variance_profile)

colour_scale <- scale_colour_manual(
  values = c('red', '#377eb8', '#4daf4a')
)
linetype_scale <- scale_linetype_manual(
  values = c('solid', 'dashed', 'dotted')
)

mean_profile_plot <- ggplot(mean_profiles, aes(depth, value, colour = n_parents, linetype = n_parents)) +
  geom_line() +
  scale_x_reverse() +
  coord_flip() +
  facet_grid(name ~ .) +
  colour_scale +
  linetype_scale +
  labs(x = 'Depth [m]', y = expression(mu(h)), colour = '# Parents', linetype = '# Parents')

warping_plot <- ggplot(warpings, aes(depth, value, colour = n_parents, linetype = n_parents)) +
  geom_line() +
  scale_x_reverse() +
  coord_flip() +
  facet_grid(name ~ .) +
  colour_scale +
  linetype_scale +
  labs(x = NULL, y = expression(du[3]*'*'*''*(h)/dh), colour = '# Parents', linetype = '# Parents')

variance_plot <- ggplot(variances, aes(depth, sqrt(value), colour = n_parents, linetype = n_parents)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_log10(breaks = c(0.1, 0.3, 1)) +
  coord_flip() +
  facet_grid(name ~ .) +
  colour_scale +
  linetype_scale +
  labs(x = NULL, y = expression(sqrt(sigma[delta]^2*''*(h))), colour = '# Parents', linetype = '# Parents')

output <- wrap_plots(
  mean_profile_plot,
  variance_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  warping_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  nrow = 1,
  guides = 'collect'
) &
  theme(legend.position = 'bottom')

ggsave_fullwidth(args$output, output, height = 21)

source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--vertical-profiles')
parser$add_argument('--output')
args <- parser$parse_args()

vertical_profiles <- qs::qread(args$vertical_profiles)

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

mean_profile_plot <- ggplot(
  vertical_profiles$mean_profiles %>% filter(method == 'MCMC'),
  aes(depth, value, colour = name, linetype = name)
) +
  geom_line() +
  colour_scale +
  linetype_scale +
  scale_x_reverse() +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(mu(h)), colour = NULL, linetype = NULL)

stdev_plot <- ggplot(
  vertical_profiles$stdevs %>% filter(method == 'MCMC'),
  aes(depth, value, colour = name, linetype = name)
) +
  geom_line() +
  colour_scale +
  linetype_scale +
  scale_x_reverse() +
  scale_y_log10(breaks = c(0.1, 0.3, 1)) +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(sqrt(sigma[delta]^2*''*(h))), colour = NULL, linetype = NULL)

warping_plot <- ggplot(
  vertical_profiles$warpings %>% filter(method == 'MCMC'),
  aes(depth, value, colour = name, linetype = name)
) +
  geom_line() +
  colour_scale +
  linetype_scale +
  scale_x_reverse() +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(du[3]*'*'*''*(h)/dh), colour = NULL, linetype = NULL)

output <- wrap_plots(
  mean_profile_plot,
  stdev_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  warping_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  nrow = 1,
  guides = 'collect'
)

ggsave_fullwidth(args$output, output, height = 7)

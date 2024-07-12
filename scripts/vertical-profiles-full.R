source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--vertical-profiles')
parser$add_argument('--output')
args <- parser$parse_args()

vertical_profiles <- qs::qread(args$vertical_profiles)

make_profile_plot <- function(df) {
  ggplot(
    df %>%
      mutate(
        method_long = c(
          'MAP' = 'MAP',
          'MCMC' = 'Post. mean and 95% interval'
        )[method]
      ),
    aes(depth, colour = method_long, fill = method_long)
  ) +
    geom_ribbon(
      mapping = aes(ymin = value_q025, ymax = value_q975),
      alpha = 0.2,
      colour = NA
    ) +
    geom_line(mapping = aes(y = value)) +
    geom_line(mapping = aes(y = value_q025), linewidth = 0.2, linetype = 'dotted') +
    geom_line(mapping = aes(y = value_q975), linewidth = 0.2, linetype = 'dotted') +
    scale_x_reverse() +
    coord_flip() +
    facet_grid(name ~ .) +
    scale_colour_manual(values = c('black', 'red')) +
    scale_fill_manual(values = c('black', 'red')) +
    labs(colour = NULL, fill = NULL)
}

log_debug('Making plots')
mean_profile_plot <- make_profile_plot(vertical_profiles$mean_profiles) +
  labs(x = 'Depth [m]', y = expression(mu(h)))

stdev_plot <- make_profile_plot(vertical_profiles$stdevs) +
  scale_y_log10(breaks = c(0.1, 0.3, 1)) +
  labs(x = NULL, y = expression(sqrt(sigma[delta]^2*''*(h))))

warping_plot <- make_profile_plot(vertical_profiles$warpings) +
  labs(x = NULL, y = expression(du[3]*'*'*''*(h)/dh))

output <- wrap_plots(
  mean_profile_plot,
  stdev_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  warping_plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()),
  nrow = 1,
  guides = 'collect'
) &
  theme(legend.position = 'bottom')

log_debug('Saving plots to {args$output}')
ggsave_fullwidth(args$output, output, height = 20)

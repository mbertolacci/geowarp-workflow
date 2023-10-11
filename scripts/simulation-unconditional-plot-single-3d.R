source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--simulated-slices')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading fit from {args$fit}')
fit <- qs::qread(args$fit)

log_debug('Loading simulated slices from {args$simulated_slices}')
simulated_slices_df <- qs::qread(args$simulated_slices)

easting_grid <- sort(unique(simulated_slices_df$easting))
northing_grid <- sort(unique(simulated_slices_df$northing))

log_debug('Plotting')
output <- ggplot(
  bind_rows(
    fit$observed_df %>%
      mutate(type = 'Observed'),
    simulated_slices_df %>%
      filter(
        easting == easting_grid[floor(3 / 4 * length(easting_grid))],
        northing == midpoint(northing_grid)
      ) %>%
      mutate(
        name = 'Simulated',
        type = 'Simulated',
        log_q_c = log_q_c + rnorm(n(), sd = sqrt(fit$parameters$sigma_squared_nugget))
      )
  ),
  aes(depth, log_q_c, group = name, colour = type)
) +
  geom_line() +
  scale_colour_manual(values = c('#cccccc', '#6a3d9a')) +
  scale_x_reverse(limits = c(41, 0)) +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(log*' '*q[c]), colour = NULL) +
  theme(legend.position = 'bottom')

log_debug('Saving to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width / 2,
  height = 9
)

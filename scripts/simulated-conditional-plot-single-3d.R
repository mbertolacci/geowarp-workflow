source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--data')
parser$add_argument('--simulated-slices')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading data from {args$data}')
input_df <- readr::read_csv(args$data)

log_debug('Loading simulated slices from {args$simulated_slices}')
simulated_slices_df <- qs::qread(args$simulated_slices)

easting_grid <- sort(unique(simulated_slices_df$easting))
northing_grid <- sort(unique(simulated_slices_df$northing))
conditional_single_location <- conditional_single_locations[[
  stringr::str_match(basename(args$simulated_slices), '(.+)_(.+)\\.qs')[3]
]]

log_debug('Plotting')
output <- ggplot(
  bind_rows(
    input_df %>%
      mutate(type = 'Observed'),
    simulated_slices_df %>%
      filter(
        easting == easting_grid[floor(conditional_single_location[1] * length(easting_grid))],
        northing == northing_grid[floor(conditional_single_location[2] * length(northing_grid))]
      ) %>%
      mutate(
        name = 'Posterior sample',
        type = 'Posterior sample',
        log_q_c = log_q_c_sample + epsilon_sample
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
  width = DISPLAY_SETTINGS$full_width / 2 - 0.5,
  height = 9
)

source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--simulated-slices')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading simulated slices from {args$simulated_slices}')
simulated_slices_df <- qs::qread(args$simulated_slices)
easting_grid <- sort(unique(simulated_slices_df$easting))
northing_grid <- sort(unique(simulated_slices_df$northing))
northing_m1 <- tail(northing_grid, 2)[1]

simulated_grid_df <- expand.grid(
  easting = sort(unique(simulated_slices_df$easting)),
  northing = sort(unique(simulated_slices_df$northing)),
  depth = sort(unique(simulated_slices_df$depth))
) %>%
  left_join(
    bind_rows(
      simulated_slices_df,
      simulated_slices_df %>%
        filter(northing == max(northing)) %>%
        mutate(northing = northing_m1)
    ) %>%
      distinct(easting, northing, depth, .keep_all = TRUE),
    by = c('easting', 'northing', 'depth')
  )

log_debug('Saving to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
cairo_pdf(
  args$output,
  width = DISPLAY_SETTINGS$full_width * 0.575 / 2.54,
  height = 11 / 2.54
)
withr::with_par(list(
  mar = c(2, 0, 0, 0)
), {
  plot_slices(
    simulated_grid_df,
    'log_q_c',
    expression('Simulation of '*Y(bold(s), h)),
    palette = log_q_c_colours,
    limits = log_q_c_scale_limits
  )
  plot3D::segments3D(
    easting_grid[floor(3 / 4 * length(easting_grid))] - min(easting_grid),
    midpoint(northing_grid) - min(northing_grid),
    z0 = 0,
    z1 = 41,
    add = TRUE
  )
})
dev.off()

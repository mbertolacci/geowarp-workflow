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
simulated_slices_df <- qs::qread(args$simulated_slices) %>%
  mutate(log_q_c = log_q_c_samples[, 2])
easting_grid <- sort(unique(simulated_slices_df$easting))
northing_grid <- sort(unique(simulated_slices_df$northing))
northing_m1 <- tail(northing_grid, 2)[1]

conditional_single_location <- conditional_single_locations[[
  stringr::str_match(basename(args$fit), '(.+)_(.+)\\.qs')[3]
]]

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
    expression('Posterior sample of '*Y(bold(s), h)),
    palette = log_q_c_colours,
    limits = log_q_c_scale_limits
  )
  plot3D::segments3D(
    easting_grid[floor(conditional_single_location[1] * length(easting_grid))] - min(easting_grid) + 0.25,
    northing_grid[floor(conditional_single_location[2] * length(northing_grid))] - min(northing_grid),
    z0 = 0,
    z1 = 41,
    add = TRUE,
    col = '#6a3d9a'
  )
  observed_locations <- fit$observed_df %>% distinct(easting, northing)
  plot3D::points3D(
    observed_locations$easting - min(easting_grid),
    observed_locations$northing - min(northing_grid),
    rep(1.5, nrow(observed_locations)),
    pch = 8,
    col = 'black',
    add = TRUE
  )
})
dev.off()

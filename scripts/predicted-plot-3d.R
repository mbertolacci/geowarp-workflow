source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--variable')
parser$add_argument('--predicted-slices')
parser$add_argument('--predicted-slices2')
parser$add_argument('--output')
args <- parser$parse_args()

fit <- qs::qread(args$fit)
predicted_slices <- qs::qread(args$predicted_slices)

if (args$variable == 'log_q_c_mean_diff') {
  predicted_slices2 <- qs::qread(args$predicted_slices2)
  predicted_slices$log_q_c_mean_diff <- (
    predicted_slices$log_q_c_mean - predicted_slices2$log_q_c_mean
  )
} else if (args$variable == 'log_q_c_sd_ratio') {
  predicted_slices2 <- qs::qread(args$predicted_slices2)
  predicted_slices$log10_log_q_c_sd_ratio <- log10(
    predicted_slices$log_q_c_sd / predicted_slices2$log_q_c_sd
  )
}

easting_grid <- sort(unique(predicted_slices$easting))
northing_grid <- sort(unique(predicted_slices$northing))
depth_grid <- sort(unique(predicted_slices$depth))
northing_m1 <- tail(northing_grid, 2)[1]
predicted_grid <- expand.grid(
  easting = sort(unique(predicted_slices$easting)),
  northing = northing_grid,
  depth = depth_grid
) %>%
  left_join(
    bind_rows(
      predicted_slices,
      predicted_slices %>%
        filter(northing == max(northing)) %>%
        mutate(northing = northing_m1)
    ) %>%
      distinct(easting, northing, depth, .keep_all = TRUE),
    by = c('easting', 'northing', 'depth')
  )

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
cairo_pdf(
  args$output,
  width = DISPLAY_SETTINGS$full_width * 0.575 / 2.54,
  height = 11 / 2.54
)
withr::with_par(list(
  mar = c(2, 0, 0, 0)
), {
  if (args$variable == 'log_q_c_mean') {
    plot_slices(
      predicted_grid,
      'log_q_c_mean',
      expression('Posterior mean of '*Y(bold(s), h)),
      palette = log_q_c_colours,
      limits = log_q_c_scale_limits
    )
  } else if (args$variable == 'log_q_c_sd') {
    plot_slices(
      predicted_grid,
      'log_q_c_sd',
      expression('Posterior st. dev. of '*Y(bold(s), h)),
      palette = log_q_c_sd_colours,
      limits = log_q_c_sd_scale_limits
    )
  } else if (args$variable == 'log_q_c_mean_diff') {
    plot_slices(
      predicted_grid,
      'log_q_c_mean_diff',
      expression('MCMC '*Y(bold(s), h) - 'MAP '*Y(bold(s), h)),
      palette = log_q_c_diff_colours,
      limits = log_q_c_diff_scale_limits
    )
  } else if (args$variable == 'log_q_c_sd_ratio') {
    plot_slices(
      predicted_grid,
      'log10_log_q_c_sd_ratio',
      expression('MCMC st. dev. / MAP st. dev.'),
      palette = log_q_c_sd_ratio_colours,
      limits = log10(log_q_c_sd_ratio_scale_limits),
      breaks = log10(c(0.1, 1, 10)),
      labels = c('0.1', '1', '10')
    )
  }
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

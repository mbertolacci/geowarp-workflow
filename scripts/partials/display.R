library(ggplot2)
library(patchwork)

theme_set(theme_bw())
theme_replace(
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.text = element_text(colour = '#23373b'),
  axis.title = element_text(colour = '#23373b'),
  plot.margin = margin(t = 5.5, r = 5.5, b = 0, l = 0, unit = 'points')
)

DISPLAY_SETTINGS <- list(
  full_width = 16.5,
  supplement_full_width = 16.5,
  full_height = 20,
  png_plot_dpi = 320
)

ggsave_base <- function(filename, plot, bg = 'transparent', dpi = DISPLAY_SETTINGS$png_plot_dpi, ...) {
  ggsave(
    filename,
    plot,
    units = 'cm',
    dpi = dpi,
    bg = bg,
    ...
  )
}

ggsave_fullwidth <- function(...) {
  ggsave_base(..., width = DISPLAY_SETTINGS$full_width)
}

NAME_PALETTE <- c(
  '#a558c4',
  '#6cb94c',
  '#616ddb',
  '#c2ad48',
  '#d2479a',
  '#578540',
  '#ca85c5',
  '#4fb89b',
  '#d04144',
  '#5da1da',
  '#cf763a',
  '#6d6cb2',
  '#8b7134',
  '#a24662',
  '#e27e84'
)

three_breaks_div10 <- function(max_x, err_lower) {
  tick_width <- 10 * ceiling(max_x / 40)
  mismatch <- max_x - 4 * tick_width
  if (err_lower) {
    delta <- -10 * ceiling(abs(mismatch) / 20)
  } else {
    delta <- -10 * floor(abs(mismatch) / 20)
  }
  delta + (1 : 3) * tick_width
}

plot_slices <- function(df, name, label, palette, limits) {
  df$value <- df[[name]]
  if (missing(limits)) {
    limits <- range(df$value, na.rm = TRUE)
  }
  df$value <- pmin(
    pmax(df$value, limits[1] + sqrt(.Machine$double.eps)),
    limits[2] - sqrt(.Machine$double.eps)
  )
  perspective_mat <- with(
    df %>%
      arrange(-depth, northing, easting),
    plot3D::slice3D(
      sort(unique(easting)) - min(easting),
      sort(unique(northing)) - min(northing),
      sort(unique(depth)),
      array(
        value,
        dim = c(length(unique(easting)), length(unique(northing)), length(unique(depth)))
      ),
      xs = c(min(easting), midpoint(sort(unique(easting)))) - min(easting),
      ys = c(max(northing), midpoint(sort(unique(northing)))) - min(northing),
      phi = 30,
      col = palette,
      xlab = 'Easting',
      ylab = 'Northing',
      zlab = 'Depth',
      clab = label,
      axes = FALSE,
      clim = limits,
      colkey = list(
        side = 1,
        length = 0.7,
        width = 2.2,
        line.clab = 1,
        adj.clab = 0,
        font.clab = 1,
        font = 1,
        col.box = 'white'
      ),
      alpha = 0.8
    )
  )
  depth_ticks <- seq(30, 0, by = -10)
  depth_tick_pos <- trans3d(
    0,
    0,
    40 - depth_ticks,
    perspective_mat
  )
  segments(
    depth_tick_pos$x - 0.01,
    depth_tick_pos$y - 0.004,
    depth_tick_pos$x,
    depth_tick_pos$y,
    col = '#23373b'
  )
  text(
    depth_tick_pos$x - 0.015,
    depth_tick_pos$y - 0.01,
    labels = depth_ticks,
    adj = 1,
    col = '#23373b'
  )
  text(
    -0.495,
    -0.02,
    labels = 'Depth [m]',
    srt = -78,
    col = '#23373b'
  )

  easting_ticks <- three_breaks_div10(diff(range(df$easting)), FALSE)
  easting_tick_pos <- trans3d(
    easting_ticks,
    0,
    0,
    perspective_mat
  )
  segments(
    easting_tick_pos$x - 0.01,
    easting_tick_pos$y - 0.01,
    easting_tick_pos$x,
    easting_tick_pos$y,
    col = '#23373b'
  )
  text(
    easting_tick_pos$x - 0.015,
    easting_tick_pos$y - 0.02,
    labels = easting_ticks,
    adj = 1,
    col = '#23373b'
  )
  text(
    -0.302,
    -0.456,
    labels = 'Easting [m]',
    col = '#23373b'
  )

  northing_ticks <- three_breaks_div10(diff(range(df$northing)), TRUE)
  northing_tick_pos <- trans3d(
    diff(range(df$easting)),
    northing_ticks,
    0,
    perspective_mat
  )
  segments(
    northing_tick_pos$x + 0.01,
    northing_tick_pos$y - 0.007,
    northing_tick_pos$x,
    northing_tick_pos$y,
    col = '#23373b'
  )
  text(
    northing_tick_pos$x + 0.015,
    northing_tick_pos$y - 0.02,
    labels = northing_ticks,
    adj = 0,
    col = '#23373b'
  )
  text(
    0.32,
    -0.42,
    labels = 'Northing [m]',
    srt = 55,
    col = '#23373b'
  )
}

log_q_c_scale_limits <- c(-3.5, 2.75)
log_q_c_colours <- colorRampPalette(rev(scales::brewer_pal(palette = 'RdYlBu')(11)))(50)
log_q_c_sd_scale_limits <- c(0, 0.85)
log_q_c_sd_colours <- scales::colour_ramp(rev(scales::brewer_pal(palette = 'GnBu')(9)))(
  seq(0, 1, length.out = 100) ^ (1 / 1.8)
)

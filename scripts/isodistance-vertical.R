source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)
library(parallel)

if (Sys.getenv('GEOWARP_THREADS') != '') {
  options('mc.cores' = as.integer(Sys.getenv('GEOWARP_THREADS')))
}

parser <- ArgumentParser()
parser$add_argument('--mcmc-fits', nargs = '+')
parser$add_argument('--map-fits', nargs = '+')
parser$add_argument('--horizontal-coordinate')
parser$add_argument('--output')
args <- parser$parse_args()

stopifnot(args$horizontal_coordinate %in% c('easting', 'northing'))

mcmc_fits <- lapply(args$mcmc_fits, qs::qread)
map_fits <- lapply(args$map_fits, qs::qread)

n_mcmc_samples_to_use <- 1000L

circle_depth_grid <- seq(
  2,
  40,
  by = 2
)

circle_centres <- data.frame(
  easting = 0,
  northing = 0,
  depth = circle_depth_grid
) %>%
  mutate(index = seq_len(n()))

dimensions <- c(
  if (args$horizontal_coordinate == 'easting') 1 else 2,
  3
)

get_contours <- function(fit, max_depth, parameters = fit$parameters) {
  circle_centres_k <- circle_centres %>% filter(depth <= max_depth)

  lapply(seq_len(nrow(circle_centres_k)), function(i) {
    centre_i <- c(
      circle_centres_k$easting[i],
      circle_centres_k$northing[i],
      circle_centres_k$depth[i]
    )

    isocorrelation_contour(
      model = fit$model,
      parameters = parameters,
      centre = centre_i,
      dimensions = dimensions
    ) %>%
      mutate(
        index = i,
        easting_centre = centre_i[1],
        northing_centre = centre_i[2],
        depth_centre = centre_i[3]
      )
  }) %>%
    bind_rows()
}

map_circle_df <- lapply(seq_along(map_fits), function(k) {
  name <- name_from_fit_path(args$map_fits[k])
  log_debug('Processing {name} for MAP')

  fit <- map_fits[[k]]
  max_depth <- max(fit$observed_df$depth)

  get_contours(fit, max_depth) %>%
    mutate(name = name)
}) %>%
  bind_rows()

mcmc_circle_df <- lapply(seq_along(mcmc_fits), function(k) {
  log_debug('Processing {args$mcmc_fits[k]} for MCMC')
  name <- name_from_fit_path(args$mcmc_fits[k])

  fit <- mcmc_fits[[k]]

  max_depth <- max(fit$observed_df$depth)
  samples <- fit$samples
  n_samples <- nrow(samples$gamma_deviation_horizontal)
  iterations <- round(seq(1, n_samples, length.out = n_mcmc_samples_to_use))

  mclapply(iterations, function(iteration) {
    parameters <- list(
      gamma_deviation_horizontal = samples$gamma_deviation_horizontal[iteration, ],
      gamma_deviation_vertical = samples$gamma_deviation_vertical[iteration, ],
      L_deviation = samples$L_deviation[iteration, , ]
    )
    get_contours(fit, max_depth, parameters) %>%
      mutate(iteration = iteration)
  }) %>%
    bind_rows() %>%
    mutate(name = name)
}) %>%
  bind_rows()

grid_horizontal_lower <- head(seq(-60, 60, length.out = 31), -1)
grid_horizontal_step <- diff(grid_horizontal_lower)[1]
grid_horizontal_centre <- grid_horizontal_lower + grid_horizontal_step / 2

grid_vertical_lower <- head(seq(0, 42, length.out = 501), -1)
grid_vertical_step <- diff(grid_vertical_lower)[1]
grid_vertical_centre <- grid_vertical_lower + grid_vertical_step / 2

grid_base_df <- if (args$horizontal_coordinate == 'easting') {
  expand.grid(
    name = unique(mcmc_circle_df$name),
    easting_index = seq_along(grid_horizontal_centre),
    depth_index = seq_along(grid_vertical_centre),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      northing_index = findInterval(0, grid_horizontal_lower)
    )
} else {
  expand.grid(
    name = unique(mcmc_circle_df$name),
    northing_index = seq_along(grid_horizontal_centre),
    depth_index = seq_along(grid_vertical_centre),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      easting_index = findInterval(0, grid_horizontal_lower)
    )
}
grid_df <- grid_base_df %>%
  left_join(
    data.frame(
      name = mcmc_circle_df$name,
      iteration = mcmc_circle_df$iteration,
      easting_index = findInterval(mcmc_circle_df$easting, grid_horizontal_lower),
      northing_index = findInterval(mcmc_circle_df$northing, grid_horizontal_lower),
      depth_index = findInterval(mcmc_circle_df$depth, grid_vertical_lower)
    ) %>%
      group_by(name, iteration, easting_index, northing_index, depth_index) %>%
      summarise(n = 1, .groups = 'drop') %>%
      group_by(name, easting_index, northing_index, depth_index) %>%
      summarise(n = sum(n), .groups = 'drop'),
    by = c('name', 'easting_index', 'northing_index', 'depth_index')
  ) %>%
  mutate(
    easting = grid_horizontal_centre[easting_index],
    northing = grid_horizontal_centre[northing_index],
    depth = grid_vertical_centre[depth_index],
    n = ifelse(is.na(n), 0, n),
    p = n / n_mcmc_samples_to_use
  )


output <- ggplot() +
  geom_raster(
    data = grid_df,
    mapping = if (args$horizontal_coordinate == 'easting') {
      aes(easting, depth, fill = p)
    } else {
      aes(northing, depth, fill = p)
    }
  )

for (index_i in unique(map_circle_df$index)) {
  output <- output +
    geom_path(
      data = map_circle_df %>% filter(index == index_i),
      mapping = if (args$horizontal_coordinate == 'easting') {
        aes(easting, depth)
      } else {
        aes(northing, depth)
      },
      linewidth = 0.25,
      colour = 'red'
    )
}

output <- output +
  scale_fill_viridis_c(limits = c(0, 1)) +
  scale_x_continuous(
    breaks = c(-60, 0, 60),
    limits = c(-62, 62)
  ) +
  scale_y_reverse() +
  facet_grid(~ name) +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    x = if (args$horizontal_coordinate == 'easting') {
      'Easting [m]'
    } else {
      'Northing [m]'
    },
    y = 'Depth [m]',
    fill = 'Posterior probability'
  )

ggsave_fullwidth(args$output, output, height = 18.5)

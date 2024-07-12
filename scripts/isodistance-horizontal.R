source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)
library(parallel)

options(mc.cores = as.integer(Sys.getenv('GEOWARP_THREADS')))

parser <- ArgumentParser()
parser$add_argument('--mcmc-fits', nargs = '+')
parser$add_argument('--map-fits', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

mcmc_fits <- lapply(args$mcmc_fits, qs::qread)
map_fits <- lapply(args$map_fits, qs::qread)

n_mcmc_samples_to_use <- 1000L

get_contour <- function(fit, parameters = fit$parameters) {
  isocorrelation_contour(
    model = fit$model,
    parameters = parameters,
    centre = c(0, 0, 10),
    dimensions = c(1, 2)
  )
}

map_circle_df <- lapply(seq_along(map_fits), function(k) {
  name <- name_from_fit_path(args$map_fits[k])
  log_debug('Processing {name} for MAP')

  get_contour(map_fits[[k]]) %>%
    mutate(name = name)
}) %>%
  bind_rows()

mcmc_circle_df <- lapply(seq_along(mcmc_fits), function(k) {
  name <- name_from_fit_path(args$mcmc_fits[k])
  log_debug('Processing {name} for MCMC')

  fit <- mcmc_fits[[k]]

  samples <- fit$samples
  n_samples <- nrow(samples$gamma_deviation_horizontal)
  iterations <- round(seq(1, n_samples, length.out = n_mcmc_samples_to_use))

  mclapply(iterations, function(iteration) {
    parameters <- list(
      gamma_deviation_horizontal = samples$gamma_deviation_horizontal[iteration, ],
      gamma_deviation_vertical = samples$gamma_deviation_vertical[iteration, ],
      L_deviation = samples$L_deviation[iteration, , ]
    )
    get_contour(fit, parameters) %>%
      mutate(iteration = iteration)
  }) %>%
    bind_rows() %>%
    mutate(name = name)
}) %>%
  bind_rows()

grid_lower <- head(seq(-60, 60, length.out = 31), -1)
grid_centre <- grid_lower + diff(grid_lower)[1] / 2

all_names <- unique(c(map_circle_df$name, mcmc_circle_df$name))
parts <- lapply(all_names, function(name_i) {
  map_circle_df_i <- map_circle_df %>% filter(name == name_i)
  mcmc_circle_df_i <- mcmc_circle_df %>% filter(name == name_i)

  easting_indices <- findInterval(mcmc_circle_df_i$easting, grid_lower)
  northing_indices <- findInterval(mcmc_circle_df_i$northing, grid_lower)
  grid_df <- expand.grid(
    easting_index = seq_along(grid_centre),
    northing_index = seq_along(grid_centre)
  ) %>%
    left_join(
      data.frame(
        iteration = mcmc_circle_df_i$iteration,
        easting_index = easting_indices,
        northing_index = northing_indices
      ) %>%
        group_by(iteration, easting_index, northing_index) %>%
        summarise(n = 1, .groups = 'drop') %>%
        group_by(easting_index, northing_index) %>%
        summarise(n = sum(n), .groups = 'drop'),
      by = c('easting_index', 'northing_index')
    ) %>%
    mutate(
      easting = grid_centre[easting_index],
      northing = grid_centre[northing_index],
      n = ifelse(is.na(n), 0, n),
      p = n / n_mcmc_samples_to_use
    )

  ggplot() +
    geom_tile(
      data = grid_df,
      mapping = aes(easting, northing, fill = p),
      width = 1.05 * diff(grid_centre)[1],
      height = 1.05 * diff(grid_centre)[1]
    ) +
    geom_path(
      data = map_circle_df_i,
      aes(easting, northing),
      linewidth = 0.25,
      colour = 'red'
    ) +
    coord_fixed() +
    labs(
      x = 'Easting [m]',
      y = 'Northing [m]',
      fill = 'Posterior probability'
    ) +
    xlim(-62, 62) +
    ylim(-62, 62) +
    scale_fill_viridis_c(limits = c(0, 1)) +
    theme(legend.position = 'bottom') +
    ggtitle(name_i)
})

output <- wrap_plots(parts, guides = 'collect', ncol = 3) &
  theme(legend.position = 'bottom')

ggsave_fullwidth(args$output, output, height = 14)

source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(png)
library(grid)
library(gridExtra)

parser <- ArgumentParser()
parser$add_argument('--map-mean')
parser$add_argument('--map-sd')
parser$add_argument('--mcmc-mean')
parser$add_argument('--mcmc-sd')
parser$add_argument('--mean-diff')
parser$add_argument('--sd-ratio')
parser$add_argument('--output')
args <- parser$parse_args()

mean_map <- readPNG(args$map_mean)
mean_mcmc <- readPNG(args$mcmc_mean)
mean_diff <- readPNG(args$mean_diff)
stdev_map <- readPNG(args$map_sd)
stdev_mcmc <- readPNG(args$mcmc_sd)
stdev_ratio <- readPNG(args$sd_ratio)

plot_parts <- function(input) {
  list(
    panel = rasterGrob(input[1 : 980, , ], interpolate = TRUE),
    scales = rasterGrob(input[981 : dim(input)[1], , ], interpolate = TRUE)
  )
}

mean_map_parts <- plot_parts(mean_map)
mean_mcmc_parts <- plot_parts(mean_mcmc)
mean_diff_parts <- plot_parts(mean_diff)
stdev_map_parts <- plot_parts(stdev_map)
stdev_mcmc_parts <- plot_parts(stdev_mcmc)
stdev_ratio_parts <- plot_parts(stdev_ratio)


output <- arrangeGrob(
  textGrob('MCMC'),
  textGrob('MAP'),
  textGrob('Comparison'),
  mean_mcmc_parts$panel,
  mean_map_parts$panel,
  mean_diff_parts$panel,
  mean_map_parts$scales,
  mean_diff_parts$scales,
  stdev_mcmc_parts$panel,
  stdev_map_parts$panel,
  stdev_ratio_parts$panel,
  stdev_map_parts$scales,
  stdev_ratio_parts$scales,
  layout_matrix = rbind(
    c(1, 2, 3),
    c(4, 5, 6),
    c(7, 7, 8),
    c(9, 10, 11),
    c(12, 12, 13)
  ),
  heights = c(0.1, 1, 0.3225, 1, 0.3225)
)

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_fullwidth(
  args$output,
  output,
  dpi = 300,
  height = 13,
  bg = 'white'
)

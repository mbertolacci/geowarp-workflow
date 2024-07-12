source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--map-fit')
parser$add_argument('--mcmc-fit')
parser$add_argument('--output')
args <- parser$parse_args()

map_fit <- qs::qread(args$map_fit)
mcmc_fit <- qs::qread(args$mcmc_fit)

samples <- mcmc_fit$samples

# NOTE(mgnb): assumes we're at one of the sites with only one AWU
stopifnot(ncol(samples$gamma_deviation_horizontal) == 1)

n_samples <- length(samples$gamma_deviation_horizontal)
sample_df <- data.frame(
  index = seq_len(n_samples),
  value = as.vector(samples$gamma_deviation_horizontal)
)

output <- ggplot(sample_df, aes(value)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.25) +
  geom_vline(xintercept = map_fit$parameters$gamma_deviation_horizontal, color = 'red') +
  labs(
    x = expression(gamma[1*','*1]),
    y = 'Posterior density'
  )

ggsave_base(args$output, output, width = 9, height = 5)

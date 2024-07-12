source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(rstan)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading fit from {args$fit}')
fit <- qs::qread(args$fit)

log_debug('Plotting traceplot')
output <- traceplot(
  fit$stan_fit,
  include = FALSE,
  pars = 'alpha_beta_hat',
  ncol = 3
)

log_debug('Saving plot to {args$output}')
dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_fullwidth(
  args$output,
  output,
  height = 90
)

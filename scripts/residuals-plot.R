source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

parser <- ArgumentParser()
parser$add_argument('--fits', nargs = '+')
parser$add_argument('--alpha-beta-samples', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

residual_plots <- lapply(seq_along(args$fits), function(i) {
  fit_path_i <- args$fits[[i]]
  dataset_name <- name_from_fit_path(fit_path_i)

  log_debug('Processing {dataset_name}')

  fit_i <- qs::qread(fit_path_i)
  alpha_beta_samples_i <- qs::qread(args$alpha_beta_samples[[i]])

  stan_data <- geowarp_stan_data(fit_i$observed_df, fit_i$model)
  mean_profile_samples <- cbind(
    stan_data$X_mean_fixed,
    stan_data$X_mean_random
  ) %*% t(alpha_beta_samples_i)

  observed_df_i <- fit_i$observed_df
  observed_df_i$log_q_c_resid <- (
    observed_df_i$log_q_c
    - rowMeans(mean_profile_samples)
  )

  output <- ggplot() +
    geom_line(
      data = observed_df_i,
      mapping = aes(depth, log_q_c_resid, colour = short_name)
    )

  if (dataset_name == 'B2-T') {
    output <- output +
      annotate(
        'label',
        x = 30,
        y = 2.2,
        colour = NAME_PALETTE[4],
        size = 3,
        label = 'CPT 11'
      )
  }

  output +
    guides(colour = 'none') +
    scale_colour_manual(
      values = c('black', NAME_PALETTE)
    ) +
    labs(x = 'Depth [m]', y = expression('Residual of log '*q[c]), colour = NULL) +
    scale_x_reverse(limits = c(41, 0)) +
    ylim(-3.3, 3.3) +
    coord_flip() +
    ggtitle(dataset_name)
})

output <- wrap_plots(residual_plots, ncol = 3)

ggsave_fullwidth(args$output, output, height = 21)

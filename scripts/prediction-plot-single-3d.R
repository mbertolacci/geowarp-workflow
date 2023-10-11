source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

parser <- ArgumentParser()
parser$add_argument('--fit')
parser$add_argument('--predicted-single')
parser$add_argument('--output')
args <- parser$parse_args()

fit <- qs::qread(args$fit)
predicted_single <- qs::qread(args$predicted_single)

base_df <- data.frame(
  easting = 0,
  northing = 0,
  depth = sort(unique(predicted_single$depth))
)

output <- ggplot(
  bind_rows(
    predicted_single %>%
      mutate(name = as.integer(factor(easting))),
    base_df %>%
      mutate(
        name = 3,
        log_q_c_mean = mean_profile(fit, .)
      )
  ),
  aes(depth, colour = factor(name))
) +
  geom_line(mapping = aes(y = log_q_c_mean), linewidth = 0.8) +
  scale_x_reverse(limits = c(41, 0)) +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(log*' '*q[c]), colour = NULL) +
  theme(legend.position = 'bottom')

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width / 4,
  height = 9
)

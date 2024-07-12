source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading predictions from {args$input}')
predictions <- qs::qread(args$input)
n_cpts <- length(predictions)

log_debug('Calculating prediction intervals')
predicted_df <- lapply(predictions, function(prediction_i) {
  prediction_i$predicted_df %>%
    mutate(
      log_q_c_star_q025 = log_q_c_star - 1.96 * log_q_c_star_sd,
      log_q_c_star_q975 = log_q_c_star + 1.96 * log_q_c_star_sd,
    ) %>%
    filter(depth_has_input_data) %>%
    select(short_name, depth, log_q_c, log_q_c_star, log_q_c_star_q025, log_q_c_star_q975)
}) %>%
  bind_rows()

if (grepl('Jaksa', predictions[[1]]$name)) {
  lower_bound <- -1.8
  upper_bound <- 3.25
} else {
  lower_bound <- -5
  upper_bound <- 2.9
}

log_debug('Saving plots to {args$output}')
n_columns <- 4
output <- ggplot(predicted_df, aes(depth)) +
  geom_ribbon(
    mapping = aes(ymin = log_q_c_star_q025, ymax = log_q_c_star_q975),
    alpha = 0.2,
    fill = 'red'
  ) +
  geom_line(mapping = aes(y = log_q_c)) +
  geom_line(mapping = aes(y = log_q_c_star), colour = 'red') +
  geom_line(mapping = aes(y = log_q_c_star_q025), colour = 'red', linewidth = 0.2, linetype = 'dotted') +
  geom_line(mapping = aes(y = log_q_c_star_q975), colour = 'red', linewidth = 0.2, linetype = 'dotted') +
  scale_x_reverse() +
  ylim(lower_bound, upper_bound) +
  coord_flip() +
  labs(x = 'Depth [m]', y = expression(log*' '*q[c])) +
  facet_wrap(~ short_name, ncol = n_columns) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
  )

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_fullwidth(
  args$output,
  output,
  height = ceiling(n_cpts / n_columns) * 4.75 + 2
)

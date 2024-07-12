source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

set.seed(20230712)

parser <- ArgumentParser()
parser$add_argument('--output')
args <- parser$parse_args()

dummy_df <- data.frame(
  horizontal = rep(seq(0, 150, length.out = 5), each = 40),
  depth = rep(seq(0, 41, length.out = 40), 5)
)
ordering <- sample.int(nrow(dummy_df))
x_observed <- cbind(dummy_df$horizontal, dummy_df$depth)[ordering, ]

parents_nearest <- parent_knn(x_observed, 5)
parents_horizontal <- parent_knn_across_groups(
  x_observed[, 2],
  as.integer(factor(x_observed[, 1])),
  5
)

n <- nrow(x_observed)
parent_df <- lapply(
  c(
    floor(nrow(dummy_df) / 2),
    nrow(dummy_df)
  ), function(i) {
  tibble(
    index = i,
    horizontal = x_observed[, 1],
    depth = x_observed[, 2],
    status = factor(case_when(
      (1 : n) == i ~ 'Target',
      (1 : n) %in% parents_nearest[i, ] ~ 'Parent (nearest)',
      (1 : n) %in% parents_horizontal[i, ] ~ 'Parent (other horizontal)',
      (1 : n) < i ~ 'Predecessor',
      TRUE ~ 'Successor'
    ), c(
      'Successor',
      'Predecessor',
      'Parent (nearest)',
      'Parent (other horizontal)',
      'Target'
    ))
  )
}) %>%
  bind_rows() %>%
  arrange(status)

output <- ggplot(parent_df, aes(horizontal, depth, colour = status, size = status, shape = status)) +
  geom_point() +
  scale_colour_manual(values = c(
    'Target' = 'red',
    'Parent (nearest)' = '#377eb8',
    'Parent (other horizontal)' = '#4daf4a',
    'Predecessor' = '#aaaaaa',
    'Successor' = '#aaaaaa'
  )) +
  scale_shape_manual(values = c(
    'Target' = 8,
    'Parent (nearest)' = 18,
    'Parent (other horizontal)' = 18,
    'Predecessor' = 19,
    'Successor' = 1
  )) +
  scale_size_manual(values = c(
    'Target' = 3,
    'Parent (nearest)' = 3,
    'Parent (other horizontal)' = 3,
    'Predecessor' = 1,
    'Successor' = 1
  )) +
  facet_wrap(~ index) +
  scale_y_reverse() +
  labs(
    x = 'Horizontal [m]',
    y = 'Depth [m]',
    colour = NULL,
    shape = NULL,
    size = NULL
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

ggsave_fullwidth(args$output, output, height = 11)

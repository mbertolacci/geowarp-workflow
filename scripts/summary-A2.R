source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggrepel)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--output')
args <- parser$parse_args()

output_df <- readr::read_csv(args$input, show_col_types = FALSE)

map_df <- output_df %>%
  distinct(short_name, easting, northing)

eda_df <- output_df %>%
  group_by(depth = 0.1 * floor(depth / 0.1)) %>%
  summarise(
    log_q_c_mean = mean(log_q_c),
    log_q_c_sd = sd(log_q_c)
  )

plot_ylim <- range(c(
  output_df$log_q_c,
  eda_df$log_q_c_mean + 2 * eda_df$log_q_c_sd,
  eda_df$log_q_c_mean - 2 * eda_df$log_q_c_sd
))

label_df <- output_df %>%
  group_by(short_name) %>%
  summarise(
    final_depth = tail(depth, 1),
    final_log_q_c = tail(log_q_c, 1)
  ) %>%
  mutate(
    label_depth = final_depth + 3 * c(
      c(0, 0.5, 2) - 1, 1,
      -c(0, 1, 2, 3, 4),
      c(0, 1, 2, 3) - 1.5,
      1
    ),
    label_log_q_c = c(
      c(-1, -1.5, -1, 0.8),
      c(-0.7, -0.7, -0.7, -0.7, -0.7),
      c(0.3, 0.3, 0.3, 0.3),
      0.4
    ),
    label_hjust = c(
      c(1, 1, 1, 0),
      c(1, 1, 1, 1, 1),
      c(0, 0, 0, 0),
      0
    )
  )

cpts_to_show <- c(2, 4, 6)
cpts_to_show_names <- sprintf('%02d', cpts_to_show)

cpt_colours <- c('#fc8d62', '#8da0cb','#66c2a5')
map_colours <- rep('black', nrow(map_df))
map_colours[cpts_to_show] <- cpt_colours
map_shapes <- rep(16, nrow(map_df))
map_shapes[cpts_to_show] <- 17

map_plot <- ggplot(map_df, aes(easting, northing, colour = short_name)) +
  geom_point(
    mapping = aes(shape = short_name)
  ) +
  geom_text_repel(
    data = map_df %>% filter(short_name %in% cpts_to_show_names),
    mapping = aes(
      label = short_name
    ),
    segment.linetype = 'dotted',
    size = 5
  ) +
  guides(colour = 'none', shape = 'none') +
  scale_colour_manual(values = map_colours) +
  scale_shape_manual(values = map_shapes) +
  labs(x = 'Easting [m]', y = 'Northing [m]') +
  coord_fixed() +
  ggtitle('CPT locations')

data_plot <- ggplot() +
  geom_line(
    data = output_df %>%
      filter(short_name %in% cpts_to_show_names),
    mapping = aes(depth, log_q_c, colour = short_name),
    linewidth = 0.3
  ) +
  geom_segment(
    data = label_df %>%
      filter(short_name %in% cpts_to_show_names),
    mapping = aes(
      x = label_depth,
      xend = final_depth,
      y = label_log_q_c,
      yend = final_log_q_c,
      colour = short_name
    ),
    linetype = 'dashed'
  ) +
  geom_text(
    data = label_df %>%
      filter(short_name %in% cpts_to_show_names),
    mapping = aes(
      label_depth,
      label_log_q_c,
      label = sprintf(' %s ', short_name),
      colour = short_name,
      hjust = label_hjust
    ),
    size = 5
  ) +
  guides(colour = 'none') +
  scale_colour_manual(
    values = cpt_colours
  ) +
  ylim(plot_ylim) +
  labs(x = 'Depth [m]', y = expression('log '*q[c]), colour = NULL) +
  scale_x_reverse() +
  coord_flip() +
  ggtitle('Example CPTs')

mean_profile_plot <- ggplot(
  eda_df,
  aes(depth)
) +
  geom_line(mapping = aes(y = log_q_c_mean)) +
  geom_ribbon(
    mapping = aes(
      ymin = log_q_c_mean - 2 * log_q_c_sd,
      ymax = log_q_c_mean + 2 * log_q_c_sd
    ),
    linetype = 'solid',
    colour = '#cccccc',
    fill = 'black',
    alpha = 0.1
  ) +
  guides(colour = 'none') +
  ylim(plot_ylim) +
  labs(x = NULL, y = expression('log '*q[c]), colour = NULL) +
  scale_x_reverse() +
  coord_flip() +
  ggtitle(expression('Mean '%+-%' 2 st.dev.'))

output <- wrap_plots(
  map_plot,
  data_plot,
  mean_profile_plot,
  heights = c(4, 2),
  design = 'ABC\n#BC'
)

ggsave_fullwidth(args$output, output, height = 8)

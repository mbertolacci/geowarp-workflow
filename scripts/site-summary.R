source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggrepel)

parser <- ArgumentParser()
parser$add_argument('--input')
parser$add_argument('--output')
args <- parser$parse_args()

input_df <- readr::read_csv(args$input, show_col_types = FALSE)
name <- name_from_fit_path(args$input)

map_df <- input_df %>%
  distinct(short_name, easting, northing)

eda_df <- input_df %>%
  group_by(depth = 0.1 * floor(depth / 0.1)) %>%
  summarise(
    log_q_c_mean = ifelse(
      length(unique(short_name)) == 1,
      NA,
      mean(log_q_c)
    ),
    log_q_c_sd = sd(log_q_c)
  ) %>%
  filter(!is.na(log_q_c_mean))

cpts_to_show_names <- input_df %>%
  group_by(short_name) %>%
  summarise(max_depth = max(depth)) %>%
  arrange(max_depth) %>%
  filter(order(max_depth) %in% c(1, floor(n() / 2), n())) %>%
  pull(short_name) %>%
  sort()
cpts_to_show <- match(cpts_to_show_names, sort(unique(input_df$short_name)))

plot_xlim <- c(
  0,
  max(c(
    eda_df$depth,
    input_df %>% filter(short_name %in% cpts_to_show_names) %>% pull(depth)
  ))
)

plot_ylim <- range(c(
  input_df$log_q_c,
  eda_df$log_q_c_mean + 2 * eda_df$log_q_c_sd,
  eda_df$log_q_c_mean - 2 * eda_df$log_q_c_sd
))

label_df <- input_df %>%
  group_by(short_name) %>%
  summarise(
    final_depth = tail(depth, 1),
    final_log_q_c = tail(log_q_c, 1)
  )


pcpt_colours <- c('#fc8d62', '#8da0cb','#66c2a5')
map_colours <- rep('black', nrow(map_df))
map_colours[cpts_to_show] <- pcpt_colours
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
    data = input_df %>%
      filter(short_name %in% cpts_to_show_names),
    mapping = aes(depth, log_q_c, colour = short_name),
    linewidth = 0.3
  ) +
  geom_text_repel(
    data = label_df %>%
      filter(short_name %in% cpts_to_show_names),
    mapping = aes(
      final_depth,
      final_log_q_c,
      label = sprintf(' %s ', short_name),
      colour = short_name
    ),
    segment.linetype = 'dotted',
    size = 5
  ) +
  guides(colour = 'none') +
  scale_colour_manual(
    values = pcpt_colours
  ) +
  ylim(plot_ylim) +
  labs(x = 'Depth [m]', y = expression('log '*q[c]), colour = NULL) +
  scale_x_reverse(limits = rev(plot_xlim)) +
  coord_flip() +
  ggtitle('Example CPTs')

mean_profile_plot <- ggplot(
  eda_df,
  aes(depth)
) +
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
  geom_line(mapping = aes(y = log_q_c_mean)) +
  guides(colour = 'none') +
  ylim(plot_ylim) +
  labs(x = NULL, y = expression('log '*q[c]), colour = NULL) +
  scale_x_reverse(limits = rev(plot_xlim)) +
  coord_flip() +
  ggtitle(expression('Mean '%+-%' 2 st.dev.'))

output <- wrap_plots(
  map_plot,
  data_plot,
  mean_profile_plot,
  heights = c(4, 2),
  design = 'ABC\n#BC'
)

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
ggsave_fullwidth(args$output, output, height = 9)

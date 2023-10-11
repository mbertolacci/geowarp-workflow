source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--cv-metrics', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

metrics <- qs::qread(args$cv_metrics)

metric_order <- c('MSE', 'CRPS', 'Int05', 'DSS', 'DSS2')

input_data <- metrics %>%
  filter(
    metric %in% metric_order
  ) %>%
  mutate(
    metric = factor(metric, metric_order)
  )

all_plots <- lapply(metric_order, function(metric_i) {
  output <- ggplot() +
    geom_vline(xintercept = 6.5, linetype = 'dotted') +
    geom_vline(xintercept = 7.5, colour = '#999999') +
    geom_rect(
      data = data.frame(
        x = c(2, 4, 6, 8)
      ),
      mapping = if (startsWith(metric_i, 'DSS')) {
        aes(
          xmin = x - 0.5,
          xmax = x + 0.5,
          ymin = -Inf,
          ymax = Inf
        )
      } else {
        aes(
          xmin = x - 0.5,
          xmax = x + 0.5,
          ymin = 0,
          ymax = Inf
        )
      },
      fill = 'black',
      alpha = 0.1
    ) +
    geom_point(
      data = input_data %>% filter(metric == metric_i),
      mapping = aes(dataset, value, colour = model, shape = model),
      position = position_dodge(width = 0.8)
    ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0))) +
    scale_colour_manual(values = c(
      'GeoWarp' = '#e6ab02',
      'GW-CV' = '#7570b3',
      'GW-Vert-CV' = '#1b9e77',
      'Linear' = '#e7298a',
      'Binned' = '#d95f02',
      'BCS' = '#d95f02'
    ), drop = FALSE) +
    scale_shape_manual(values = c(
      'GeoWarp' = 16,
      'GW-CV' = 15,
      'GW-Vert-CV' = 17,
      'Linear' = 0,
      'Binned' = 1,
      'BCS' = 5
    ), drop = FALSE) +
    guides(
      colour = guide_legend(ncol = 3, override.aes = list(linetype = 0)),
      shape = guide_legend(ncol = 3, override.aes = list(linetype = 0))
    ) +
    labs(x = 'Dataset', y = metric_i, colour = '', shape = '') +
    theme(
      plot.title = element_text(size = 9, hjust = 0.5),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 8),
      panel.grid.major.x = element_blank()
    )
  if (!startsWith(metric_i, 'DSS')) {
    output <- output +
      annotation_logticks(sides = 'l', color = '#333333') +
      scale_y_log10()
  }
  output
})

output <- wrap_plots(
  c(
    head(all_plots, -1),
    list(
      all_plots[[length(all_plots)]] +
        theme(
          axis.title.x = element_text(),
          axis.text.x = element_text(angle = 15, hjust = 1)
        )
    )
  ),
  ncol = 1,
  guides = 'collect'
) &
  theme(legend.position = 'bottom')

ggsave_fullwidth(args$output, output, height = 13)

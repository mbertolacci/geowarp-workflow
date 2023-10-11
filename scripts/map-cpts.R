source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggrepel)

set.seed(202308315)

parser <- ArgumentParser()
parser$add_argument('--datasets', nargs = '+')
parser$add_argument('--height', type = 'integer')
parser$add_argument('--output')
args <- parser$parse_args()

datasets <- lapply(args$datasets, function(filename) {
  df <- readr::read_csv(filename, show_col_types = FALSE)
  name <- basename(filename) %>%
    stringr::str_remove('\\.csv$')
  list(name = name, df = df)
})

discretise_limits <- function(x, by) {
  c(floor(min(x) / by) * by, ceiling(max(x) / by) * by)
}

dataset_plots <- lapply(datasets, function(dataset) {
  label_df <- dataset$df %>%
    ungroup() %>%
    mutate(
      easting = easting - min(easting),
      northing = northing - min(northing)
    ) %>%
    distinct(short_name, easting, northing)

  output <- ggplot(label_df, aes(easting, northing, colour = short_name))

  if (dataset$name == 'A2') {
    output <- output +
      geom_polygon(
        data = data.frame(
          easting = c(25, 45, 235, 215),
          northing = c(190, 210, 5, -15)
        ),
        mapping = aes(easting, northing),
        inherit.aes = FALSE,
        fill = NA,
        colour = 'black',
        linetype = 'dashed'
      ) +
      annotate('text', x = 85, y = 200, label= 'A2-T')
  }

  if (dataset$name == 'B2') {
    output <- output +
      geom_polygon(
        data = data.frame(
          easting = c(119, 170, 190, 139),
          northing = c(-10, 145, 140, -17)
        ),
        mapping = aes(easting, northing),
        inherit.aes = FALSE,
        fill = NA,
        colour = 'black',
        linetype = 'dashed'
      ) +
      annotate('text', x = 210, y = 133, label= 'B2-T')
  }

  x_limits <- discretise_limits(label_df$easting, 50) + c(-25, 0)
  y_limits <- discretise_limits(label_df$northing, 50) + c(-25, 0)

  if (dataset$name == 'A2') {
    y_limits[2] <- y_limits[2] + 25
  }

  output <- output +
    geom_point() +
    geom_text_repel(
      mapping = aes(
        label = short_name
      ),
      segment.linetype = 'dotted',
      size = 3
    ) +
    guides(colour = 'none') +
    scale_colour_manual(
      values = NAME_PALETTE
    ) +
    scale_x_continuous(
      breaks = seq(0, 250, by = 50),
      limits = x_limits,
      expand = expansion()
    ) +
    scale_y_continuous(
      breaks = seq(0, 500, by = 50),
      limits = y_limits,
      expand = expansion()
    ) +
    labs(x = 'Easting [m]', y = 'Northing [m]') +
    coord_fixed() +
    ggtitle(dataset$name)

  list(
    plot = output,
    x_limits = x_limits,
    y_limits = y_limits
  )
})


widths <- sapply(dataset_plots, function(dataset_plot) {
  diff(dataset_plot$x_limits) / 25
})
heights <- sapply(dataset_plots, function(dataset_plot) {
  diff(dataset_plot$y_limits) / 25
})

output <- wrap_plots(
  lapply(dataset_plots, getElement, 'plot')
)
# HACK(mgnb): patchwork does a lot of work here to separate out the title
# and get the axes in a nice position, but it doesn't lay out the plots on
# an absolute grid, which is what we need.
output_gtable <- patchwork::patchworkGrob(output)

first_plot_tlbr <- list(
  t = 3,
  l = 4
)
first_plot_tlbr$b <- first_plot_tlbr$t + heights[1] - 1
first_plot_tlbr$r <- first_plot_tlbr$l + widths[1] - 1

second_plot_tlbr <- list(
  t = first_plot_tlbr$b + 4,
  l = 4
)
second_plot_tlbr$b <- second_plot_tlbr$t + heights[2] - 1
second_plot_tlbr$r <- second_plot_tlbr$l + widths[2] - 1

third_plot_tlbr <- list(
  t = 3,
  l = second_plot_tlbr$r + 4
)
third_plot_tlbr$b <- third_plot_tlbr$t + heights[3] - 1
third_plot_tlbr$r <- third_plot_tlbr$l + widths[3] - 1

cell_size <- 0.6
n_columns <- max(
  first_plot_tlbr$r,
  second_plot_tlbr$r,
  third_plot_tlbr$r
) + 2
n_rows <- max(
  first_plot_tlbr$b,
  second_plot_tlbr$b,
  third_plot_tlbr$b
) + 2
real_output <- gtable::gtable(
  widths = unit(rep(cell_size, n_columns), 'cm'),
  heights = unit(rep(cell_size, n_rows), 'cm')
)

real_output <- gtable::gtable_add_grob(
  real_output,
  output_gtable$grobs[[7]],
  t = first_plot_tlbr$t - 1L,
  l = first_plot_tlbr$l,
  b = first_plot_tlbr$t - 1L,
  r = first_plot_tlbr$r
)
real_output <- gtable::gtable_add_grob(
  real_output,
  output_gtable$grobs[[10]],
  t = first_plot_tlbr$t,
  l = first_plot_tlbr$l,
  b = first_plot_tlbr$b,
  r = first_plot_tlbr$r
)
real_output <- gtable::gtable_add_grob(
  real_output,
  output_gtable$grobs[[17]],
  t = second_plot_tlbr$t - 1L,
  l = second_plot_tlbr$l,
  b = second_plot_tlbr$t - 1L,
  r = second_plot_tlbr$r
)
real_output <- gtable::gtable_add_grob(
  real_output,
  output_gtable$grobs[[20]],
  t = second_plot_tlbr$t,
  l = second_plot_tlbr$l,
  b = second_plot_tlbr$b,
  r = second_plot_tlbr$r
)
real_output <- gtable::gtable_add_grob(
  real_output,
  output_gtable$grobs[[27]],
  t = third_plot_tlbr$t - 1L,
  l = third_plot_tlbr$l,
  b = third_plot_tlbr$t - 1L,
  r = third_plot_tlbr$r
)
real_output <- gtable::gtable_add_grob(
  real_output,
  output_gtable$grobs[[30]],
  t = third_plot_tlbr$t,
  l = third_plot_tlbr$l,
  b = third_plot_tlbr$b,
  r = third_plot_tlbr$r
)

ggsave_base(
  args$output,
  real_output,
  width = gtable::gtable_width(real_output),
  height = gtable::gtable_height(real_output)
)

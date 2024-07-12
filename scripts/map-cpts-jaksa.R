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

label_df <- input_df %>%
  ungroup() %>%
  mutate(
    easting = easting - min(easting),
    northing = northing - min(northing)
  ) %>%
  distinct(short_name, easting, northing)

output <- ggplot(label_df, aes(easting, northing, colour = short_name)) +
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
  labs(x = 'Easting [m]', y = 'Northing [m]') +
  coord_fixed() +
  ggtitle('Locations')

ggsave_base(
  args$output,
  output,
  width = 8,
  height = 8
)

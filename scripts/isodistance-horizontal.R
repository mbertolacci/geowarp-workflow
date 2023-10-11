source('scripts/partials/base.R')
source('scripts/partials/display.R')

library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(geowarp)

parser <- ArgumentParser()
parser$add_argument('--fits', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

fits <- lapply(args$fits, qs::qread)

circle_centre <- c(0, 0, 10)
radius <- uniroot(
  function(x) (1.0 + sqrt(3) * x) * exp(-sqrt(3) * x) - 0.5,
  c(0, 2)
)$root

circle_df <- lapply(seq_along(fits), function(k) {
  name <- name_from_fit_path(args$fits[k])
  fit <- fits[[k]]
  circle_centre_warped <- warped_coordinates(fit, data.frame(
    easting = circle_centre[1],
    northing = circle_centre[2],
    depth = circle_centre[3]
  ))

  theta <- seq(0, 2 * pi, length.out = 200)
  x_warped <- cbind(
    circle_centre_warped[1] + radius * cos(theta),
    circle_centre_warped[2] + radius * sin(theta),
    circle_centre_warped[3]
  )
  x_unwarped <- unwarped_coordinates(fit, x_warped)
  data.frame(
    name = name,
    easting = x_unwarped[, 1],
    northing = x_unwarped[, 2],
    depth = x_unwarped[, 3],
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows()

lighter <- function(x) colorspace::lighten(x, amount = 0.4)
darker <- function(x) colorspace::darken(x, amount = 0.4)

output <- ggplot(circle_df, aes(easting, northing, colour = name)) +
  geom_path(linewidth = 0.8) +
  coord_fixed() +
  scale_colour_manual(values = c(
    lighter('#1b9e77'), '#1b9e77', darker('#1b9e77'),
    lighter('#d95f02'), '#d95f02', darker('#d95f02')
  )) +
  labs(
    x = 'Easting [m]',
    y = 'Northing [m]',
    colour = NULL
  )

ggsave_fullwidth(args$output, output, height = 12)

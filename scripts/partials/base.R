library(logger)

if (Sys.getenv('GEOWARP_LOG_LEVEL') != '') {
  log_threshold(list(
    'trace' = TRACE,
    'debug' = DEBUG,
    'info' = INFO,
    'warn' = WARN,
    'error' = ERROR
  )[[Sys.getenv('GEOWARP_LOG_LEVEL')]])
}

if (Sys.getenv('GEOWARP_THREADS') != '') {
  options('geowarp.threads' = as.integer(Sys.getenv('GEOWARP_THREADS')))
} else {
  options('geowarp.threads' = -1L)
}

midpoint <- function(x) x[floor(length(x) / 2)]

datasets_3d <- c(
  'A1',
  'A2',
  'A3',
  'B1',
  'B2',
  'B3',
  'Jaksa'
)
datasets_2d <- c(
  'A2-T',
  'B2-T'
)
datasets <- c(datasets_3d, datasets_2d)
datasets_with_all <- c(datasets_3d, 'All', datasets_2d)

datasets_for_all <- c(
  'A1', 'A2', 'A3', 'B1', 'B2', 'B3'
)

models <- c(
  'GeoWarp',
  'GW-NoWarp',
  'GW-CV',
  'GW-NoWarp-CV',
  'GW-Vert-CV',
  'GW-WN-CV',
  'Linear',
  'Binned',
  'BCS'
)

model_from_fit_path <- function(fit_path) {
  stringr::str_match(basename(fit_path), '(.+)_(.+)\\.qs')[, 2]
}

name_from_fit_path <- function(fit_path) {
  stringr::str_match(basename(fit_path), '(.+)_(.+)\\.qs')[, 3]
}

n_parents_from_fit_path <- function(fit_path) {
  output <- 50L
  if (grepl('p25', fit_path)) {
    output <- 25L
  } else if (grepl('p100', fit_path)) {
    output <- 100L
  }
  output
}

conditional_single_locations <- list(
  A1 = c(0.80, 0.50),
  A2 = c(0.80, 0.50),
  A3 = c(0.50, 0.50),
  B1 = c(0.50, 0.15),
  B2 = c(0.50, 0.20),
  B3 = c(0.50, 0.50)
)

isocorrelation_contour <- function(
  model,
  parameters,
  centre,
  dimensions,
  target_distance = 0.9689941,
  distance_upper = 1000,
  n_theta = 500,
  max_iterations = 30
) {
  warp_df <- function(df) {
    warped_coordinates(
      parameters = parameters,
      model = model,
      df = df
    )
  }
  centre_warped <- warp_df(data.frame(
    easting = centre[1],
    northing = centre[2],
    depth = centre[3]
  ))

  includes_depth <- 3 %in% dimensions
  if (includes_depth) {
    depth_domain <- model$vertical_domain
    depth_mid <- mean(depth_domain)
    is_below_mid <- centre[3] < depth_mid
    # NOTE(mgnb): Space out the angles more towards the boundaries, to reach
    # out in horizontal directions more
    theta_base <- pi * qbeta(seq(0, 1, length.out = 500), 0.1, 0.1)
    theta <- if (is_below_mid) {
      pi + theta_base
    } else {
      theta_base
    }
  } else {
    theta <- seq(0, pi, length.out = n_theta)
  }
  distance_lower <- rep(0, length(theta))
  distance_upper <- rep(distance_upper, length(theta))

  get_unwarped <- function(theta, distance) {
    output <- data.frame(
      easting = rep(centre[1], length(theta)),
      northing = centre[2],
      depth = centre[3]
    )
    output[, dimensions[1]] <- output[, dimensions[1]] + distance * cos(theta)
    output[, dimensions[2]] <- output[, dimensions[2]] + distance * sin(theta)
    output
  }

  for (iteration in seq_len(max_iterations)) {
    distance_mid <- (distance_lower + distance_upper) / 2
    unwarped_df <- get_unwarped(theta, distance_mid)

    if (includes_depth) {
      outside_bounds <- (
        (unwarped_df$depth < model$vertical_domain[1])
        | (unwarped_df$depth > model$vertical_domain[2])
      )
      unwarped_df$depth <- pmax(pmin(unwarped_df$depth, depth_domain[2]), depth_domain[1])
    } else {
      outside_bounds <- rep(FALSE, nrow(unwarped_df))
    }

    x_warped_mid <- warp_df(unwarped_df)
    distance <- sqrt(
      (x_warped_mid[, 1] - centre_warped[1]) ^ 2
      + (x_warped_mid[, 2] - centre_warped[2]) ^ 2
      + (x_warped_mid[, 3] - centre_warped[3]) ^ 2
    )
    distance_lower <- ifelse(
      distance < target_distance,
      distance_mid,
      distance_lower
    )
    distance_upper <- ifelse(
      !outside_bounds & distance < target_distance,
      distance_upper,
      distance_mid
    )
    if (all(distance_upper - distance_lower < .Machine$double.eps ^ 0.25)) {
      break
    }
  }

  distance_mid <- (distance_lower + distance_upper) / 2

  if (includes_depth){
    theta_full <- if (is_below_mid) {
      c(theta - pi, theta)
    } else {
      c(theta, theta + pi)
    }
  } else {
    theta_full <- c(theta, theta + pi)
  }
  distance_mid_full <- c(distance_mid, distance_mid)

  get_unwarped(theta_full, distance_mid_full)
}

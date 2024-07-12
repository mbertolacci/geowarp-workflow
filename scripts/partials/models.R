library(geowarp)

shared_vertical_warping <- geowarp_bernstein_awu(
  order = 20,
  prior = list(type = 'gamma', shape = 1.01, rate = 0.01, lower = 0, upper = Inf)
)

get_horizontal_coordinates <- function(df) {
  if ('horizontal' %in% colnames(df)) {
    'horizontal'
  } else {
    c('easting', 'northing')
  }
}

get_horizontal_domains <- function(df) {
  if ('horizontal' %in% colnames(df)) {
    range(df$horizontal)
  } else {
    list(range(df$easting), range(df$northing))
  }
}

is_shared_awu_site <- function(df) {
  any(grepl('A3', df$name) | grepl('B3', df$name))
}

is_no_variance_boundary_knots_sites <- function(df) {
  any(grepl('Jaksa', df$name))
}

base_model_factory <- function(
  df,
  mean_model = geowarp_mean_model(
    fixed_formula = ~ depth,
    vertical_basis_functions = TRUE,
    vertical_basis_function_delta = 0.1
  ),
  ...
) {
  geowarp_model(
    variable = 'log_q_c',
    horizontal_coordinates = get_horizontal_coordinates(df),
    vertical_coordinate = 'depth',
    vertical_domain = c(0, 41),
    horizontal_domains = get_horizontal_domains(df),
    nugget_prior = inverse_gamma_quantile_prior(0.1, 1),
    mean_model = mean_model,
    ...
  )
}

base_geowarp_deviation_model_factory <- function(
  df,
  geometric_warping_unit = geowarp_geometric_warping_unit(prior_shape = 6),
  vertical_warping = shared_vertical_warping,
  ...
) {
  if (is_shared_awu_site(df)) {
    n_horizontal_awus <- 1L
    axial_warping_unit_mapping = c(1, 1, 2)
  } else {
    n_horizontal_awus <- length(get_horizontal_coordinates(df))
    axial_warping_unit_mapping = seq_len(n_horizontal_awus + 1L)
  }

  horizontal_scaling <- 0.1
  geowarp_deviation_model(
    axial_warping_units = c(
      rep(
        list(geowarp_linear_awu(
          scaling = horizontal_scaling,
          prior = list(
            type = 'inv_uniform',
            shape = 0,
            rate = 0,
            lower = 1 / (horizontal_scaling * 200),
            upper = 1 / (horizontal_scaling * 0.5)
          )
        )),
        n_horizontal_awus
      ),
      list(vertical_warping)
    ),
    axial_warping_unit_mapping = axial_warping_unit_mapping,
    geometric_warping_unit = geometric_warping_unit,
    ...
  )
}

base_geowarp_variance_model <- function(
  df,
  vertical_basis_functions,
  ...
) {
  if (vertical_basis_functions) {
    geowarp_variance_model(
      vertical_basis_functions = TRUE,
      vertical_basis_function_delta = 1,
      vertical_basis_function_length_scale_prior = list(scale = 1),
      vertical_basis_function_boundary_knots = if (
        is_no_variance_boundary_knots_sites(df)
      ) {
        0L
      } else {
        3L
      },
      ...
    )
  } else {
    geowarp_variance_model(
      vertical_basis_functions = FALSE,
      ...
    )
  }
}

model_factories <- list(
  'Linear' = function(df) {
    base_model_factory(
      df,
      mean_model = geowarp_mean_model(
        fixed_formula = ~ depth,
        vertical_basis_functions = FALSE
      ),
      deviation_model = geowarp_white_deviation_model(
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = FALSE
        )
      )
    )
  },
  'GW-WN-CV' = function(df) {
    base_model_factory(
      df,
      deviation_model = geowarp_white_deviation_model(
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = FALSE
        )
      )
    )
  },
  'GW-NoWarp-CV' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = FALSE
        ),
        vertical_warping = geowarp_linear_awu(prior = list(type = 'gamma', shape = 1.01, rate = 0.01, lower = 0, upper = 10000)),
        geometric_warping_unit = NULL
      )
    )
  },
  'GW-NoWarp' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = TRUE
        ),
        vertical_warping = geowarp_linear_awu(prior = list(type = 'gamma', shape = 1.01, rate = 0.01, lower = 0, upper = 10000)),
        geometric_warping_unit = NULL
      )
    )
  },
  'GW-Vert-CV' = function(df) {
    base_model_factory(
      df,
      deviation_model = geowarp_vertical_only_deviation_model(
        axial_warping_unit = shared_vertical_warping,
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = FALSE
        )
      )
    )
  },
  'GW-CV' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = FALSE
        )
      )
    )
  },
  'GeoWarp' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = TRUE
        )
      )
    )
  },
  'GeoWarp-bench' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = base_geowarp_variance_model(
          df,
          vertical_basis_functions = TRUE
        )
      )
    )
  }
)

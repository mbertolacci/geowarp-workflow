library(geowarp)

shared_homoskedastic_variance_model <- geowarp_variance_model(
  vertical_basis_functions = FALSE
)

vertical_warping <- geowarp_bernstein_awu(
  order = 20,
  prior = list(shape = 1.01, rate = 0.01)
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

base_geowarp_deviation_model_factory <- function(df, ...) {
  geowarp_deviation_model(
    axial_warping_units = c(
      rep(
        list(geowarp_linear_awu(
          scaling = 0.1,
          prior = inverse_gamma_quantile_prior(
            0.1 * 0.5, 0.1 * 100,
            0.01, 0.99
          )
        )),
        length(get_horizontal_coordinates(df))
      ),
      list(vertical_warping)
    ),
    geometric_warping_unit = geowarp_geometric_warping_unit(prior_shape = 2),
    ...
  )
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
        variance_model = shared_homoskedastic_variance_model
      )
    )
  },
  'GW-Vert-CV' = function(df) {
    base_model_factory(
      df,
      deviation_model = geowarp_vertical_only_deviation_model(
        axial_warping_unit = vertical_warping,
        variance_model = shared_homoskedastic_variance_model
      )
    )
  },
  'GW-CV' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = shared_homoskedastic_variance_model
      )
    )
  },
  'GeoWarp' = function(df) {
    base_model_factory(
      df,
      deviation_model = base_geowarp_deviation_model_factory(
        df,
        variance_model = geowarp_variance_model(
          vertical_basis_functions = TRUE,
          vertical_basis_function_delta = 1,
          vertical_basis_function_length_scale_prior = list(scale = 1)
        )
      )
    )
  }
)

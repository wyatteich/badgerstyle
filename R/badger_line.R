# Badger Line

# This function creates a line in Badger's signature style, complete with endpoints.
#'
#' @param data the data frame that the plot is based on
#' @param x the variable for the x axis
#' @param y the variable for the y axis
#' @param color_scale the column that defines the color scale
#' @param width the width of the desired lines

badger_line <- function(data, x, y, color_scale, width = 4) {
  endpoints <- dplyr::bind_rows(
    dplyr::filter(data, {{x}} == max({{x}})),
    dplyr::filter(data, {{x}} == min({{x}}))
  )

  lines <- levels(data[[color_scale]])

  line_functions <- list()

  for(i in 1:length(lines)) {
    line_functions <- list(
      geom_line(
        data = data,
        aes(x = {{x}}, y = {{y}}),
        linewidth = width,
        lineend = "round"
      ),

      geom_point(
        data = endpoints,
        aes(x = {{x}}, y = {{y}}),
        shape = 21,
        stroke = width,
        size = width * 0.925,
        fill = "white",
      )
    )
  }

  line_functions
}

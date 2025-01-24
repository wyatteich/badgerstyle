# Badger Line

# This function creates a line in Badger's signature style, complete with endpoints.
#'
#' @param data the data frame that the plot is based on
#' @param x the variable for the x axis
#' @param y the variable for the y axis
#' @param color the color of the line and endpoints

badger_line <- function(data, x, y, color) {

  endpoints <- dplyr::bind_rows(
    dplyr::filter(data, {{x}} == max({{x}})),
    dplyr::filter(data, {{x}} == min({{x}}))
  )

  list(
    geom_line(data = data,
              aes(x = {{x}}, y = {{y}}),
              color = color,
              linewidth = 4,
              lineend = "round"),
    geom_point(data = endpoints,
               aes(x = {{x}}, y = {{y}}),
               color = color,
               size= 8),
    geom_point(data = endpoints,
               aes(x = {{x}}, y = {{y}}),
               color = "white",
               size = 3)
  )


}


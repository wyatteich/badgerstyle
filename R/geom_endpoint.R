#' geom_endpoint
#'
#' This function adds endpoints a line in Badger's signature style
#'
#' @param data the data frame that the plot is based on
#' @param x the variable for the x axis
#' @param y the variable for the y axis
#' @param color the color of the endpoints


geom_endpoint <- function(data, x, y, color) {

  endpoints <- dplyr::bind_rows(
    dplyr::filter(data, {{x}} == max({{x}})),
    dplyr::filter(data, {{x}} == min({{x}}))
  )

  geom_point(data = endpoints,
             aes(x = {{x}}, y = {{y}}),
             color = color,
             fill = "white",
             shape = 21, size = 3.7, stroke = 4.2)

  # DEPRECATED

  # list(
  #   geom_point(data = endpoints,
               # aes(x = {{x}}, y = {{y}}),
               # color = color,
               # size= 8),
  #   geom_point(data = endpoints,
  #              aes(x = {{x}}, y = {{y}}),
  #              color = "white",
  #              size = 3)
  # )

}

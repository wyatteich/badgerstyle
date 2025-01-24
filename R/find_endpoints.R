#' find_endpoints
#'
#' Creates an endpoint for a specified function, using ggplot commands
#' @param data The dataframe that you will be drawing your endpoints from
#' @param x The variable along the x-axis.


find_endpoints <- function(data, x) {

  endpoints <- dplyr::bind_rows(
    dplyr::filter(data, {{x}} == max({{x}})),
    dplyr::filter(data, {{x}} == min({{x}}))
  ) #|>
    #dplyr::select({{x}}, {{y}})

  #p <- ggplot2::ggplot(endpoints, aes(x = x, y = y)) +
    #ggplot2::geom_point(size = 8) +
    #ggplot2::geom_point(size = 3, color = "white")

  return(endpoints)
}





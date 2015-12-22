
#' Bind two factors as an example
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
#'#' fbind(iris$Species[c(1, 51, 101)], PlantGrowth$group[c(1, 11, 21)])
fbind <- function(a, b) {
  factor(c(as.character(a), as.character(b)))
}


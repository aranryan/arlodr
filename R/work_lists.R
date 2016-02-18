

#' Create a named list
#'
#' Based on idea here
#' http://r.789695.n4.nabble.com/how-to-make-list-return-a-list-of-named-elements-td2720620.html
#'
#' I've used this to create a list of ts objects as part of working on the holiday regressors
#'
#' @param vector_of_variable_names
#'
#' @return
#' @export
#'
#' @examples
create_named_list <- function (vector_of_variable_names) {
  sapply(vector_of_variable_names, function(x) get(x), simplify = FALSE)
}

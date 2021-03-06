

#' Recoder
#'
#' Sets up a way to recode a variable, similar to a lookup table approach
#' copied from following link
#' http://susanejohnston.wordpress.com/2012/10/01/find-and-replace-in-
#' r-part-2-how-to-recode-many-values-simultaneously/
#' @param data
#' @param oldvalue
#' @param newvalue
#'
#' @return
#' @export
#'
#' @examples
recoder_func <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)

  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec
}

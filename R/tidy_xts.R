


#' put a tidy data frame with identfying columns
#' such as a geographic column and a date column into xts format
#'
#' x is a tidy dataframe with variables as columns
#' to_exclude a character vector of columns to exclude in the gather step
#' such as c("date", "area_sh")
#' creates a character vector of columns to exclude in the gather step
#' I ended up not going this route, and instead requiring the character
#' vector as an argument, but this is still a useful example of what's
#' possible
#' to_exclude <- c("date", interp(geo_code))
#'
#' based on example from:
#' http://stackoverflow.com/questions/28911496/exclusions-with-when-using-string-versions-underscore-suffix-such-as-gather
#'
#' when working with BLS data I had to add the following step
#' before read.zoo because I was getting an index error
#'    as.data.frame() %>%

#'
#' @param x
#' @param to_exclude
#'
#' @return
#' @export
#'
#' @examples
simp_xts <- function(x, to_exclude){

  y <- x %>%
    # following uses a standard evaluation, but then specificially
    # points out the columns to exclude using the select_vars_ step
    # that underlies tidyr
    gather_("var",
            "value",
            select_vars_(names(.),
                         names(.),
                         exclude = to_exclude))%>%
    mutate(vargeo = paste(var, area_sh, sep="_")) %>%
    select(date, vargeo, value) %>%
    spread(vargeo, value) %>%
    read.zoo(regular=TRUE) %>%
    xts()
}

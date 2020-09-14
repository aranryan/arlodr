

#' create an index of a quarterly xts or maybe zoo series
#'
#' @param x
#' @param index_year
#'
#' @return
#' @export
#'
#' @examples
index_q=function(x, index_year){
  start_index <- as.Date(paste(index_year,"-01-01",sep=""))
  end_index <- as.Date(paste(index_year,"-10-01",sep=""))
  temp_mean <- mean(window(x, start=start_index, end=end_index))
  x_index <- (x / temp_mean)*100
  return(x_index)
}

#' runs across a quarterly xts object to create an index of each series
#'
#' @param x
#' @param index_year
#'
#' @return
#' @export
#'
#' @examples
index_q_xts=function(x, index_year){
  start <- as.Date((start(x)))
  h <- zooreg(vapply(x, index_q, FUN.VALUE =
                       numeric(nrow(x)),
                     index_year=index_year))
  h <- zooreg(h, order.by=index(x))
  h <- xts(h)
  return(h)
}

#' create an index of a single quarterly series that is in a melted dataframe
#'
#' @param x
#' @param index_year
#'
#' @return
#' @export
#'
#' @examples
index_q_melted=function(x, index_year){
  x_index <- x %>%
    tidyr::spread(variable, value) %>%
    read.zoo(drop=FALSE) %>%
    xts()  %>%
    index_q(index_year=index_year) %>%
    data.frame() %>%
    as.matrix() %>%
    #reshape2::melt() %>%
    data.frame() %>%
    dplyr::mutate(date = as.Date(rownames(.))) %>%
    tidyr::gather(variable, value, -date)
  return(x_index)
}


#' create an index of a _monthly_ xts or maybe zoo series
#'
#' @param x
#' @param index_year
#'
#' @return
#' @export
#'
#' @examples
index_m=function(x, index_year){
  start_index <- as.Date(paste(index_year,"-01-01",sep=""))
  end_index <- as.Date(paste(index_year,"-12-01",sep=""))
  temp_mean <- mean(window(x, start=start_index, end=end_index))
  x_index <- (x / temp_mean)*100
  return(x_index)
}

#' create an index of a single monthly series that is in a melted dataframe
#'
#' @param x
#' @param index_year
#'
#' @return
#' @export
#'
#' @examples
index_m_melted=function(x, index_year){
  x_index <- x %>%
    tidyr::spread(variable, value) %>%
    read.zoo(drop=FALSE) %>%
    xts()  %>%
    index_m(index_year=index_year) %>%
    data.frame() %>%
    as.matrix() %>%
    #reshape2::melt() %>%
    data.frame() %>%
    dplyr::mutate(date = as.Date(rownames(.))) %>%
    tidyr::gather(variable, value, -date)
  return(x_index)
}

#' Apply the seasonal factors
#'
#' @param df Data frame in a tidy format with seasonal factors already joined on
#' @param date Name of the data column (used in organizing columns for output)
#' @param group Name of geography or other grouping, e.g., geoseg.
#'
#' @return
#' @export
#'
#' @examples m_us_1 <- raw_m_us %>%

seasonal_apply <- function(df, date, group){

  df_1 <- df %>%
    dplyr::mutate(df, supdsa = supd/supdsf,
           demdsa = demd/demdsf,
           occsa = occ/occsf,
           adrsa = adr/adrsf,
           revparsa = revpar/revparsf) %>%
    select({{ date }}, {{ group }}, supdsa, demdsa, occsa, adrsa, revparsa,
           supt, demt, rmrevt, supd, demd, occ, adr, revpar, strdays,
           supdsf, demdsf, occsf, adrsf, revparsf, everything())
}

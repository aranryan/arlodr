



#' run pooled and fixed effect regressions
#'
#' @param df
#' @param eq_id
#' @param start_date
#' @param end_date
#' @param gp_vec
#' @param y
#' @param x1
#' @param x2
#' @param x3
#' @param x4
#' @param x5
#' @param use_ar
#' @param use_c
#' @param drop_area_sh
#' @param select_out
#'
#' @return
#' @export
#'
#' @examples
run_pooled_r <- function(df, eq_id, start_date, end_date, gp_vec, y, x1,
                         x2, x3, x4, x5, use_ar, use_c, drop_area_sh, select_out){

  df_ready <- df
  # calculates a list of area_sh individuals to keep
  # by dropping those that we want to exclude
  a <- unique(df_ready$area_sh)
  keep_area_sh <- setdiff(a, drop_area_sh)

  if(x2 == "blank") x2 <- NULL
  if(x3 == "blank") x3 <- NULL
  if(x4 == "blank") x4 <- NULL
  if(x5 == "blank") x5 <- NULL

  # creates list of variables to gather for dataframe
  vars_needed <- c(y, x1, x2, x3, x4, x5)
  # write out formula
  spec <- NULL
  spec <- paste0(y," ~ ")
  if(length(x1) > 0) spec <- paste0(spec, x1)
  if(length(x2) > 0) spec <- paste0(spec, " + ", x2)
  if(length(x3) > 0) spec <- paste0(spec, " + ", x3)
  if(length(x4) > 0) spec <- paste0(spec, " + ", x4)
  if(length(x5) > 0) spec <- paste0(spec, " + ", x5)

  # add ar term if true
  if(use_ar) spec <- paste0(spec, " + lag(", y, ",1)")
  # remove constant term if false
  if(!use_c) spec <- paste(spec, " - 1")

  # convert spect to formula
  hold_spec_text <- as.character(spec)
  spec <- as.formula(spec)

  ####
  # differs from individual regressions in that I create a pdata frame with everything
  # create pdata.frame
  # so for me "individuals" are called "area_sh" and time is called "date"
  # skipped the step of filling in any missing quarters because I've already done it
  # in the initial set up of the data

  df_pdata <- df_ready %>%
    # filter to keep certain areas
    filter(area_sh %in% keep_area_sh) %>%
    #pdata.frame(., index = c("area_sh", "date"), drop.index = FALSE, row.names = TRUE)
    pdata.frame(., index = c(gp_vec, "date"), drop.index = FALSE, row.names = TRUE)

  # similar to eviews with "Fixed effects" for the intercept. So this uses separate
  # fixed effects estimate of intercept for each metro
  dem_fe <- plm(spec, data=df_pdata,model="within")
  #   summary(dem_fe)
  #   dem_fe$coefficients
  #   dem_fe$formula
  #   dem_fe$call

  tidy_coef <- tidy(dem_fe) %>%
    mutate(p.sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01,  "**",
                                 ifelse(p.value < 0.1, "*",
                                        ifelse(p.value < 0.2, "-",""))))) %>%
    mutate(eq_id = eq_id) %>%
    mutate(spec = hold_spec_text) %>%
    select(eq_id, everything())

  glance_eq <- glance(dem_fe) %>%
    mutate(p.sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01,  "**",
                                 ifelse(p.value < 0.1, "*",
                                        ifelse(p.value < 0.2, "-",""))))) %>%
    mutate_(spec = ~hold_spec_text) %>%
    mutate(eq_id = eq_id) %>%
    select(eq_id, everything())

  # this accesses the fixed effects for each market
  # the fixef function extracts the fixed effects from a plm object
  # these are the intercepts specific to each market, such as
  # when the plm model is estimated with model="within"
  # not sure what it would do if the fixed effects aren't there.
  # By setting type = level, we are getting the level of the intercept,
  # other choices would be in terms of deviation from the mean across markets.
  fixef <- fixef(dem_fe, type = 'level')
  a <- data.frame(fixef[1:length(fixef)])
  a$area_sh <- rownames(a)
  row.names(a)<-NULL
  colnames(a) <- c("estimate", "area_sh")
  fixef <- a %>%
    mutate(eq_id = eq_id) %>%
    mutate(term = "intercept") %>%
    mutate(spec = hold_spec_text) %>%
    select(eq_id, term, area_sh, estimate, spec)

  # select the output
  if (select_out == "tidy_coef") out <- tidy_coef
  if (select_out == "glance_eq") out <- glance_eq
  if (select_out == "fixef") out <- fixef
  out
}

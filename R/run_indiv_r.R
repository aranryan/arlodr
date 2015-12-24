

#' run individual regressions for each metro
#'
#' original part of regress script in fred project
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
run_indiv_r <- function(df, eq_id, start_date, end_date, gp_vec, y, x1,
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
  if(use_ar) spec <- paste0(spec, " + arterm")
  # remove constant term if false
  if(!use_c) spec <- paste0(spec, " - 1")

  # remove instances in which a blank x variable creates extra +
  # for testing
  #spec <- c("test +  + +")
  # steps to replace + + patterns
  # spec <- gsub("\\+\\s*\\+", " + ", spec)
  # spec <- gsub("\\+\\s*\\+", " + ", spec)
  # spec <- gsub("\\+\\s*\\+", " + ", spec)
  # spec <- gsub("\\+\\s*\\+", " + ", spec)


  # convert spect to formula
  hold_spec_text <- as.character(spec)
  print(hold_spec_text)
  spec <- as.formula(spec)

  # sets up text for later calculation of ar term
  arexpression <- paste0("lag(",y,")")

  regdf <- df_ready %>%
    filter(date >= start_date & date <= end_date) %>%
    # extracts the data we need
    select_(.dots = c("date", "area_sh", vars_needed)) %>%
    # drops usxxx from individuals
    filter(area_sh %in% keep_area_sh) %>%
    # filter(area_sh != "usxxx") %>%
    # filters for complete cases
    filter(complete.cases(.)) %>%
    # not sure why the following is needed, but it helped
    data.frame() %>%
    # gets ready to fill any missing quarters
    read.zoo(regular=TRUE, split=gp_vec)

  # fill in any missing quarters
  yrt2 <- range(time(regdf))
  y0 <- zoo(,seq(from = yrt2[1], to = yrt2[2], by = "quarter"))
  regdf1 <- merge(regdf, y0) %>%
    data.frame(date=time(.), .) %>%
    gather(variable, value, -date) %>%
    mutate(variable = as.character(variable)) %>%
    separate(variable, c("variable", "area_sh"), sep = "\\.", extra="merge") %>%
    spread(variable, value)

  # simple miami example
  #   temp_b <- regdf1 %>%
  #     filter(area_sh == "mimfl") %>%
  #     mutate(arterm = lag(dlogdemdsaupa))
  #   temp_reg <- lm(dlogdemdsaupa ~ dlogdemdsaupa_totus + -1, data = temp_b)
  #   summary(temp_reg)

  regdf2 <- regdf1 %>%
    # puts into order by the group vector and date.
    # date is important for lag calcs
    arrange_(.dots = c(gp_vec, "date")) %>%
    group_by_(gp_vec) %>%
    mutate_(.dots= setNames(list(arexpression), "arterm")) %>%
    # based on
    # http://stackoverflow.com/questions/26657938/how-to-make-lm-interpret-eval-in-formula
    do(fiteq = lm(spec, data = .))
  # do(fiteq = lm(as.formula(paste0(y1, " ~ ", x1, " + ", x2, sep="")), data = .))

  # get the coefficients by group in a tidy data_frame
  tidy_coef <- tidy(regdf2, fiteq) %>%
    mutate(p.sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01,  "**",
                                 ifelse(p.value < 0.1, "*",
                                        ifelse(p.value < 0.2, "-","")))))%>%
    mutate_(spec = ~hold_spec_text) %>%
    mutate(eq_id = eq_id) %>%
    select(eq_id, everything()) %>%
    ungroup() %>%
    arrange(term)
  # get the summary statistics by group in a tidy data_frame
  glance_eq <- glance(regdf2, fiteq) %>%
    mutate(p.sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01,  "**",
                                 ifelse(p.value < 0.1, "*",
                                        ifelse(p.value < 0.2, "-",""))))) %>%
    mutate_(spec = ~hold_spec_text) %>%
    mutate(eq_id = eq_id) %>%
    select(eq_id, area_sh, r.squared:p.value, p.sig, everything()) %>%
    ungroup()
  # do durbinwatson test on each equation and merge on
  dw_temp <-  lapply(1:length(regdf2$fiteq), function(x) durbinWatsonTest(eval(regdf2[[x,2]])))
  dw_temp <- do.call(rbind.data.frame, dw_temp) %>%
    rename(dw_p = p) %>%
    select(dw, dw_p)
  glance_eq <- glance_eq %>% cbind(., dw_temp) %>%
    select(-spec, everything())

  # select the output
  if (select_out == "tidy_coef") out <- tidy_coef
  if (select_out == "glance_eq") out <- glance_eq
  out
}

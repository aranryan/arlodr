library(usethis)

load("data-raw/holiday_regress.Rdata")
# unpack lists, removed mem_list,
a <- c(janend_list, val_list, eas_list, jlf_list, augend_list, sepstr_list,
       hlw_list, vet_list, thk_list, chr_list, han_list)
varNames <- names(a)
for (i in seq_along(varNames)) {
  print(varNames[i])
  hold_a <- a[[i]]
  # names the vector, this uses the first object as a name for the second object
  assign(paste(varNames[i], sep=""), hold_a)
}

holiday_reg <- cbind(eas_7_1_ts_m, val_fs_ts_m, augend_ss_ts_m,
                  sepstr_ss_ts_m, sepstr_mon_ts_m, jlf_fssm_ts_m, hlw_fss_ts_m,
                  thk_2826_ts_m, chr_fri_ts_m, hanwdayschr_m)

usethis::use_data(holiday_reg, internal = FALSE, overwrite=T)

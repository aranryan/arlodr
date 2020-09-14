
# names this as a code chunk so I can run it from knitr
## @knitr variablesXY

require("plm")
require("dynlm")
require("knitr")
require("xlsx")
require("tframe")
require("tframePlus")
require("lubridate")
require("stringr")
require("scales")
require("zoo")
require("xts")
if (!require(seasonal)) {
  install.packages("seasonal")
  require(seasonal)
}
Sys.setenv(X13_PATH = "C:/Aran Installed/x13ashtml")
#checkX13()
require("forecast")
require("car")
require("reshape2")
require("ggplot2")
require("tidyr")
require("dplyr")
require("lazyeval")
require("broom")
require("assertthat")
library("readxl")
require("readr")










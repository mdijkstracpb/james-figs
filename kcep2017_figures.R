rm(list = ls(all = T))
library(stringr)
source("james-light.R")
source("trivial.R")

j_init(file_name = "trivial.cpb")

if (0 == nrow(j_ls(collapsed = FALSE))) {
  source("james-figures-settings.R")
  source("load_kcep2017.R")
  j_save()
}

# Nu eerst figure
#figure_line(index = 1)
#!/usr/bin/env Rscript


library(devtools)

install_github("johnjosephhorton/JJHmisc")

install("../code/shaRing")



write.table(data.frame(date = date()), "shaRing_installed.txt")
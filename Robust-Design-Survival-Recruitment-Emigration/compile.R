
library(rmarkdown)
library(knitr)
library(pander)

setwd(file.path(Sys.getenv("USERPROFILE"),"Google Drive/Tutorials/Robust-Design-Survival-Only"))
render("robust.Rmd")

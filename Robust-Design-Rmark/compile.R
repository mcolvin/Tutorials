
library(rmarkdown)
library(knitr)
library(pander)

setwd(file.path(Sys.getenv("USERPROFILE"),"Google Drive/Tutorials/Robust-Design-Rmark"))
render("index.Rmd")

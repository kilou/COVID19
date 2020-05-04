# run local github shiny app
rm(list=ls())

reqpack <- function(X) {
  pak_failed <- which(!unlist(lapply(X, require, character.only = TRUE)))
  if(length(pak_failed)>=1){
    install.packages(X[pak_failed], repos = "https://stat.ethz.ch/CRAN/",dependencies = T)
    lapply(X[pak_failed], require, character.only = TRUE)
  }
}
pkg_list <- c("dplyr",
              "ggplot2",
              "readxl",
              "shiny",
              "shinybusy",
              "snowfall",
              "tippy",
              "rhandsontable",
              "plotly",
              "writexl")
reqpack(pkg_list)


#downloading the files
download.file(url = 'https://github.com/kilou/COVID19/archive/master.zip',destfile = 'covid_app.zip')

#unzip
unzip(zipfile = './covid_app.zip',overwrite = T)

#seting the working directory to the correct folder: (needed because app.r sources at the current wd)

setwd("./COVID19-master")
source("functions.r")
shiny::runApp("app.R")

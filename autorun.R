# run local github shiny app

#downloading the files
download.file(url = 'https://github.com/kilou/COVID19/archive/master.zip',destfile = 'covid_app.zip')

#unzip
unzip(zipfile = './covid_app.zip',overwrite = T)

#seting the working directory to the correct folder: (needed because app.r sources at the current wd)

setwd("./COVID19-master")
source("app.R")
shinyApp(ui, server)

# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()
golem::disable_autoload()

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

usethis::use_package('fontawesome')
usethis::use_package('subniche')
usethis::use_package('gbm')
usethis::use_package('factoextra')
usethis::use_package('shinyTree')
usethis::use_package('rintrojs')
usethis::use_package('cicerone')
usethis::use_package('oceanmap')
usethis::use_package("colourpicker")
usethis::use_package('sortable')
usethis::use_package('pdp')
usethis::use_package("geodist")
usethis::use_package("plot3D")
usethis::use_package('imputeMissings')
usethis::use_package("shinycssloaders")
usethis::use_package('rgl')
usethis::use_package('shinybusy')
usethis::use_package("Metrics")
usethis::use_package("shinyjqui")
usethis::use_package("colorspace")
usethis::use_package("writexl")
usethis::use_package('wesanderson')
usethis::use_package('e1071')
usethis::use_package('DBI')
usethis::use_package('shinydashboard')
usethis::use_package('shinydashboardPlus')
usethis::use_package('shinyjs')
usethis::use_package('shiny')
usethis::use_package('readxl')
usethis::use_package('vegan')
usethis::use_package('caret')
usethis::use_package('viridisLite')
usethis::use_package('aweSOM')
usethis::use_package('sp')
usethis::use_package('raster')
usethis::use_package('gstat')
usethis::use_package('ggplot2')
usethis::use_package('sf')
usethis::use_package('class')
usethis::use_package('shinyWidgets')
usethis::use_package('randomForestExplainer')
usethis::use_package('data.table')
usethis::use_package('purrr')
usethis::use_package('shinyBS')
usethis::use_package('ggpubr')
usethis::use_package("colorRamps")
usethis::use_package('kohonen')
usethis::use_package("segRDA")
usethis::use_package("gplots")
usethis::use_package('dendextend')
usethis::use_package("ggrepel")
usethis::use_package("beepr")
usethis::use_package('ggridges')
usethis::use_package('ggthemes')
usethis::use_package('klaR')

## Add one line by test you want to create
usethis::use_test("app")

# Documentation
3
## Vignette ----


library(golem)
add_rstudioconnect_file()
install.packages("here")
rstudioapi::navigateToFile("dev/03_deploy.R")
write("options(shiny.autoload.r=FALSE)", ".Rprofile", append = TRUE)


install.packages('goodpress')
remotes::install_github("maelle/goodpress", ref = "main")
vignette("setup", package = "goodpress")
library(goodpress)

usethis::use_vignette("imesc")
3
devtools::build_vignettes()
devtools::build_site()
rmarkdown::render_site("/R3/imesc/imesc/vignettes")
#https://rstudio.github.io/visual-markdown-editing/shortcuts.html

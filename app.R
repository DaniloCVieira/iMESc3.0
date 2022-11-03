# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
#renv::dependencies()$Package

options( "golem.app.prod" = TRUE)
remotes::install_deps(upgrade="never")
pkgload::load_all(export_all = FALSE,quiet =T,warn_conflicts =F)

imesc::run_app(options=list(quiet=T,shiny.autoload.r=FALSE))
# add parameters here (if any)
#shiny::runApp('.', host = '0.0.0.0', port = 80)
#tools::package_dependencies(packages ="imesc")
#pkgload::parse_deps(packages ="imesc")
#remove.packages("golem")

#packageVersion('rlang')
#library(processx)
#devtools::install_github('DaniloCVieira/iMESc2.1', upgrade='never', build =F, force=T)
#library(shiny)
#library(pkgload)
#shiny::runGitHub('iMESc2.1','DaniloCVieira')
#



#temp<-tempfile()
#out<-download.file(url = "https://github.com/DaniloCVieira/iMESc2.1/archive/master",paste0(temp,'.zip'))


# Launch the ShinyApp (Do not remove this comment)

list.of.packages <- c('remotes')
if(!length(grep("connect/apps",getwd()))>0){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {install.packages(new.packages, dependencies = TRUE)}
}
remotes::install_deps(upgrade="never")
pkgload::load_all(export_all = FALSE,quiet =T,warn_conflicts =F)
imesc::run_app(options=list(quiet=T,shiny.autoload.r=FALSE))

#shiny::runGitHub('iMESc','DaniloCVieira')

.First.lib <- function(libname, pkgname) {
  require(deSolve)
  require(lubridate)
}

.onLoad = function(libname, pkgname) {

library.dynam(pkgname, package=pkgname, lib.loc=.libPaths())
if (!require('devtools')) {install.packages('devtools')}
if (!require('verification')) {install.packages('verification')}
if (!require('leaflet')) {devtools::install_github('rstudio/leaflet')} 	
options(digits=10)
}
.onUnload = function(libpath) {
  pkgname <- "rAedesSim";
  library.dynam.unload(chname=pkgname, libpath=libpath)
}

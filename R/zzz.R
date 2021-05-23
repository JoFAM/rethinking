.onLoad <- function(libname, pkgname) { }

# This causes a bunch of errors when developing the package
# So I commented it out in order to be able to work on and
# rewrite all the manuals + provide the proper methods.
# .onAttach <- function(...) {
#   theLib <- dirname(system.file(package = "rethinking"))
#   pkgdesc <- utils::packageDescription("rethinking", lib.loc = theLib)
#   builddate <- gsub(';.*$', '', pkgdesc$Packaged)
#   msg <- paste("rethinking (Version ", pkgdesc$Version, ")", sep = "")
#   packageStartupMessage(msg)
# }

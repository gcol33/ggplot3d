.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  register_theme_3d()
}

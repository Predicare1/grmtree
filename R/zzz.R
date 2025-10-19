
.onLoad <- function(libname, pkgname) {
  # Register S3 methods explicitly to ensure they work even when
  # other packages mask base generics
  register_s3_method("print", "grmtree")
  register_s3_method("plot", "grmtree")
}

# Standalone S3 registration helper
register_s3_method <- function(generic, class, method = NULL) {
  if (is.null(method)) {
    method <- get(paste0(generic, ".", class), envir = parent.frame())
  }

  registerS3method(
    generic,
    class,
    method,
    envir = asNamespace(utils::packageName())
  )
}

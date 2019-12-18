# pointshapes3d private helper functions
#
.just_one <- function(x) {
  if (length(x != 1)) {
    nm <- deparse(substitute(x))
    if (length(x) > 1) {
      warning("Ignoring all but the first value of ", nm,
              call. = FALSE)
    } else if (length(x) == 0) {
      stop("Expected a value for ", nm)
    }
  }

  x[1]
}

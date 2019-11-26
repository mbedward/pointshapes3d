#' Generate random points on the surface of a cube
#'
#' @param n Number of points to generate.
#'
#' @param sidelen Cube side length. Default is 1.
#'
#' @param pos Either a numeric vector of three values giving the position on
#'   each axis, or a single value which will be used for all axes.
#'
#' @return A named list with elements:
#'   \code{coords}: A three column matrix of point coordinates;
#'   \code{npts}: Number of points;
#'   \code{sidelen}: Side length;
#'   \code{pos}: Vector of length three giving the position on each axis;
#'   \code{sides}: A vector of integers (values 0-5) identifying the side on
#'   which each point lies.
#'
#' @examples
#' # 1000 points on a unit cube at the origin
#' xcube <- p3d_cube(n = 1000)
#' head(xcube$coords)
#'
#' # 4000 points on a larger cube at x=2, y=-5, z=0
#' xcube <- p3d_cube(n = 4000, sidelen = 5, pos = c(2, -5, 0))
#'
#' \dontrun{
#' # If you have installed package 'threejs' you can view
#' # and rotate the cube points in a browser window
#' threejs::scatterplot3js(xcube$coords, size = 0.2)
#' }
#'
#' @export
#'
p3d_cube <- function(n, sidelen = 1, pos = c(0,0,0)) {
  n <- .just_one(n)
  sidelen <- .just_one(sidelen)

  if (length(pos) == 1) {
    pos <- rep(pos, 3)
  } else if (length(pos) != 3) {
    stop("pos should be a vector of three values, or a single value")
  }

  m <- matrix(0, nrow = n, ncol = 3)
  colnames(m) <- c("x", "y", "z")

  # Randomly select sides
  sides <- sample(0:5, size = n, replace = TRUE)

  # Axis perpendicular to each selected side
  axs <- sides %% 3

  ii <- matrix(c(1:n, 1+axs), ncol=2)
  m[ii] <- ifelse(sides > 2, 1, 0)

  # Second axis
  axs <- (axs + 1) %% 3
  ii[,2] <- 1 + axs
  m[ii] <- runif(n)

  # Third axis
  axs <- (axs + 1) %% 3
  ii[,2] <- 1 + axs
  m[ii] <- runif(n)

  m <- t(apply(m, MARGIN = 1, function(xyz) xyz * sidelen + pos))

  list(coords = m, npts = n, sidelen = sidelen, pos = pos, sides = sides)
}


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

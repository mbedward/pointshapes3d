#' Generate random points on the surface of a cube
#'
#' @param n Number of points to generate.
#'
#' @param sidelen Cube side length. Default is 1. Negative values are permitted.
#'
#' @param pos Either a numeric vector of three values giving the position on
#'   each axis, or a single value which will be used for all axes. Default is
#'   the origin: \code{c(0,0,0)}. If \code{sidelen} is a positive value,
#'   \code{pos} will be the minimum corner position on each axis.
#'
#' @return A named list with elements:
#'   \describe{
#'   \item{coords}{A three column matrix of point coordinates.}
#'   \item{npts}{Number of points.}
#'   \item{sidelen}{Side length.}
#'   \item{pos}{Vector of length three giving the position on each axis.}
#'   \item{sides}{A vector of integers (values 0-5) identifying the side on
#'     which each point lies.}
#'   }
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

  if (sidelen == 0) stop("sidelen must be a non-zero value")

  if (length(pos) == 1) {
    pos <- rep(pos, 3)
  } else if (length(pos) != 3) {
    stop("pos should be a vector of three values, or a single value")
  }

  m <- matrix(0, nrow = n, ncol = 3)
  colnames(m) <- c("x", "y", "z")

  # Randomly select sides
  sides <- sample(0:5, size = n, replace = TRUE)

  # Axis perpendicular to each side
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

  # Scale and position
  m <- t(apply(m, MARGIN = 1, function(xyz) xyz * sidelen + pos))

  list(coords = m, npts = n, sidelen = sidelen, pos = pos, sides = sides)
}


#' Generate random points on the surface of a sphere
#'
#' @param n Number of points to generate.
#'
#' @param radius Sphere radius. Default is 1. Must be positive.
#'
#' @param pos Either a numeric vector of three values giving the position on
#'   each axis, or a single value which will be used for all axes. Default is
#'   the origin: \code{c(0,0,0)}.
#'
#' @return A named list with elements:
#'   \describe{
#'   \item{coords}{A three column matrix of point coordinates.}
#'   \item{npts}{Number of points.}
#'   \item{radius}{Sphere radius.}
#'   \item{pos}{Vector of length three giving the position on each axis.}
#'   }
#'
#' @examples
#' # 1000 points on a unit sphere at the origin
#' xsphere <- p3d_sphere(n = 1000)
#' head(xsphere$coords)
#'
#' # 4000 points on a larger sphere at x=10, y=-20, z=0
#' xsphere <- p3d_sphere(n = 4000, radius = 5, pos = c(10, -20, 0))
#'
#' \dontrun{
#' # If you have installed package 'threejs' you can view
#' # and rotate the sphere points in a browser window
#' threejs::scatterplot3js(xsphere$coords, size = 0.2)
#' }
#'
#' @export
#'
p3d_sphere <- function(n, radius, pos = c(0,0,0)) {
  n <- .just_one(n)
  radius <- .just_one(radius)

  if (radius == 0) stop("radius must be a positive value")

  if (length(pos) == 1) {
    pos <- rep(pos, 3)
  } else if (length(pos) != 3) {
    stop("pos should be a vector of three values, or a single value")
  }

  # Matrix of Unif(0,1) values, guarding against zero rows
  nok <- 0
  m <- matrix(0, nrow = 0, ncol = 3)
  while (nok < n) {
    needed <- n - nok
    mtry <- matrix(rnorm(3*needed), nrow = needed, ncol = 3)

    zeros <- as.logical(apply(mtry, MARGIN = 1, all.equal, c(0,0,0)))
    zeros[is.na(zeros)] <- FALSE

    if (any(zeros)) {
      mtry <- mtry[!zeros, , drop = FALSE]
    }

    if (nrow(mtry) > 0) m <- rbind(m, mtry)
    nok <- nrow(m)
  }

  m <- apply(m, MARGIN = 1, function(p) {
    d <- sum(p^2)
    if (d > 0) {
      radius * p / sqrt(sum(p^2))
    }
  })

  m <- t(m)
  colnames(m) <- c("x", "y", "z")

  # Scale and position
  m <- t(apply(m, MARGIN = 1, function(xyz) xyz + pos))

  list(coords = m, npts = n, radius = radius, pos = pos)
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

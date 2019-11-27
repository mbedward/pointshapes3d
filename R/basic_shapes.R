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
p3d_sphere <- function(n, radius = 1, pos = c(0,0,0)) {
  n <- .just_one(n)
  radius <- .just_one(radius)

  if (radius <= 0) stop("radius must be a positive value")

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


#' Generate random points on the surface of a cylinder
#'
#' Presently this function only creates a cylinder aligned with one of the axes.
#' The next step is to allow the orientation of the cylinder to be varied more
#' flexibly.
#'
#' @param n Number of points to generate.
#'
#' @param radius Cylinder radius (must be positive). Default is 1.
#'
#' @param height Cylinder height (must be positive). Default is 1.
#'
#' @param pos Either a numeric vector of three values giving the position of the
#'   cylinder centroid on each axis, or a single value which will be used for
#'   all axes. Default is the origin: \code{c(0,0,0)}.
#'
#' @param align The axis along which the cylinder should be aligned, specified
#'   as either character (one of (\code{'x', 'y', 'z'}), case ignored) or an
#'   integer (one of \code{1, 2, 3}).
#'
#' @return A named list with elements:
#'   \describe{
#'   \item{coords}{A three column matrix of point coordinates.}
#'   \item{npts}{Number of points.}
#'   \item{radius}{Cylinder radius.}
#'   \item{height}{Cylinder height.}
#'   \item{pos}{Vector of length three giving the position on each axis.}
#'   }
#'
#' @examples
#' # 4000 points on a cylinder with radius=10 and height=20
#' # aligned with the X axis.
#' xcyl <- p3d_cylinder(n = 4000, radius = 10, height = 20, align = "x")
#' head(xcyl$coords)
#'
#' \dontrun{
#' # If you have installed package 'threejs' you can view
#' # and rotate the sphere points in a browser window
#' threejs::scatterplot3js(xcyl$coords, size = 0.2)
#' }
#'
#' @export
#'
p3d_cylinder <- function(n, radius, height, pos = c(0,0,0), align = "z") {
  n <- .just_one(n)
  radius <- .just_one(radius)
  height <- .just_one(height)
  align <- .just_one(align)

  if (radius <= 0) stop("radius must be a positive value")
  if (height <= 0) stop("height must be a positive value")

  if (length(pos) == 1) {
    pos <- rep(pos, 3)
  } else if (length(pos) != 3) {
    stop("pos should be a vector of three values, or a single value")
  }

  if (is.character(align)) {
    align <- match(tolower(align), c("x", "y", "z"))
  } else if (is.numeric(align)) {
    align <- match(align, 1:3)
  } else {
    align <- NA
  }
  if (is.na(align)) stop("align should be one of either 'x', 'y', 'z' or 1, 2, 3")

  theta <- runif(n, 0, 2*pi)
  h <- runif(n, 0, height)

  m <- cbind(
    radius * cos(theta),
    radius * sin(theta),
    h
  )

  cols <- c(setdiff(1:3, align), align)
  m[, cols] <- m
  colnames(m) <- c("x", "y", "z")

  list(coords = m, npts = n, radius = radius, height = height)
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

#' Rotate a point cloud
#'
#' @param x Either a list containing a 3-column matrix of point coordinates
#'   as a named element \code{'coords'}; or a matrix directly.
#'
#' @param theta A numeric vector giving the angle of rotation on the
#'   X, Y and Z axes respectively. If \code{length(theta) < 3} the implied
#'   missing values are set to zero; e.g. \code{theta = pi/4} would result
#'   in rotation around the X-axis but none around the Y or Z axes.
#'
#' @param origin The point about which to rotate the point cloud, specified as
#'   either a character string or a 3-element numeric vector of point
#'   coordinates. If a character string: \code{'centroid'} (default) means
#'   rotate around the centroid of the point cloud; \code{'origin'} means rotate
#'   around the origin.
#'
#' @return Rotated point coordinates as a three-column matrix.
#'
#' @export
#'
p3d_rotate <- function(x, theta = c(0, 0, 0), origin = "centroid") {
  if (is.list(x)) {
    if ("coords" %in% names(x)) coords <- x$coords
    else stop("Expected list to include a matrix as a named element 'coords'")

  } else if (is.matrix(x) && ncol(x) == 3) {
    coords <- x

  } else {
    stop("Expected either a list or 3-column matrix of point coordinates")
  }

  if (nrow(coords) == 0) {
    # nothing to do
    return(coords)
  }

  ntheta <- length(theta)
  if (ntheta < 3) {
    theta <- c(theta, rep(0, 3 - ntheta))
  }

  if (is.null(origin)) origin <- "centroid"

  if (is.character(origin)) {
    origin <- match.arg(tolower(origin), c("centroid", "origin"))
    if (origin == "origin") origin <- c(0,0,0)
    else origin <- colMeans(coords)

  } else if (is.numeric(origin) && length(origin) == 3) {
    # nothing to do

  } else {
    stop("Argument origin should be one of: NULL, 'origin', 'centroid' ",
         "or a numeric vector of length 3")
  }

  nonzero <- abs(theta) > 1e-6
  if (any(nonzero)) {
    R <- diag(3)

    if (nonzero[1]) R <- R %*% p3d_rx(theta[1])
    if (nonzero[2]) R <- R %*% p3d_ry(theta[2])
    if (nonzero[3]) R <- R %*% p3d_rz(theta[3])
  }

  zapsmall( t(apply(coords, 1, function(xyz) {
    origin + (R %*% (xyz - origin))
  })) )
}


#' Get matrix for rotation around the X axis
#'
#' @param theta Angle of rotation in radians
#'
#' @return 3x3 matrix rotation matrix
#'
#' @export
#'
p3d_rx <- function(theta) {
  theta <- .just_one(theta)

  matrix(c(1, 0,           0,
           0, cos(theta), -sin(theta),
           0, sin(theta),  cos(theta)),
         nrow = 3,
         byrow = TRUE)
}

#' Get matrix for rotation around the Y axis
#'
#' @param theta Angle of rotation in radians
#'
#' @return 3x3 matrix rotation matrix
#'
#' @export
#'
p3d_ry <- function(theta) {
  theta <- .just_one(theta)

  matrix(c( cos(theta), 0, sin(theta),
            0,          1, 0,
           -sin(theta), 0, cos(theta)),
         nrow = 3,
         byrow = TRUE)
}

#' Get matrix for rotation around the Z axis
#'
#' @param theta Angle of rotation in radians
#'
#' @return 3x3 matrix rotation matrix
#'
#' @export
#'
p3d_rz <- function(theta) {
  theta <- .just_one(theta)

  matrix(c(cos(theta), -sin(theta), 0,
           sin(theta),  cos(theta), 0,
           0,           0,          1),
         nrow = 3,
         byrow = TRUE)
}

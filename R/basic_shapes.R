#' Generate random points on the surface of a cube
#'
#' @param n Number of points to generate.
#'
#' @param lims Vector of min and max ordinates. The default (\code{c(0,1)})
#'   corresponds to a unit cube at the origin.
#'
#' @return A named list with elements:
#'   \code{coords}: A three column matrix of point coordinates;
#'   \code{sides}: A vector of integers (values 0-5) identifying the side on
#'   which each point lies.
#'
#' @examples
#' xcube <- p3d_cube(n = 1000)
#' head(xcube$coords)
#'
#' \dontrun{
#' # If you have installed package 'threejs' you can view
#' # and rotate the cube points in a browser window
#' threejs::scatterplot3js(xcube$coords, size = 0.2)
#' }
#'
#' @export
#'
p3d_cube <- function(n, lims = c(0, 1)) {
  m <- matrix(0, nrow = n, ncol = 3)
  colnames(m) <- c("x", "y", "z")

  lims <- sort(lims)
  sidelen <- diff(lims)

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

  list(coords = m * sidelen + lims[1],
       sides = sides)
}

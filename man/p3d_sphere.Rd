% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_shapes.R
\name{p3d_sphere}
\alias{p3d_sphere}
\title{Generate random points on the surface of a sphere}
\usage{
p3d_sphere(n, radius = 1, pos = c(0, 0, 0))
}
\arguments{
\item{n}{Number of points to generate.}

\item{radius}{Sphere radius. Default is 1. Must be positive.}

\item{pos}{Either a numeric vector of three values giving the position on
each axis, or a single value which will be used for all axes. Default is
the origin: \code{c(0,0,0)}.}
}
\value{
A named list with elements:
  \describe{
  \item{coords}{A three column matrix of point coordinates.}
  \item{npts}{Number of points.}
  \item{radius}{Sphere radius.}
  }
}
\description{
Generate random points on the surface of a sphere
}
\examples{
# 1000 points on a unit sphere at the origin
xsphere <- p3d_sphere(n = 1000)
head(xsphere$coords)

# 4000 points on a larger sphere at x=10, y=-20, z=0
xsphere <- p3d_sphere(n = 4000, radius = 5, pos = c(10, -20, 0))

\dontrun{
# If you have installed package 'threejs' you can view
# and rotate the sphere points in a browser window
threejs::scatterplot3js(xsphere$coords, size = 0.2)
}

}

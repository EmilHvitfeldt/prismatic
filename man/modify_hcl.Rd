% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.R
\name{modify_hcl}
\alias{modify_hcl}
\title{Modify individual HCL axes}
\source{
\url{https://en.wikipedia.org/wiki/HCL_color_space}
}
\usage{
modify_hcl(col, h, c, l)
}
\arguments{
\item{col}{A \code{colors} object (see \code{\link[=color]{color()}}) or a vector of any of the three
kinds of R color specifications, i.e., either a color name (as listed by
\code{\link[grDevices:colors]{grDevices::colors()}}), a hexadecimal string (see \code{\link[=col2rgb]{col2rgb()}}), or a
positive integer \code{i} meaning \code{\link[grDevices:palette]{grDevices::palette()}}\verb{[i]}.}

\item{h}{Expression to modify the hue of \code{col}.}

\item{c}{Expression to modify the chroma of \code{col}.}

\item{l}{Expression to modify the luminance of \code{col}.}
}
\value{
A \code{colors} object.
}
\description{
This function lets you modify individual axes of a color in HCL color space.
}
\details{
The expression used in \code{h}, \code{c}, and \code{l} is evaluated in the \code{hcl} space and
you have access to \code{h}, \code{c}, and \code{l} as vectors along with vectors in the
calling environment.

\code{h} ranges from 0 to 360, \code{l} ranges from 0 to 100, and \code{c} while dependent
on \code{h} and \code{l} will roughly be within 0 and 180, but often on a narrower
range. Colors after modification will be adjusted to fit within the color
space.
}
\examples{
plot(modify_hcl("red", h = 160))
plot(modify_hcl("red", h = h + 50))

plot(modify_hcl("red", h = h + 1:100))
plot(modify_hcl("red", c = c - 1:200))
plot(modify_hcl("red", l = l + 1:50))

plot(modify_hcl(rainbow(10), l = 25))

plot(modify_hcl(rainbow(10), h + h / 2, l = 70))
}

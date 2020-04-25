#' Estimate the Hellinger distance between two random variates
#'
#' @usage hellinger(x1, x2, nbreaks = 100, minx = min(c(x1, x2)),
#' maxx = max(c(x1, x2)))
#'
#' @param x1 A \code{numeric} random variate of draws from the first distribution
#' @param x2 A \code{numeric} random variate of draws from the second distribution
#' @param nbreaks A single \code{numeric} giving how many breaks to break the
#' discrete distribution into
#' @param minx A single \code{numeric} giving the lower bound of integration
#' @param maxx A single \code{numeric} giving the upper bound of integration
#'
#' @return A helldist object containing approximate Hellinger distances and
#'fitted density kernals.
#' \item{hdist_disc}{Estimate of Hellinger distance using discrete approximation
#' of the distributions}
#' \item{hdist_cont}{Estimate of Hellinger distance using continous approximation
#'of distributions}
#' @details Hellinger distance is approximated in two ways:
#'
#'(1) by binning the random variates and calculating the Hellinger distance
#'for discrete distributions and
#'
#' (2) by creating a continuous approximation of the distributions using
#'\code{density} and then using numerical integration to calculate the
#'Hellinger distance.
#'
#'Method (2) - continuous integration - should in genernal be more accurate
#'however, it may give poor approximations for multi-modal distributions.
#'
#'Continuous integration may return NaN if the distributions are near identical.
#' Class \code{helldist} has a plot method that can be used to compared the
#'discrete and continuous distribution fits.
#'
#'It is recommended to visually
#'check distribution fits, particularly if the number of random variates is
#'small.
#'
#' In general these methods will be inaccurate if analysis is performed on
#'too few samples, e.g. <10 000. >100 000 would be ideal.
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname hellinger
#' @export

hellinger <- function(x1, x2, nbreaks = 100, minx = min(c(x1, x2)), maxx = max(c(x1, x2))){

			dy <- (maxx-minx)/nbreaks
			breaks <- seq(minx, maxx, by = dy)
			x1d <- hist(x1, plot = F, breaks = breaks)
			x2d <- hist(x2, plot = F, breaks = breaks)

			hdist_disc <- (1/sqrt(2)) * sqrt(sum((sqrt(x1d$count/sum(x1d$count)) - sqrt(x2d$count/sum(x2d$count)))^2))

			e1 <- density(x1, from = minx, to = maxx)
			e2 <- density(x2, from = minx, to = maxx)

			 BCcoef <- sqrt((e1$y) *(e2$y))
			fx1 <- approxfun(e1$x, BCcoef, rule = 2)

			 hdist_cont <- sqrt(1 - integrate(fx1, minx, maxx)$value)

			hout <- list(hdist_disc = hdist_disc,
			hdist_cont = hdist_cont,
			histograms = list(h1 = x1d, h2 = x2d),
			densities = list(d1 = e1, d2 = e2))
			class(hout) <- "helldist"

		return(hout)

		}

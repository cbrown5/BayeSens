#' Estimate the Kullback-Leibler divergence between two random variates
#'
#' @Usage kldiv(x1, x2, nbreaks = 100, minx = min(c(x1, x2)),
#' maxx = max(c(x1, x2)))
#'
#' @param x1 A \code{numeric} random variate of draws from the posterior
#' distribution
#' @param x2 A \code{numeric} random variate of draws from the prior
#' distribution
#' @param nbreaks A single \code{numeric} giving how many breaks to break the
#' discrete distribution into
#' @param minx A single \code{numeric} giving the lower bound of integration
#' @param maxx A single \code{numeric} giving the upper bound of integration
#'
#' @return A helldist object containing approximate Hellinger distances and
#'fitted density kernals.
#' \item{hdist_disc}{Estimate of Hellinger distance using discrete approximation
#' of the distributions}
#' \item{hdist_cont}{Estimate of Hellinger distance using continous
#' approximation
#'of distributions}
#' @details  Kullback-Leibler divergence is approximated by
#' binning the random variates and calculating the KL-div
#'for discrete distributions.
#'
#'It is recommended to visually
#'check distribution fits, particularly if the number of random variates is
#'small.
#'
#' In general this methods will be inaccurate if analysis is performed on
#'too few samples, e.g. <10 000. >100 000 would be ideal.
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname kldiv
#' @export

kldiv <- function(x1, x2, nbreaks = 100, minx = min(c(x1, x2)),
maxx = max(c(x1, x2)), small = 0.01){

			dy <- (maxx-minx)/nbreaks
			breaks <- seq(minx, maxx, by = dy)
			x1d <- hist(x1, plot = F, breaks = breaks)
			x2d <- hist(x2, plot = F, breaks = breaks)
			x1p <- (x1d$counts + small) / sum(x1d$counts + small)
			x2p <- (x2d$counts + small) / sum(x2d$counts + small)
			vals <- x1p * log2(x1p/x2p)
			kd <- sum(na.omit(vals))
			
			hout <- list(kd = kd,
			histograms = list(h1 = x1d, h2 = x2d))
			
			class(hout) <- "kldiverg"

		return(hout)

		}

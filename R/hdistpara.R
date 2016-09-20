#' Parametric estimation of the Hellinger distance between two random variates
#'
#' @Usage hdistpara(x1, x2 = NULL, params = NULL, densfun = 'normal')
#'
#' @param x1 A \code{numeric} random variate of draws from the first
#' distribution
#' @param x2 An optional \code{numeric} random variate of draws from the second
#'  distribution.
#' @param params An optional \code{numeric} of two parameters giving the
#' parameters for the density function.
#' @param minx A \code{character} giving the density function to fit to both
#'  variates. Currently only "normal" and "beta" are supported.
#'
#' @return A helldistp object containing approximate Hellinger distances and
#'  fitted density kernals.
#' \item{hdist}{Estimate of Hellinger distance}
#'
#' @details Hellinger distance is approximated by fitting distributions using
#'  \code{MASS::fitdistr} and then calculating the exact Hellinger distance
#' given the fitted parameters. Currently the only options are to compare two Beta distributions or two normal distributions (the default).
#'
#' If \code{params} is given the second density function will be specified
#' exactly. If \code{x2} is given, the second density function will be estimate from the random variate.
#' If using \code{params} the parameters should be the mean and sd
#' (ie \code{c(mean, sd)}) in that order for 'normal' density and a and
#' b for the beta distribution (ie \code{c(a, b)}) in that order.
#' '
#' Class \item{helldistp} has a plot method that can be used to compared
#' the discrete and continuous distribution fits. It is recommended to
#' visually check distribution fits, particularly if the number of random
#' variates is small.
#' 
#' In general these methods will be inaccurate if analysis is performed on too
#' few samples, e.g. <10 000. >100 000 would be ideal.
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname hdistpara
#' @export

hdistpara <- function(x1, x2 = NULL, params = NULL, densfun = 'normal'){

			stopifnot(any(is.null(x2), is.null(params)))
			stopifnot(sum(is.null(x2) & is.null(params))<1)

			stopifnot(any(densfun %in% c('normal','beta')))

			if (densfun == 'normal'){

				if(length(params)>0){
					mu2 <- params[1]
					s2 <- params[2]
					x2 < NULL
					d2 <- NULL
					d2$estimate <- c(mu2, s2)
					} else {
					d2 <- MASS::fitdistr(x2, 'normal')
					mu2 <- as.numeric(d2$estimate[1])
					s2 <- as.numeric(d2$estimate[2])
				}

				d1 <- MASS::fitdistr(x1, 'normal')

				mu1 <- as.numeric(d1$estimate[1])
				s1 <- as.numeric(d1$estimate[2])

				hdist <- sqrt(1 -
				(sqrt((2*s1*s2)/(s1^2 + s2^2)) *
				exp(-0.25 * ((mu1 - mu2)^2/(s1^2 + s2^2)))
				)
				)

			} else if (densfun == 'beta'){

			stopifnot((c(x1, x2)< 1) & (c(x1, x2)> 0))

				if(length(params)>0){
					a2 <- params[1]
					b2 <- params[2]
					x2 < NULL
					d2 <- NULL
					d2$estimate <- c(a2, b2)
				} else {
					d2 <- MASS::fitdistr(x2, 'beta', start = list(shape1 = 1, shape2 = 1))
					a2 <- d2$estimate[1]
					b2 <- d2$estimate[2]
				}

				d1 <- MASS::fitdistr(x1, 'beta', start = list(shape1 = 1, shape2 = 1))


				a <- (d1$estimate[1] + a2)/2
				b <-  (d1$estimate[2] + b2)/2
				hdist <- as.numeric(sqrt(1 - (
					beta(a, b)/
					sqrt(beta(d1$estimate[1], d1$estimate[2]) *
					beta(a2, b2))
					)))

				}

			hout <- list(hdist = hdist,
			densities = list(d1 = d1, d2 = d2),
			distnam = densfun,
			vars = list(x1 = x1, x2 = x2))
			class(hout) <- "helldistp"

		return(hout)

		}

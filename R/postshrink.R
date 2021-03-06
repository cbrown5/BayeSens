
#' Estimate posterior shrink
#'
#' @Usage postshrink(thetaprior, thetapost, thetaMLE)
#'
#' @param thetaprior A \code{numeric} giving the estimate of a parameter
#'from the prior
#' @param thetapost A \code{numeric} giving the estimate of a parameter
#'from the posterior
#' @param thetaMLE A \code{numeric} giving the maximum likelihood estimate
#' of a parameter
#'
#' @return An estimate of posterior shrinkage.
#' @details Posterior shrinkage measures the degree to which the posterior
#'estimate has shrunk towards the maximum likelihood estimate and away from the
#'prior.
#'
#'Values close to 1 indicate the prior has little influence on the
#'posterior, whereas values close to 0 indicate the prior has a large
#'influence on the posterior.
#'
#' Some care must be taken in selecting parameter estimates to use in the
#'shrink equation and also in estimating the MLE (which is not always
#'straightfoward). For complex models the MLE may be estiamted using data
#'cloning, see \code{\link{dataclone}}.
#'
#' Shrink values can occaisionally be >1 or <0. Values <0 occur when
#'the posterior parameter estimate has moved in the opposite direction from
#'the prior than the MLE.
#' Values >1 occur when the MLE is closer to the prior estimate than
#' the posterior.
#' Values not in 0-1 can occur if (1) your MLE estimate
#'is inaccurate, (2) your posterior is multi-model, or (3) your posterior
#'estimate is constrained by other parameters. If (1) then try other methods
#'for obtaining an MLE or increase replication if using data cloning.
#'If (2) or (3) posterior shrink may be inappropriate for your model, because
#'the posterior shrink cannot be characterised by a simple univariate measure.
#'
#' See: Berger JO (1985) Statistical Decision Theory and Bayesian Analysis,
#' Second Edition, Springer, New York.
#'
#' Thanks to Ed Boone for suggesting this one.
#'
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname postshrink
#' @export

postshrink <- function(thetaprior, thetapost, thetaMLE){
		(thetapost - thetaprior)/(thetaMLE - thetaprior)
		}

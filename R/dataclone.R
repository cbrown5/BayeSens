#' Clone a dataframe
#'
#' @usage dataclone(dat, K)
#'
#' @param dat A \code{data.frame} of the data
#' @param K A \code{numeric} Giving the the number of clones to create.
#'
#' @return A cloned \code{data.frame}
#'
#' @details
#'Data cloning can be used to obtain maximum likelihood estimates
#'for parameters using Bayesian MCMC algorithms.
#'It may be useful for instance when estimating posterior shrinkage
#'with \code{\link{postshrink}} from the prior.
#'It is recommended that multiple values of K are run to find a large enough
#'value that gives stable esimates.
#'
#' See: Lele SR, Dennis B, Lutscher F. Data cloning: easy maximum likelihood
#'estimation for complex ecological models using Bayesian Markov chain Monte
#' Carlo methods. Ecology letters. 2007 Jul 1;10(7):551-63.
#'
#' @author Christopher J. Brown christo.j.brown@gmail.com
#' @rdname dataclone
#' @export

dataclone <- function(dat, K){
	do.call("rbind",lapply(1:K, function(i,x) x, dat))
		}

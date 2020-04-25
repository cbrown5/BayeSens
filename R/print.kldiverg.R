#' @export  

print.kldiverg <- function(kd){
		cat('Kullback-Leibler divergence', '\n')
		print(signif(kd$kd, 2))
		}

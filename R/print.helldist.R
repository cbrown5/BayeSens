#' @export  

print.helldist <- function(helldist){

		cat('Hellinger distance - continuous', '\n')
		print(signif(helldist$hdist_cont, 2))
		cat('\n', 'Hellinger distance - discrete', '\n')
		print(signif(helldist$hdist_disc, 2))
		
		}

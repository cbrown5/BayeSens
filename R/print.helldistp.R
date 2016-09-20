#' @export  

print.helldistp <- function(helldistp){
		cat('Hellinger distance for', helldistp$distnam,'distribution \n')
		print(signif(helldistp$hdist, 2))		
		}

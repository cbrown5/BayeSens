#' @export  

plot.helldist <- function(helldist){

		stopifnot(class(helldist) == 'helldist')
		ymax <- max(c(helldist$densities$d1$y, helldist$densities$d2$y, helldist$histograms$h1$density, helldist$histograms$h2$density))		
			
		par(mfrow = c(1,2))
		plot(helldist$densities$d1,ylim = c(0, ymax), type = 'l', main = 'Fit - var1')
		lines(helldist$histograms$h1$mids, helldist$histograms$h1$density, col = 'red')
		legend('topright', legend = c('continuous fit', 'discrete fit'), lty =1, col = c('black','red'))
		
		plot(helldist$densities$d2,ylim = c(0, ymax), type = 'l', main = 'Fit - var2')
		lines(helldist$histograms$h2$mids, helldist$histograms$h2$density, col = 'red')
				legend('topright', legend = c('continuous fit', 'discrete fit'), lty =1, col = c('black','red'))

					
		}

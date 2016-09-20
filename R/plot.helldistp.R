#' @export  

plot.helldistp <- function(helldist){

		nsteps <- 100
		xmin <- min(c(helldist$var$x1, helldist$var$x2))
		xmax <- max(c(helldist$var$x1, helldist$var$x2))		
		x <- seq(xmin, xmax, length.out = nsteps)
		
		h1 <- hist(helldist$var$x1, breaks = x, plot = F)
		
		if(!is.null(helldist$var$x2)){
			h2 <- hist(helldist$var$x2, breaks = x, plot = F)
		} else {h2 <- NULL}

		if(helldist$distnam == 'normal'){
			
			d1 <- dnorm(x, mean = helldist$densities$d1$estimate[1], 
			sd = helldist$densities$d1$estimate[2])
			
			d2 <- dnorm(x, mean = helldist$densities$d2$estimate[1], 
			sd = helldist$densities$d2$estimate[2])
					
				} else if (helldist$distnam == 'beta'){
			
			d1 <- dbeta(x, shape1 = helldist$densities$d1$estimate[1], 
			shape2 = helldist$densities$d1$estimate[2])
			
			d2 <- dbeta(x, shape1 = helldist$densities$d2$estimate[1], 
			shape2 = helldist$densities$d2$estimate[2])	
			}
			
			ymax <- max(c(d1, d2, h1$density, h2$density))		

			par(mfrow = c(1,2))
			plot(x, d1,ylim = c(0, ymax), type = 'l', main = 'Fit - var1')
			points(h1$mids, h1$density, col = 'red')
			legend('topright', legend = c('parametric fit', 'histogram'), lty =1, col = c('black','red'))
		
			plot(x, d2,ylim = c(0, ymax), type = 'l', main = 'Fit - var2')
			if(!is.null(helldist$var$x2)){
				points(h2$mids, h2$density, col = 'red')
				legend('topright', legend = c('parametric fit', 'histogram'), lty =1, col = c('black','red'))
			}
					
		}

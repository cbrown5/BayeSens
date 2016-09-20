## ------------------------------------------------------------------------
r1 <- rnorm(10000, 1, 1)
r2 <- rnorm(10000, 1, 3)

## ------------------------------------------------------------------------
library(BayeSens)
hout <- hellinger(r1, r2, nbreaks = 100)
hout

## ---- fig.width=7, fig.height=5------------------------------------------
plot(hout)

## ---- fig.width=7, fig.height=5------------------------------------------
houtp <- hdistpara(r1, r2, densfun = 'normal')

houtp
plot(houtp)

## ------------------------------------------------------------------------
postshrink(thetaprior = 0.1, thetapost = 0.2, thetaMLE = 0.22)


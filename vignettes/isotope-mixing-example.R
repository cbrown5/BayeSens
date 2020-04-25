## -----------------------------------------------------------------------------
library(BayeSens)
library(MixSIAR)

## -----------------------------------------------------------------------------
mix.filename <- system.file("extdata", "killerwhale_consumer.csv", package = "MixSIAR")

mix <- load_mix_data(filename=mix.filename,
                     iso_names=c("d13C","d15N"),
                     factors=NULL,
                     fac_random=NULL,
                     fac_nested=NULL,
                     cont_effects=NULL)


source.filename <- system.file("extdata", "killerwhale_sources.csv", package = "MixSIAR")

source <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix)

discr.filename <- system.file("extdata", "killerwhale_discrimination.csv", package = "MixSIAR")

discr <- load_discr_data(filename=discr.filename, mix)

## -----------------------------------------------------------------------------
alpha <- rep(1, source$n.sources) #default prior values
p_prior <- MCMCpack::rdirichlet(10000, alpha) #draw prior samples 

## ----fig.width=8--------------------------------------------------------------
#Plot histogram and density (same data, different ways to view it )
par(mfrow = c(1,2))
hist(p_prior[,1], 20, main = source$source_names[1])
plot(density(p_prior[,1]), main = source$source_names[1])
abline(v = 1/source$n.sources)

## -----------------------------------------------------------------------------
model_filename <- "MixSIAR_model_kw_uninf.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)
jags.uninf <- run_model(run="test",mix,source,discr,model_filename,alpha.prior = alpha, resid_err, process_err)

## -----------------------------------------------------------------------------
p_post <- jags.uninf$BUGSoutput$sims.list$p.global

## -----------------------------------------------------------------------------
hellinger(p_prior[,1], p_post[,1])
kldiv(p_prior[,1], p_post[,1])

## -----------------------------------------------------------------------------
hell_out <- lapply(1:source$n.sources, function(i) hellinger(p_prior[,i], p_post[,i])$hdist_disc)
kl_out <- lapply(1:source$n.sources, function(i) kldiv(p_prior[,i], p_post[,i])$kd)
info_df <- data.frame(source_names = source$source_names, 
                      hellinger = unlist(hell_out),
                      KLD = unlist(kl_out))
info_df

## ----fig.width=8--------------------------------------------------------------
par(mfrow = c(1,2))
plot(density(p_post[,1]), main = source$source_names[1])
lines(density(p_prior[,1]), col = "red")
abline(v = 1/source$n.sources, lty = 2)

plot(density(p_post[,3]), main = source$source_names[3])
lines(density(p_prior[,3]), col = "red")
abline(v = 1/source$n.sources, lty = 2)

## -----------------------------------------------------------------------------
kw.alpha <- c(10,1,0,0,3)
kw.alpha <- kw.alpha*length(kw.alpha)/sum(kw.alpha)
kw.alpha[which(kw.alpha==0)] <- 0.01
model_filename <- "MixSIAR_model_kw_inf.txt"  
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)
jags.inf <- run_model(run="test",mix,source,discr,model_filename,alpha.prior=kw.alpha, resid_err, process_err)

## -----------------------------------------------------------------------------
p_prior_inf <- MCMCpack::rdirichlet(10000, kw.alpha) #draw prior samples 
p_post_inf <- jags.inf$BUGSoutput$sims.list$p.global

## -----------------------------------------------------------------------------
hell_out_inf <- lapply(1:source$n.sources, function(i) hellinger(p_prior_inf[,i], p_post_inf[,i])$hdist_disc)
kl_out_inf <- lapply(1:source$n.sources, function(i) kldiv(p_prior_inf[,i], p_post_inf[,i])$kd)
info_df <- cbind(info_df, 
                 data.frame(
                      hellinger_inf = unlist(hell_out_inf),
                      KLD_inf = unlist(kl_out_inf)))
info_df

## ----fig.width=8--------------------------------------------------------------

plot(density(p_post_inf[,1]), lty = 2, main = "informed prior", xlim = c(0,1))
lines(density(p_prior_inf[,1]), lty = 2, col = "red")



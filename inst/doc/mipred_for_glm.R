## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(mipred)

## ------------------------------------------------------------------------
data(cll)
head(cll)

## ------------------------------------------------------------------------
cll_bin <- cll # Generate a new data copy
cll_bin$srv5y_s[cll_bin$srv5y>12] <- 0  # Apply an administrative censorship at t=12 months
cll_bin$srv5y[cll_bin$srv5y>12] <- 12

cll_bin$Status[cll_bin$srv5y_s==1] <- 1  # Define the new binary "Status" outcome variable
cll_bin$Status[cll_bin$srv5y_s==0] <- 0  # Encoding is 1:Dead, 0:Alive

## ------------------------------------------------------------------------
binary.outcome<-cll_bin$Status
artificially.censored<-(cll_bin$Status==0 & cll_bin$srv5y<12)
tableout <- table(artificially.censored,binary.outcome)
tableout

## ---- echo=FALSE---------------------------------------------------------
rm("binary.outcome","artificially.censored")

## ------------------------------------------------------------------------
cll_bin$srv5y <- NULL # Remove the original censoring and follow-up time 
cll_bin$srv5y_s <- NULL

## ---- warning=FALSE, message=FALSE, fig.height=10, fig.width=6-----------
library(mice)
md.pattern(cll_bin, rotate.names = TRUE)

## ------------------------------------------------------------------------
table(cll_bin$cyto)
table(cll_bin$perfstat)
table(cll_bin$remstat)
table(cll_bin$asct)
table(cll_bin$donor)
table(cll_bin$sex_match)
table(cll_bin$cond)

## ------------------------------------------------------------------------
summary(cll_bin)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  predcv_av <-
#    mipred.cv(
#      Status ~ age10 + perfstat + remstat + cyto + asct + donor + sex_match + cond,
#      family = binomial,
#      data = cll_bin[, -1],
#      nimp = 10,
#      folds = 10
#    )

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(predcv_av,file = "predcv_av.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "predcv_av.rda")

## ------------------------------------------------------------------------
head(predcv_av$pred)

## ------------------------------------------------------------------------
head(predcv_av$linpred)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  predcv_rb <-
#    mipred.cv(
#      Status ~ age10 + perfstat + remstat + cyto + asct + donor + sex_match + cond,
#      family = binomial,
#      data = cll_bin[, -1],
#      nimp = 10,
#      folds = 10,
#      method = "rubin"
#    )

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(predcv_rb,file = "predcv_rb.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "predcv_rb.rda")

## ------------------------------------------------------------------------
head(predcv_rb$pred)

## ------------------------------------------------------------------------
head(predcv_rb$linpred)

## ------------------------------------------------------------------------
predfinal_av <- apply(predcv_av$pred,1,mean)
predfinal_rb <- apply(predcv_rb$pred,1,mean)

## ------------------------------------------------------------------------
missrownrs<-sort(unique (unlist (lapply (cll_bin, function (x) which (is.na (x))))))
miss  <- matrix(0, nrow=nrow(cll_bin), ncol=1)
miss[missrownrs]<-1

## ---- fig.height=8, fig.width=8, fig.cap="Predicted values from the `averaging` and `rubin` method based on 10 imputations"----
par(mfrow=c(2,2),pty="s")
boxplot(predfinal_av~cll_bin$Status)
title("averaging method")
boxplot(predfinal_rb~cll_bin$Status)
title("rubin method")
matplot(predfinal_av,predfinal_rb,pch=19,type="n",xlab="averaging method",ylab="rubin method")
matpoints(predfinal_av[miss==1],predfinal_rb[miss==1],col=2,pch=1)
matpoints(predfinal_av[miss==0],predfinal_rb[miss==0],col=1,pch=1)
title("rubin versus averaging method")
matplot((predfinal_av+predfinal_rb)/2,predfinal_av-predfinal_rb,pch=19,type="n",
  xlab="average prediction",ylab="prediction difference")
matpoints((predfinal_av[miss==1]+predfinal_rb[miss==1])/2,
  predfinal_av[miss==1]-predfinal_rb[miss==1],col=2,pch=1)
matpoints((predfinal_av[miss==0]+predfinal_rb[miss==0])/2,
  predfinal_av[miss==0]-predfinal_rb[miss==0],col=1,pch=1)
title("differences versus average prediction")

## ---- eval=FALSE---------------------------------------------------------
#  repslist_av <- vector("list", 3)
#  m <- c(1, 10, 100)
#  for (counter in 1:3) {
#    reps_av <- array(NA, dim = c(nrow(cll_bin), m[counter], 10))
#    for (rep in 1:10) {
#      reps_av[, , rep] <-
#        mipred.cv(
#          Status ~ age10 + perfstat + remstat + cyto + asct + donor + sex_match + cond,
#          family = binomial,
#          data = cll_bin[,-1],
#          nimp = m[counter],
#          folds = 10
#        )[[2]]
#    }
#    repslist_av[[counter]] <- reps_av
#  }

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(repslist_av,file = "repslist_av.rda")

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  load(file = "repslist_av.rda")

## ---- eval=FALSE---------------------------------------------------------
#  repslist_rb <- vector("list", 3)
#  m <- c(1, 10, 100)
#  for (counter in 1:3) {
#    reps_rb <- array(NA, dim = c(nrow(cll_bin), m[counter], 10))
#    for (rep in 1:10) {
#      reps_rb[, , rep] <-
#        mipred.cv(
#          Status ~ age10 + perfstat + remstat + cyto + asct + donor + sex_match + cond,
#          family = binomial,
#          data = cll_bin[,-1],
#          nimp = m[counter],
#          folds = 10,
#          method = "rubin"
#        )[[2]]
#    }
#    repslist_rb[[counter]] <- reps_rb
#  }

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(repslist_rb,file = "repslist_rb.rda")

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  load(file = "repslist_rb.rda")

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  # remove this later - substitute for analysis results
#  repslist_av <- repslist_av[c(1,10,100)]
#  repslist_rb <- repslist_rb[c(1,10,100)]

## ---- eval=FALSE---------------------------------------------------------
#  avcv_mean3 <- apply(repslist_av[[3]][,,1],1,mean) # one hundred imputations

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(avcv_mean3,file = "avcv_mean3.rda")

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  load(file = "avcv_mean3.rda")

## ---- fig.height=6, fig.width=9, eval=FALSE------------------------------
#  par(mfrow=c(1,2),pty="s")
#  matplot(avcv_mean3,repslist_av[[3]][,,1],pch=19,type="n",
#    xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_mean3[miss==1],repslist_av[[3]][miss==1,,1],col=2,pch=1)
#  matpoints(avcv_mean3[miss==0],repslist_av[[3]][miss==0,,1],col=1,pch=1)
#  
#  matplot(avcv_mean3,
#    repslist_av[[3]][,,1]-avcv_mean3%*%matrix(1,nrow=1,ncol=ncol(repslist_av[[3]][,,1])),
#    pch=19,type="n",xlab="ordered mean probability",ylab="mean-centered individual imputed predictions")
#  matpoints(avcv_mean3[miss==1],
#    repslist_av[[3]][miss==1,,1]-
#      avcv_mean3[miss==1]%*%matrix(1,nrow=1,ncol=ncol(repslist_av[[3]][,,1])),
#    col=2,pch=1)
#  matpoints(avcv_mean3[miss==0],
#    repslist_av[[3]][miss==0,,1]-
#      avcv_mean3[miss==0]%*%matrix(1,nrow=1,ncol=ncol(repslist_av[[3]][,,1])),
#    col=1,pch=1)

## ---- out.width='100%', fig.align='center', echo=FALSE-------------------
knitr::include_graphics('fig_av_100.png')

## ---- eval=FALSE---------------------------------------------------------
#  rbcv_mean3 <- apply(repslist_rb[[3]][,,1],1,mean)

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(rbcv_mean3,file = "rbcv_mean3.rda")

## ---- echo=FALSE, eval=FALSE---------------------------------------------
#  load(file = "rbcv_mean3.rda")

## ---- fig.height=6, fig.width=9, eval=FALSE------------------------------
#  par(mfrow=c(1,2),pty="s")
#  matplot(rbcv_mean3,repslist_rb[[3]][,,1],pch=19,type="n",
#    xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_mean3[miss==1],repslist_rb[[3]][miss==1,,1],col=2,pch=1)
#  matpoints(rbcv_mean3[miss==0],repslist_rb[[3]][miss==0,,1],col=1,pch=1)
#  
#  matplot(rbcv_mean3,
#    repslist_rb[[3]][,,1]-rbcv_mean3%*%matrix(1,nrow=1,ncol=ncol(repslist_rb[[3]][,,1])),
#    pch=19,type="n",
#    xlab="ordered mean probability",ylab="mean-centered individual imputed predictions")
#  matpoints(rbcv_mean3[miss==1],
#    repslist_rb[[3]][miss==1,,1]-
#      rbcv_mean3[miss==1]%*%matrix(1,nrow=1,ncol=ncol(repslist_rb[[3]][,,1])),
#    col=2,pch=1)
#  matpoints(rbcv_mean3[miss==0],
#    repslist_rb[[3]][miss==0,,1]-
#      rbcv_mean3[miss==0]%*%matrix(1,nrow=1,ncol=ncol(repslist_rb[[3]][,,1])),
#    col=1,pch=1)

## ---- out.width='100%', fig.align='center', echo=FALSE-------------------
knitr::include_graphics('fig_rb_100.png')

## ---- eval=FALSE---------------------------------------------------------
#  avcv_means1 <- apply(repslist_av[[1]],1,mean) # one imputation
#  avcv_means2 <- apply(repslist_av[[2]],1,mean) # ten imputations
#  avcv_means3 <- apply(repslist_av[[3]],1,mean) # one hundred imputations

## ---- fig.height=6, fig.width=9, eval=FALSE------------------------------
#  par(mfrow=c(2,3),pty="s")
#  matplot(avcv_means1,apply(repslist_av[[1]],c(1,3),mean),pch=19,type="n",
#    xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_means1[miss==1],apply(repslist_av[[1]][miss==1,,,drop=FALSE],c(1,3),mean),
#    col=2,pch=1)
#  matpoints(avcv_means1[miss==0],apply(repslist_av[[1]][miss==0,,,drop=FALSE],c(1,3),mean),
#    col=1,pch=1)
#  title('single imputation')
#  
#  matplot(avcv_means2,apply(repslist_av[[2]],c(1,3),mean),pch=19,type="n",
#    xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_means2[miss==1],apply(repslist_av[[2]][miss==1,,],c(1,3),mean),
#    col=2,pch=1)
#  matpoints(avcv_means2[miss==0],apply(repslist_av[[2]][miss==0,,],c(1,3),mean),
#    col=1,pch=1)
#  title('10 imputations')
#  
#  matplot(avcv_means3,apply(repslist_av[[3]],c(1,3),mean),pch=19,type="n",
#    xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_means3[miss==1],apply(repslist_av[[3]][miss==1,,],c(1,3),mean),
#    col=2,pch=1)
#  matpoints(avcv_means3[miss==0],apply(repslist_av[[3]][miss==0,,],c(1,3),mean),col=1,pch=1)
#  title('100 imputations')
#  
#  matplot(avcv_means1,apply(repslist_av[[1]],c(1,3),mean)-avcv_means1%*%matrix(1,nrow=1,ncol=10),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_means1[miss==1],
#    apply(repslist_av[[1]][miss==1,,,drop=FALSE],c(1,3),mean)-
#      avcv_means1[miss==1]%*%matrix(1,nrow=1,ncol=10),
#    col=2,pch=1)
#  matpoints(avcv_means1[miss==0],
#    apply(repslist_av[[1]][miss==0,,,drop=FALSE],c(1,3),mean)-
#      avcv_means1[miss==0]%*%matrix(1,nrow=1,ncol=10),
#    col=1,pch=1)
#  
#  matplot(avcv_means2,
#    apply(repslist_av[[2]],c(1,3),mean)-avcv_means2%*%matrix(1,nrow=1,ncol=10),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_means2[miss==1],
#    apply(repslist_av[[2]][miss==1,,],c(1,3),mean)-
#      avcv_means2[miss==1]%*%matrix(1,nrow=1,ncol=10),
#    col=2,pch=1)
#  matpoints(avcv_means2[miss==0],
#    apply(repslist_av[[2]][miss==0,,],c(1,3),mean)-
#      avcv_means2[miss==0]%*%matrix(1,nrow=1,ncol=10),
#    col=1,pch=1)
#  
#  matplot(avcv_means3,
#    apply(repslist_av[[3]],c(1,3),mean)-avcv_means3%*%matrix(1,nrow=1,ncol=10),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(avcv_means3[miss==1],
#    apply(repslist_av[[3]][miss==1,,],c(1,3),mean)-
#      avcv_means3[miss==1]%*%matrix(1,nrow=1,ncol=10),
#    col=2,pch=1)
#  matpoints(avcv_means3[miss==0],
#    apply(repslist_av[[3]][miss==0,,],c(1,3),mean)-
#      avcv_means3[miss==0]%*%matrix(1,nrow=1,ncol=10),
#    col=1,pch=1)

## ---- out.width='100%', fig.align='center', echo=FALSE-------------------
knitr::include_graphics('fig_av_means.png')

## ---- eval=FALSE---------------------------------------------------------
#  rbcv_means1 <- apply(repslist_rb[[1]],1,mean)
#  rbcv_means2 <- apply(repslist_rb[[2]],1,mean)
#  rbcv_means3 <- apply(repslist_rb[[3]],1,mean)

## ---- fig.height=6, fig.width=9, eval=FALSE------------------------------
#  par(mfrow=c(2,3),pty="s")
#  matplot(rbcv_means1,apply(repslist_rb[[1]],c(1,3),mean),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_means1[miss==1],apply(repslist_rb[[1]][miss==1,,,drop=FALSE],c(1,3),mean),
#    col=2,pch=1)
#  matpoints(rbcv_means1[miss==0],apply(repslist_rb[[1]][miss==0,,,drop=FALSE],c(1,3),mean),
#    col=1,pch=1)
#  title('single imputation')
#  
#  matplot(rbcv_means2,apply(repslist_rb[[2]],c(1,3),mean),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_means2[miss==1],apply(repslist_rb[[2]][miss==1,,],c(1,3),mean),col=2,pch=1)
#  matpoints(rbcv_means2[miss==0],apply(repslist_rb[[2]][miss==0,,],c(1,3),mean),col=1,pch=1)
#  title('10 imputations')
#  
#  matplot(rbcv_means3,apply(repslist_rb[[3]],c(1,3),mean),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_means3[miss==1],apply(repslist_rb[[3]][miss==1,,],c(1,3),mean),col=2,pch=1)
#  matpoints(rbcv_means3[miss==0],apply(repslist_rb[[3]][miss==0,,],c(1,3),mean),col=1,pch=1)
#  title('100 imputations')
#  
#  matplot(rbcv_means1,
#    apply(repslist_rb[[1]],c(1,3),mean)-rbcv_means1%*%matrix(1,nrow=1,ncol=10),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_means1[miss==1],
#    apply(repslist_rb[[1]][miss==1,,,drop=FALSE],c(1,3),mean)-
#      rbcv_means1[miss==1]%*%matrix(1,nrow=1,ncol=10),
#    col=2,pch=1)
#  matpoints(rbcv_means1[miss==0],
#    apply(repslist_rb[[1]][miss==0,,,drop=FALSE],c(1,3),mean)-
#      rbcv_means1[miss==0]%*%matrix(1,nrow=1,ncol=10),
#    col=1,pch=1)
#  
#  matplot(rbcv_means2,
#    apply(repslist_rb[[2]],c(1,3),mean)-rbcv_means2%*%matrix(1,nrow=1,ncol=10),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_means2[miss==1],
#    apply(repslist_rb[[2]][miss==1,,],c(1,3),mean)-
#      rbcv_means2[miss==1]%*%matrix(1,nrow=1,ncol=10),
#    col=2,pch=1)
#  matpoints(rbcv_means2[miss==0],
#    apply(repslist_rb[[2]][miss==0,,],c(1,3),mean)-
#      rbcv_means2[miss==0]%*%matrix(1,nrow=1,ncol=10),
#    col=1,pch=1)
#  
#  matplot(rbcv_means3,
#    apply(repslist_rb[[3]],c(1,3),mean)-rbcv_means3%*%matrix(1,nrow=1,ncol=10),
#    pch=19,type="n",xlab="ordered mean probability",ylab="individual imputed predictions")
#  matpoints(rbcv_means3[miss==1],
#    apply(repslist_rb[[3]][miss==1,,],c(1,3),mean)-
#      rbcv_means3[miss==1]%*%matrix(1,nrow=1,ncol=10),
#    col=2,pch=1)
#  matpoints(rbcv_means3[miss==0],
#    apply(repslist_rb[[3]][miss==0,,],c(1,3),mean)-
#      rbcv_means3[miss==0]%*%matrix(1,nrow=1,ncol=10),
#    col=1,pch=1)

## ---- out.width='100%', fig.align='center', echo=FALSE-------------------
knitr::include_graphics('fig_rb_means.png')

## ------------------------------------------------------------------------
R.statistic <- function(reps, miss){
resmat<-matrix(NA,nrow=2,ncol=7)
dimnames(resmat)<-list(c("missing","observed"),
  c("R (range)","q10","median","q90","missing","replicates", "mean"))
means <- apply(reps, 1, mean)
diffs<-reps-means%*%matrix(1,nrow=1,ncol=ncol(reps))  # remove means

# variation between p=0.2 and 0.8,  and fully observed records
diffssel<-diffs[means<0.8&means>0.2&miss==0,] # select between 0.2 and 0.8 and observed records
quant<-quantile(as.vector(diffssel),c(.10, 0.5, .90))
resmat[2,2:4]<-quant
resmat[2,1]<-quant[3]-quant[1] # R measure
resmat[2,5]<-0  #missing record?  1=yes, 0=no
resmat[2,6]<-ncol(reps)  # number of replicates
resmat[2,7]<-mean(as.vector(diffssel)) # mean

# variation between p=0.2 and 0.8,  and missing records
diffssel<-diffs[means<0.8&means>0.2&miss==1,] # select between 0.2 and 0.8 and missing records
quant<-quantile(as.vector(diffssel),c(.10, 0.5, .90))
resmat[1,2:4]<-quant
resmat[1,1]<-quant[3]-quant[1] # R measure 
resmat[1,5]<-1  #missing record?  1=yes, 0=no
resmat[1,6]<-ncol(reps)  # number of replicates
resmat[1,7]<-mean(as.vector(diffssel))  # mean
resmat
}

## ---- echo=TRUE, message=FALSE, eval=FALSE-------------------------------
#  R1.av <- R.statistic(apply(repslist_av[[1]],c(1,3),mean),miss)
#  R2.av <- R.statistic(apply(repslist_av[[2]],c(1,3),mean),miss)
#  R3.av <- R.statistic(apply(repslist_av[[3]],c(1,3),mean),miss)
#  # Make percentages
#  Rstat.av <-
#    rbind(R1.av, R2.av, R3.av)*matrix(rep(c(100,100,100,100,1,1,100),6),byrow=T,ncol=7)
#  Rstat.av.miss <- t(Rstat.av[c(1,3,5),1])
#  Rstat.av.obsv <- t(Rstat.av[c(2,4,6),1])

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(Rstat.av,file = "Rstat_av.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "Rstat_av.rda")
Rstat.av.miss <- t(Rstat.av[c(1,3,5),1]) 
Rstat.av.obsv <- t(Rstat.av[c(2,4,6),1])

## ---- echo=TRUE, message=FALSE, eval=FALSE-------------------------------
#  R1.rb <- R.statistic(apply(repslist_rb[[1]],c(1,3),mean),miss)
#  R2.rb <- R.statistic(apply(repslist_rb[[2]],c(1,3),mean),miss)
#  R3.rb <- R.statistic(apply(repslist_rb[[3]],c(1,3),mean),miss)
#  # Make percentages
#  Rstat.rb <-
#    rbind(R1.rb, R2.rb, R3.rb)*matrix(rep(c(100,100,100,100,1,1,100),6),byrow=T,ncol=7)
#  Rstat.rb.miss <- t(Rstat.rb[c(1,3,5),1])
#  Rstat.rb.obsv <- t(Rstat.rb[c(2,4,6),1])

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(Rstat.rb,file = "Rstat_rb.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "Rstat_rb.rda")
Rstat.rb.miss <- t(Rstat.rb[c(1,3,5),1])
Rstat.rb.obsv <- t(Rstat.rb[c(2,4,6),1])

## ---- fig.height=6, fig.width=9------------------------------------------
par(mfrow=c(1,2),pty="s")
index <- c(1, 10, 100)
# first plot - averaging approach.
matplot(index,cbind(t(Rstat.av.miss), t(Rstat.av.obsv)),
  type="n",log="x",pch=NULL,ylim=c(0,15),axes=F,
  xlab="number of imputations",ylab="percentage deviation R")
matlines(index,cbind(t(Rstat.av.miss), t(Rstat.av.obsv)),
  type="b",lty=c(1,1,2,2),col=1,log="x",pch=c(1,20))
axis(1,at=index)
axis(2)
box()
title("Averaging approach")
# second plot - rubin approach.
matplot(index,cbind(t(Rstat.rb.miss), t(Rstat.rb.obsv)),
  type="n",log="x",pch=NULL,ylim=c(0,15),axes=F,
  xlab="number of imputations",ylab="percentage deviation R")
matlines(index,cbind(t(Rstat.rb.miss), t(Rstat.rb.obsv)),
  type="b",lty=c(1,1,2,2),col=1,log="x",pch=c(1,20))
axis(1,at=c(1,10,100))
axis(2)
box()
title("Rubin approach")

## ------------------------------------------------------------------------
knitr::kable(round(Rstat.av, digits=2), 
  caption="Variance summaries of deviation relative to the mean prediction between 
  replicate predictions for different imputations with the averaging approach. 
  All statistics were multiplied by 100.")

## ------------------------------------------------------------------------
knitr::kable(round(Rstat.rb, digits=2), 
  caption="Variance summaries of deviation relative to the mean prediction between 
  predictions for different imputations with the rubin approach. 
  All statistics were multiplied by 100.")

## ------------------------------------------------------------------------
library(pROC)
Briers <- function(status, reps, miss) {
  resmat <- matrix(NA, nrow = 3, ncol = 4)
  dimnames(resmat) <-
    list(c("missing", "all", "complete"), c("Briermean", "Briersd", "AUCmean", "AUCsd"))
  
  statustotal <- status
  statusmissing <- status[miss == 1]
  statusobserved <- status[miss == 0]
  Briers <-
    apply(reps, 2, function(x)
      mean((statustotal - x) ^ 2)) # get Briers for each replicate analysis
  Briersmissing <-
    apply(reps[miss == 1, , drop=F], 2, function(x)
      mean((statusmissing - x) ^ 2)) # for missing records only
  Briersobserved <-
    apply(reps[miss == 0, , drop=F], 2, function(x)
      mean((statusobserved - x) ^ 2)) # for complete records

  AUCs <-
    apply(reps, 2, function(x)
      auc(roc(statustotal, x))) # get AUCs for each replicate analysis
  AUCmissing <-
    apply(reps[miss == 1, , drop = F], 2, function(x)
      auc(roc(statusmissing, x))) # for missing records only
  AUCobserved <-
    apply(reps[miss == 0, , drop = F], 2, function(x)
      auc(roc(statusobserved, x))) # for complete records
  
resmat[1,1]<-mean(Briersmissing)
resmat[2,1]<-mean(Briers)
resmat[3,1]<-mean(Briersobserved)
resmat[1,2]<-sd(Briersmissing)
resmat[2,2]<-sd(Briers)
resmat[3,2]<-sd(Briersobserved)

resmat[1,3]<-mean(AUCmissing)
resmat[2,3]<-mean(AUCs)
resmat[3,3]<-mean(AUCobserved)
resmat[1,4]<-sd(AUCmissing)
resmat[2,4]<-sd(AUCs)
resmat[3,4]<-sd(AUCobserved)

resmat
}

## ---- warning=FALSE, error=FALSE, eval=FALSE-----------------------------
#  B1.av <- Briers(cll_bin$Status, apply(repslist_av[[1]],c(1,3),mean),miss)
#  B2.av <- Briers(cll_bin$Status, apply(repslist_av[[2]],c(1,3),mean),miss)
#  B3.av <- Briers(cll_bin$Status, apply(repslist_av[[3]],c(1,3),mean),miss)
#  
#  B1.rb <- Briers(cll_bin$Status, apply(repslist_rb[[1]],c(1,3),mean),miss)
#  B2.rb <- Briers(cll_bin$Status, apply(repslist_rb[[2]],c(1,3),mean),miss)
#  B3.rb <- Briers(cll_bin$Status, apply(repslist_rb[[3]],c(1,3),mean),miss)
#  
#  Brier.av <- 100*rbind(B1.av, B2.av, B3.av) # Make percentages
#  Brier.rb <- 100*rbind(B1.rb, B2.rb, B3.rb) # Make percentages

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(Brier.av,file = "Brier_av.rda")
#  save(Brier.rb,file = "Brier_rb.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "Brier_av.rda")
load(file = "Brier_rb.rda")

## ------------------------------------------------------------------------
knitr::kable(round(Brier.av, digits=2), 
  caption = "Brier scores and AUCs calculated on predictions generated from 
  the averaging method with different numbers of imputations.  
  Standard deviations are shown for 10 replicate analyses.
  All statistics were multiplied by 100.")

## ------------------------------------------------------------------------
knitr::kable(round(Brier.rb, digits=2), 
caption = "Brier scores and AUCs calculated on predictions generated from 
the rubin method with different numbers of imputations.  
Standard deviations are shown for 10 replicate analyses.
  All statistics were multiplied by 100.")

## ---- fig.height=6, fig.width=9------------------------------------------
par(mfrow=c(2,3),pty="s")
index <- c(1, 10, 100)
matplot(index,cbind(Brier.av[c(1,4,7),1,drop=F],Brier.rb[c(1,4,7),1,drop=F]),
  type="n",log="x",pch=NULL,axes=F,xlab="number of imputations",ylab="mean Brier score")
matlines(index,Brier.av[c(1,4,7),1,drop=F],
  type="b",lty=c(1,1,2,2),col=1,log="x",pch="1")
matlines(index,Brier.rb[c(1,4,7),1,drop=F],
  type="b",lty=c(1,1,2,2),col=1,log="x",pch="2")
axis(1,at=index)
axis(2)
box()
title("Missing data")

matplot(index,cbind(Brier.av[c(2,5,8),1,drop=F],Brier.rb[c(2,5,8),1,drop=F]),
  type="n",log="x",pch=NULL,axes=F,xlab="number of imputations",ylab="mean Brier score")
matlines(index,Brier.av[c(2,5,8),1,drop=F],
  type="b",lty=c(1,1,2,2),col=1,log="x",pch="1")
matlines(index,Brier.rb[c(2,5,8),1,drop=F],
  type="b",lty=c(1,1,2,2),col=1,log="x",pch="2")
axis(1,at=index)
axis(2)
box()
title("All data")

matplot(index,cbind(Brier.av[c(3,6,9),1,drop=F],Brier.rb[c(3,6,9),1,drop=F]),
  type="n",log="x",pch=NULL,axes=F,xlab="number of imputations",ylab="mean Brier score")
matlines(index,Brier.av[c(3,6,9),1,drop=F],
  type="b",lty=c(1,1,2,2),col=1,log="x",pch="1")
matlines(index,Brier.rb[c(3,6,9),1,drop=F],
  type="b",lty=c(1,1,2,2),col=1,log="x",pch="2")
axis(1,at=index)
axis(2)
box()
title("Fully observed data")


matplot(index,Brier.av[c(1,4,7),1,drop=F],type="n",log="x",
  ylim=c(17.5,21),pch=NULL,axes=F,xlab="number of imputations",ylab="mean Brier score")
matlines(index,Brier.av[c(1,4,7),1,drop=F],
  type="b",ylim=c(17.5,21),lty=c(1,1,2,2),col=1,log="x",pch="1")
matlines(index,Brier.rb[c(1,4,7),1,drop=F],
  type="b",ylim=c(17.5,21),lty=c(1,1,2,2),col=1,log="x",pch="2")
axis(1,at=index)
axis(2)
box()
title("Missing data")

matplot(index,Brier.av[c(2,5,8),1,drop=F],type="n",log="x",
  ylim=c(17.5,21),pch=NULL,axes=F,xlab="number of imputations",ylab="mean Brier score")
matlines(index,Brier.av[c(2,5,8),1,drop=F],
  type="b",ylim=c(17.5,21),lty=c(1,1,2,2),col=1,log="x",pch="1")
matlines(index,Brier.rb[c(2,5,8),1,drop=F],
  type="b",ylim=c(17.5,21),lty=c(1,1,2,2),col=1,log="x",pch="2")
axis(1,at=index)
axis(2)
box()
title("All data")

matplot(index,Brier.av[c(3,6,9),1,drop=F],type="n",log="x",ylim=c(17.5,21),
  pch=NULL,axes=F,xlab="number of imputations",ylab="mean Brier score")
matlines(index,Brier.av[c(3,6,9),1,drop=F],
  type="b",ylim=c(17.5,21),lty=c(1,1,2,2),col=1,log="x",pch="1")
matlines(index,Brier.rb[c(3,6,9),1,drop=F],
  type="b",ylim=c(17.5,21),lty=c(1,1,2,2),col=1,log="x",pch="2")
axis(1,at=index)
axis(2)
box()
title("Fully observed data")

## ------------------------------------------------------------------------
load(file = "cll_bin_new.rda")

## ------------------------------------------------------------------------
head(cll_bin_new)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  pred_av <-
#    mipred(
#      Status ~ age10 + perfstat + remstat + cyto + asct + donor + sex_match + cond,
#      family = binomial,
#      data = cll_bin[, -1],
#      newdata = cll_bin_new[, -1],
#      nimp = 100,
#      folds = 1
#    )

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(pred_av,file = "pred_av.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "pred_av.rda")

## ------------------------------------------------------------------------
head(pred_av$pred)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  pred_rb <-
#    mipred(
#      Status ~ age10 + perfstat + remstat + cyto + asct + donor + sex_match + cond,
#      family = binomial,
#      data = cll_bin[, -1],
#      newdata = cll_bin_new[, -1],
#      nimp = 100,
#      folds = 1,
#      method = "rubin"
#    )

## ---- echo=FALSE, message=FALSE, eval=FALSE------------------------------
#  save(pred_rb,file = "pred_rb.rda")

## ---- echo=FALSE---------------------------------------------------------
load(file = "pred_rb.rda")

## ------------------------------------------------------------------------
head(pred_rb$pred)

## ------------------------------------------------------------------------
av_means <- apply(pred_av[[2]],1,mean)
rb_means <- apply(pred_rb[[2]],1,mean)

## ------------------------------------------------------------------------
av_means
rb_means

## ------------------------------------------------------------------------
boxplot(t(pred_av[[2]]))

## ------------------------------------------------------------------------
summary(t(pred_av[[2]]))

## ------------------------------------------------------------------------
boxplot(t(pred_rb[[2]]))

## ------------------------------------------------------------------------
summary(t(pred_rb[[2]]))

## ---- message=FALSE, eval=FALSE------------------------------------------
#  predcv_av <-
#    mipred.cv(
#      Status ~ .,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 10,
#      folds = 10
#    )

## ---- message=FALSE, eval=FALSE------------------------------------------
#  pred_av <-
#    mipred(
#      Status ~ .,
#      family = binomial,
#      data = cll_bin[, -1],
#      newdata = cll_bin_new[, -1],
#      nimp = 100,
#      folds = 1
#    )

## ---- message=FALSE, eval=FALSE------------------------------------------
#  predcv_av <-
#    mipred.cv(
#      Status ~ age10 + I(age10 ^ 2) + sex_match + cyto,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 10,
#      folds = 10
#    )

## ---- message=FALSE, eval=FALSE------------------------------------------
#  predcv_av <-
#    mipred.cv(
#      Status ~ age10 + sex_match + cyto,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 10,
#      folds = 10
#    )

## ------------------------------------------------------------------------
imp <- mice(cll_bin[, -1], m = 1, maxit = 0, printFlag = FALSE)

## ------------------------------------------------------------------------
names(imp)

## ------------------------------------------------------------------------
imp$predictorMatrix

## ------------------------------------------------------------------------
predmat <- imp$predictorMatrix
predmat[,c("age10")] <- 0
predmat

## ---- message=FALSE, eval=FALSE------------------------------------------
#  predcv_av <-
#    mipred.cv(
#      Status ~ age10 + sex_match + cyto,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 10,
#      folds = 10,
#      mice.options = list(predictorMatrix = predmat)
#    )

## ---- eval=FALSE---------------------------------------------------------
#  seed<-round(runif(10*5)*10000) # 10*5 because 10 folds by 5 imputations (for each fold)
#  # hence 10*5 separate calls to mice
#  mice.options<-NULL
#  mice.options$seed<-seed
#  set.seed(12345) # need to set seed explicitly here as well due to random generation for
#  # fold definition (outside mice function)
#  predcv_av <-
#    mipred.cv(
#      Status ~ age10 + sex_match + cyto,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 5,
#      folds = 10,
#      mice.options=list(maxit=5, printFlag=FALSE, seed=seed)
#    )

## ---- eval=FALSE---------------------------------------------------------
#  seed<-round(runif(10)*10000) # 10 because 10 folds and all imputations generated directly for each fold
#  # the 5 imputations are directly generated in one call to the `mice`function for each fold
#  mice.options<-NULL
#  mice.options$seed<-seed
#  set.seed(12345) # need to set seed explicitly here as well due to random generation for
#  # fold definition (outside mice function)
#  predcv_av <-
#    mipred.cv(
#      Status ~ age10 + sex_match + cyto,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 5,
#      folds = 10,
#      method="rubin",
#      mice.options=list(maxit=5, printFlag=FALSE, seed=seed)
#    )

## ---- eval=FALSE---------------------------------------------------------
#  mipred.cv(
#      Status ~ perfstat+remstat+cyto,
#      family = binomial,
#      data = cll_bin[,-1],
#      nimp = 5,
#      folds = 2
#    )


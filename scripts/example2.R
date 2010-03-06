###############################################################################
# clusterCons - Consensus clustering functions for R
# 
# Author: Dr. T. Ian Simpson
# Affiliation : University of Edinburgh
# E-mail : ian.simpson@ed.ac.uk
#
# Example script number 2 - more advanced use with data from Golub et al. 1999
###############################################################################

data('golub');

#call the cluscomp method
cmr <- cluscomp(data.frame(t(golub)),algorithms=c('kmeans','pam'),merge=0,clmin=2,clmax=5,reps=100)

#exploring the cmr
summary(cmr);
summary(cmr$kmeans_3);
getClass('consmatrix');

#lets look at a heat map
cm <- cmr$kmeans_3;
heatmap(cm@cm);

#get cluster robustness
cr <- clrob(cm);

#get member robustness
mr <- memrob(cm);

#lets expore the mr list
summary(mr);

#get the member robustness for the first cluster
mr$cluster1;

#now lets move to looking at some comparisons
#calculate the areas under the curves
ac <- aucs(cmr)

#plot out the auc curves
library(lattice);
library(grid);

xyplot(aucs~k,groups=a,ac,auto.key=T,xlab='cluster',ylab='AUC',type='b')

#now lets calculate the deltak
dk <- deltak(ac)

#plot out the results to find optimal class number
x11()
xyplot(deltak~k,groups=a,dk,type='b',auto.key=T)
#now plot out a bit more elegantly
x11()
grp=c('red','green')
title=list(label='Change in A(K) by cluster number with two different clustering algorithms',cex=0.8)
legend <-list(title='algorithm',cex.title=1,text=list(levels(dk$a)),cex=0.8,lines=list(col=grp,type=c('b'),pch=1))
details <- list(title='conditions',cex.title=1,text=list(c("iterations = 100","resampling freq. = 0.8")),cex=0.8)
ylab = expression({Delta*K})
xyplot(deltak~k,groups=a,dk,col=grp,type=c('l','p'),main=title,scales=list( x=list( at = c(1:4))),xlab='cluster number',ylab=ylab)
draw.key(vp=viewport(0.8,0.8),legend,draw=T)
draw.key(vp=viewport(0.8,0.6),details,draw=T)


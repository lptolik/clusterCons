###############################################################################
# Alogrithm specification file for clusterCons
# 
# Author: Dr. T. Ian Simpson
# Affiliation : University of Edinburgh
# E-mail : ian.simpson@ed.ac.uk
# Version : 0.3
###############################################################################
#The following clustering algorithms from cluster are specified here :-
#		agnes  - agglomerative hierarchical clustering
#		pam    - partitioning around medoids, a more robust version of k-means
#		kmeans - k-means clustering
#		hclust - hierarchical cluster analysis on a set of dissimilarities
#		diana  - divisive hierarchical clustering
###############################################################################
#agnes
agnes_clmem <- function(x,clnum,params=list()){
	#assign the parameters to the data.frame
	params$x <- x
	#perform the clustering
	hc <- do.call(agnes,params)
	#grab the membership object
	clmem <- data.frame(cutree(hc,clnum))
	#move the row.names across
	row.names(clmem) <- row.names(x)
	#tidy the column header
	names(clmem) <-c('cm')
	#return the membership object
	return(clmem)
}
#examples
#1. with nothing but the data.frame and the clnum
#agnes_clmem(cluster_example_data2,clnum=2)
#2. with both specified (k will be ignored as this will be set to the current clnum in the loop)
#agnes_clmem(cluster_example_data2,clnum=2,params=list(metric='euclidean',method='average'))

#pam
#be careful to use a valid metric name for pam as the function does not check and will default to euclidean
pam_clmem <- function(x,clnum,params=list()){
	#the data is not a dissimilarity matrix so :-
	#check if params have been passed if not set up the default
	#the k value should not be specfied as this comes from the loop
	if(length(params)==0){
		#pam will automatically generate a distance matrix from a data frame, but here we explicitly state the distance metric for clarity
		params <- list(metric='euclidean',k=clnum)
	}
	else{
		#If k is set then replace with clnum. If diss has not been set default to 'euclidean'
		#it checks the list with regexpr checks if any are equal to 1 (T/F) return then sums the logicals
		if(sum(regexpr('^metric$',names(params))==1)<1){
			params$metric='euclidean'
		}
		params$k <- clnum
	}
	params$x <- x;
	clmem <- data.frame(do.call(pam,params)$clustering);
	names(clmem) <- c('cm');
	return(clmem);
}
#examples
#1. with no dissimilarity matrix or cluster number specified
#pam_clmem(cluster_example_data2,clnum=2)
#2. with both specified (k will be ignored as this will be set to the current clnum in the loop)
#pam_clmem(cluster_example_data2,clnum=2,params=list(metric='euclidean',k=2))

#kmeans
kmeans_clmem <- function(x,clnum,params=list()){
	#need to check whether a center call is made in params if it is needs to be corrected to current clnum
	if(length(params)==0){
#		params <- list(centers=clnum);
		params <- list(centers=clnum,nstart=10);
	}
	else{
		#check whether centers has been set. If it has then it should be replaced with clnum
		#it checks the list with regexpr checks if any are equal to 1 (T/F) return then sums the logicals
		if(sum(regexpr('^centers$',names(params))==1)>=1){
			params$centers=clnum
		}
	}
	params$x <- x;
	clmem <- data.frame(do.call(kmeans,params)$cluster);
	names(clmem) <- c('cm');
	return(clmem);
}
#examples
#1. simple
#kmeans_clmem(cluster_example_data2,clnum=4)
#2. with change of kmeans algorithm, note clnum overides centers
#kmeans_clmem(cluster_example_data2,clnum=6,params=list(algorithm="Lloyd",centers=23))

#hclust
hclust_clmem <- function(x,clnum,params=list()){
	#check if params have been passed if not set up the default
	if(length(params)!=0){
		#the data is not a dissimilarity matrix allow new param for this calling function named diss
		#check whether diss has been set, if so generate distance matrix
		#it checks the list with regexpr checks if any are equal to 1 (T/F) return then sums the logicals
		if(sum(regexpr('^diss$',names(params))==1)>=1){
			dm <-dist(x,method=params$diss)
			#drop the parameter from the list
			params$diss <-NULL;
		}
		else{
			#if not default euclidean
			dm <- dist(x);
		}
		params$d <- dm;
	}
	else{
		dm <- dist(x);
		params <- list(d=dm);
	}
	#perform the clustering
	hc <- do.call(hclust,params)
	#grab the membership object
	clmem <- data.frame(cutree(hc,clnum))
	#tidy the column header
	names(clmem) <-c('cm')
	return(clmem);
}
#examples
#1. simple
#hclust_clmem(cluster_example_data2,clnum=4)
#2. with change of distance algorithm
#hclust_clmem(cluster_example_data2,clnum=6,params=list(diss='manhattan'))

#diana
#be careful to use a valid metric name for pam as the function does not check and will default to euclidean
diana_clmem <- function(x,clnum,params=list()){
	#the data is not a dissimilarity matrix so :-
	#check if params have been passed if not set up the default
	if(length(params)!=0){
		#check whether diss has been set, if not then set to default 'euclidean'
		if(sum(regexpr('^metric$',names(params))==1)<1){
			params$metric='euclidean'
		}
	}
	#pam will automatically generate a distance matrix from a data frame, but here we explicitly state the distance metric for clarity
	else{
		params <- list(metric='euclidean')
	}
	params$x <- x;
	hc <- do.call(diana,params);
	#grab the membership object
	clmem <- data.frame(cutree(hc,clnum))
	#move the row.names across
	row.names(clmem) <- row.names(x)
	#tidy the column header
	names(clmem) <-c('cm')
	#return the membership object
	return(clmem)
}
#examples
#1. simple
#diana_clmem(cluster_example_data2,clnum=4)
#2. with change of distance algorithm
#diana_clmem(cluster_example_data2,clnum=6,params=list(metric='manhattan'))

#OPTIONAL ADDITIONAL FUNCTIONS (uncomment for use)

##ADD YOUR OWN ALGORITHM CALLING FUNCTIONS HERE

##generic new function
#new_clmem(x,clnum,params=list()){
#	#perform parameter checks to ensure that the call is consitent with the algorithm function
#	#assign the paramters to the data frame
#	params$x <- x;
#	#perform the clustering
#	cl_obj <- data.frame(do.call(new,params));
#	#extract or convert to the cluster membership object and convert to data.frame for example :- 
#	#for direct assignment algorithms
#	clmem <- data.frame(cl_obj$clustering) # or whatever the field name for the assuigned clusters is in the returned object
#	#or for heirarchical cluster results
#	#clmem <- data.frame(cutree(cl_obj,clnum));
#	names(clmem) <- c('cm');
#	return(clmem);	
#}

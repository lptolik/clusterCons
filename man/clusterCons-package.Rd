\name{clusterCons-package}
\alias{clusterCons-package}
\alias{clusterCons}
\docType{package}
\title{Calculate consensus clustering results from re-sampled clustering experiments with the option of using multiple algorithms and parameters}
\description{
clusterCons is a package containing functions that generate robustness measures for clusters and cluster membership based on generating consensus 
matrices from bootstrapped clustering experiments in which a random proportion of rows of the data set are used in each individual clustering.
This allows the user to prioritise clusters and the members of clusters based on their consistency in this regime. The functions allow the user to
select several algorithms to use in the re-sampling scheme and with any of the parameters that the algorithm would normally take.
}
\details{
\tabular{ll}{
Package: \tab clusterCons\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2010-03-09\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
Depends: \tab methods,cluster\cr
Extends: \tab cluster\cr
Suggests: \tab lattice\cr
}

The user should first prepare an entirely numeric \code{data.frame} in which the conditions to be clustered are the column names and the unique ids
of the entities are the row names. Compatibility of the resulting data.fram can be checked by using the \code{data_check} function. 
}
%main
\section{Functions to run the consensus clustering and retrieve robustness information}{
\code{cluscomp} - generate consensus matrices from re-sampled clustering experiments with the option of multiple algorithms and parameters\cr
\code{clrob} - calculate the robustness of the clusters from the consensus matrix\cr
\code{memrob} - calculate the cluster membership robustness from the consensus matrix\cr
}
%algorithms
\section{Internal functions to call the individual clustering algorithms}{
\code{agnes_clmem} - wrapper for the \code{agnes} function of package cluster\cr
\code{diana_clmem} - wrapper for the \code{diana} function of package cluster\cr
\code{hclust_clmem} - wrapper for the \code{hclust} function of package cluster\cr
\code{kmeans_clmem} - wrapper for the \code{kmeans} function of package cluster\cr
\code{pam_clmem} - wrapper for the \code{pam} function of package cluster\cr
}
%analysis curves
\section{Functions to calculate AUC related metrics}{
\code{auc} - calculates the area under the curve for a series of clustering experiments with the same cluster number\cr
\code{aucs} - calculates the areas under the curves of a series of clustering experiments over a range of cluster numbers\cr
\code{deltak} - calculates the change in the area under the curve\cr
}

%checking and class definition
\section{Functions to check data and object validity}{
\code{data_check} - check that the provided \code{data.frame} is formatted correctly\cr
\code{validConsMatrixObject} - check the validity of a \code{consmatrix} object\cr
\code{validMergeMatrixObject} - check the validity of a \code{mergematrix} object\cr
\code{validMemRobListObject} - check the validity of a membership robustness list object\cr
\code{validMemRobMatrixObject} - check the validity of a membership robustness matrix object\cr
}

\section{Author}{
Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\section{References}{
\cite{
Gene and group prioritisation using a simple consensus clustering methodology in R. Simpson, T.I., Armstrong, J.D. and Jarman A.P.
(manuscript in preparation).}

\cite{
Consensus clustering: A resampling-based method for class discovery and visualization of gene expression microarray data. Monti, S., Tamayo, P., Mesirov, J. and Golub, T. Machine Learning, 52, July 2003.
}
}

\section{Keywords}{
cluster
}

\section{See Also}{
\code{\link{cluster}},\code{\link{lattice}}
}

\section{Examples}{
~~ simple examples of the most important functions ~~
}

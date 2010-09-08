\name{deltak}
\alias{deltak}
\title{
Function to calculate the change in the area under the curve (AUC) across a range of cluster number values
}
\description{
This function takes an \code{"auc"} class object and calculates the difference in AUC value by cluster number (called delta-K). Peaks in delta-K
coincide with the cluster numbers that are most robust and provide estimates for the optimal cluster number.
}
\usage{
deltak(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
		a valid \code{"auc"} class object, normally provided as a result from the \code{\link{aucs}} function.
}
}

\value{ 
	\code{deltak(x)} returns a data.frame with the following variables.
	\item{k}{
		cluster number as a factor
		}
	\item{a}{
		algorithm identifier as a factor
		}
	\item{dk}{
		the delta-K value
	}
}

\references{
Merged consensus clustering to assess and improve classification of microarray data. Simpson, T.I., Armstrong, J.D. and Jarman A.P.
(submitted).}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\seealso{
Also see the \code{\link{aucs}} function.
}
\examples{
#load a test cluscomp result set
data(testcmr)

#reduce size for the example
testcmr <- testcmr[1:12];

#calculate the AUCs
acs <- aucs(testcmr);

#calculate the delta-K values
dks <- deltak(acs);
}
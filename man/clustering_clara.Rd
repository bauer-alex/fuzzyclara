% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering_clara.R
\name{clustering_clara}
\alias{clustering_clara}
\title{Perform CLARA clustering algorithm}
\usage{
clustering_clara(
  data,
  clusters = 5,
  metric = "euclidean",
  samples = 10,
  sample_size = NULL,
  type = "hard",
  cores = 1,
  seed = 1234,
  m = 1.5,
  verbose = 1,
  build = FALSE,
  ...
)
}
\arguments{
\item{data}{data.frame to be clustered}

\item{clusters}{Number of clusters. Defaults to 5.}

\item{metric}{A character specifying a predefined dissimilarity metric (like
\code{"euclidean"} or \code{"manhattan"}) or a self-defined dissimilarity
function. Defaults to \code{"euclidean"}. Will be passed as argument
\code{method} to \code{\link[proxy]{dist}}, so check \code{?proxy::dist} for
full details.}

\item{samples}{Number of subsamples}

\item{sample_size}{Number of observations belonging to a sample. If NULL
(default), the minimum of \code{nrow(data)} and \code{40 + clusters * 2} is
used as sample size.}

\item{type}{One of \code{c("hard","fuzzy")}, specifying the type of
clustering to be performed.}

\item{cores}{Numbers of cores for computation. \code{cores > 1} implies
a parallel call. Defaults to 1.}

\item{seed}{Random number seed. Defaults to 1234.}

\item{m}{Fuzziness exponent (only for \code{type = "fuzzy"}), which has to be
a numeric of minimum 1. Defaults to 2.}

\item{verbose}{Can be set to integers between 0 and 2 to control the level of
detail of the printed diagnostic messages. Higher numbers lead to more
detailed messages. Defaults to 1.}

\item{build}{Additional build algorithm to choose initial medoids (only
relevant for type = "fuzzy". Default FALSE.)}

\item{...}{Additional arguments passed to the main clustering algorithm and
to proxy::dist for the calculation of the distance matrix
(\code{\link{pam}} or \code{\link[vegclust]{vegclust}})}
}
\value{
Object of class fuzzyclara
}
\description{
Function to perform a CLARA clustering in a hard or fuzzy way.
The function can either be called using a common dissimilarity metric or
a self-defined distance function.
}
\details{
If the clustering is run on mulitple cores, the verbose messages are printed
in a file \code{clustering_progress.log} (if \code{verbose > 0}).
}
\references{
Kaufman, L., and Rousseeuw, P. J. (1986). Clustering large data sets.
\emph{Pattern Recognition in Practice}, 425--437.
}

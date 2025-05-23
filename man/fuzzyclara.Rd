% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzyclara.R
\name{fuzzyclara}
\alias{fuzzyclara}
\title{Perform clustering}
\usage{
fuzzyclara(
  data,
  clusters = 5,
  metric = "euclidean",
  algorithm = "clara",
  samples = 10,
  sample_size = NULL,
  max_neighbors = 100,
  num_local = 10,
  type = "hard",
  cores = 1,
  seed = 1234,
  m = 1.5,
  verbose = 1,
  scale = TRUE,
  build = FALSE,
  ...
)
}
\arguments{
\item{data}{data.frame or matrix to be clustered}

\item{clusters}{Number of clusters. Defaults to 5.}

\item{metric}{A character specifying a predefined dissimilarity metric (like
\code{"euclidean"} or \code{"manhattan"}) or a self-defined dissimilarity
function. Defaults to \code{"euclidean"}. Will be passed as argument
\code{method} to \code{\link[proxy]{dist}}, so check \code{?proxy::dist} for
full details.}

\item{algorithm}{One of \code{c("clara","clarans")}}

\item{samples}{Number of subsamples (only if \code{algorithm = "clara"})}

\item{sample_size}{Number of observations belonging to a sample. If NULL
(default), the minimum of \code{nrow(data)} and \code{40 + clusters * 2} is
used as sample size. (only if \code{algorithm = "clara"})}

\item{max_neighbors}{Maximum number of randomized medoid searches with each
cluster (only if \code{algorithm = "clarans"})}

\item{num_local}{Number of clustering iterations (only if
\code{algorithm = "clarans"})}

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

\item{scale}{Scale numeric variables before distance matrix calculation?
Default TRUE}

\item{build}{Additional build algorithm to choose initial medoids (only
relevant for type = "fuzzy". Default FALSE.)}

\item{...}{Additional arguments passed to the main clustering algorithm and
to proxy::dist for the calculation of the distance matrix
(\code{\link{pam}} or \code{\link[vegclust]{vegclust}})}
}
\value{
Object of class "fuzzyclara"
}
\description{
Function to perform a cluster analysis in a hard or fuzzy way. The
function can either be performed using a common dissimilarity metric or
a self-defined distance function.
}
\details{
If the clustering is run on mulitple cores, the verbose messages are printed
in a file \code{clustering_progress.log} (if \code{verbose > 0}).
}
\examples{
 
# Hard clustering
cc_hard <- fuzzyclara(data        = USArrests,
                      clusters    = 3,
                      metric      = "euclidean",
                      samples     = 1,
                      sample_size = NULL,
                      type        = "hard",
                      seed        = 3526,
                      verbose     = 0)
cc_hard

# Fuzzy clustering
cc_fuzzy <- fuzzyclara(data        = USArrests,
                       clusters    = 3,
                       metric      = "euclidean",
                       samples     = 1,
                       sample_size = NULL,
                       type        = "fuzzy",
                       m           = 2,
                       seed        = 3526,
                       verbose     = 0)
cc_fuzzy

# Fuzzy clustering with self-defined distance function
dist_function <- function(x, y) {
sqrt(sum((x - y)^2))
}

cc_dist <- fuzzyclara(data        = USArrests,
                      clusters    = 3,
                      metric      = dist_function,
                      samples     = 1,
                      sample_size = NULL,
                      type        = "fuzzy",
                      m           = 2,
                      seed        = 3526,
                      verbose     = 0)
cc_dist

# Hard clustering with other distance function
cc_manh <- fuzzyclara(data        = USArrests,
                      clusters    = 3,
                      metric      = "manhattan",
                      samples     = 1,
                      sample_size = NULL,
                      type        = "hard",
                      seed        = 3526,
                      verbose     = 0)

cc_manh

# Hard clustering with Minkowski distance
# In order to specify arguments of the distance metric (e. g. p for
# Minkowski distance), 
# you can use a self-defined distance function.

dist_mink <- function(x, y) {
proxy::dist(list(x, y), method = "minkowski", p = 1)
}
cc_mink <- fuzzyclara(data        = USArrests,
                      clusters    = 3,
                      metric      = dist_mink,
                      samples     = 1,
                      sample_size = NULL,
                      type        = "hard",
                      seed        = 3526,
                      verbose     = 0)
cc_mink

}
\references{
Kaufman, L., and Rousseeuw, P. J. (1986). Clustering large data sets.
\emph{Pattern Recognition in Practice}, 425--437.

Ng, R. T., and Han, J. (2002). CLARANS: A method for clustering
objects for spatial data mining. \emph{IEEE transactions on knowledge and data engineering},
14(5), 1003--1016. \doi{10.1109/tkde.2002.1033770}.
}

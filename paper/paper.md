---
title: 'fuzzyclara: Efficient Medoid-based Clustering Algorithms for Large and Fuzzy Data'

tags:
  - R
  - Cluster analysis
  - High-dimensional data
authors:
  - name: Maximilian Weigert
    orcid: 0000-0001-6783-1421
    affiliation: 1
  - name: Alexander Bauer
    orcid: 0000-0003-3495-5131
    affiliation: 1
  - name: Jana Gauß
  - name: Asmik Nalmpatian
    orcid: 0000-0003-3495-5131
    affiliation: 1
affiliations:
 - name: Statistical Consulting Unit StaBLab, Department of Statistics, LMU Munich, Germany
   index: 1
date: 30 August 2022
bibliography: paper.bib
---

# Summary

Cluster analysis aims to find reasonable groupings of a set of observations
that resemble their characteristics (REFERENCE?).
The `fuzzyclara` package comprises routines to cover the whole workflow for
real-world clustering applications.
This includes both the estimation of hard and fuzzy clusterings,
the inclusion of subsampling-based estimation algorithms that make the
estimation on large data feasible, and the inclusion of general
convenience functionalities and visualization techniques.


# Statement of Need

Apart from offering general functions for clustering, the `fuzzyclara` package
specifically tackles two issues of cluster analysis applications.
First, it includes routines for fuzzy clustering which avoid the common hard
clustering assumption that each observation is a clear member of one sole cluster.
Instead, membership probabilities indicate to which extent the characteristics
of each observation are shaped by the characteristics of several 'typical' clusters.
Second, the estimation of classical clustering algorithms is often only hardly
or not at all feasible in large data situations with thousands of observations.
Sampling-based algorithms building on the CLARA algorithm are implemented to
make the estimation feasible in such situations.
Building on these two points, the 'fuzzyclara' package offers routines for all
aspects of a cluster analysis, including the use of user-defined distance
functions and diverse visualization techniques.

The concepts of fuzzy clustering. Together with this, we aim to provide a package
reflecting the pipeline for performing 
\\ R offers several packages containing. 
\\ Absatz zu existierenden Paketen: `cluster`,
\begin{itemize}
\item Allgemeine Pakete für partionierendes Cluster: `cluster` [@R_cluster]
\item Fuzzy Clustering: `fclust` [@R_fclust], `vegclust` [@R_vegclust], ...
\item Subsampling: clara (`cluster`-Paket) [@R_cluster], clarans im Paket `qtcat` [@R_qtcat]
\item `fastkmedoids` enthält wohl auch (Versionen von) CLARA und CLARANS [@R_fastkmedoids]
\end{itemize}
However, a combination of both strategies is not available.

# Combination of fuzzy and CLARA clustering
For our we build on the clara clustering algorithm which was originally
designed as a efficient variant of the partitioning around medoids (PAM) algorithm
\cite{kaufmann}.
- Algorithmus/Flowchart
Instead of a hard clustering method, we apply the fuzzy-k-medoids algorithm
(Krishnapuram et al.) on each subsample of the data. In accordance to the
clara algorithm, the best subsample is then chosen based on all data. For each
subsample, 
The subset of 
medians which minimizes the sum of minimal weighted distances is selected for
the final clustering solutions. 

chosen in accordance to 
Formel für Verrechnung
\begin{equation}
m = 
\end{equation}

# Clustering pipeline

# Application
To showcase the functionality of our clustering package, we apply fuzzyclara
clustering to the USArrests data available in clustering.
Grpahik 1: Optimale Clusteranzahl
Graphik 2: Wordcloud, PCA-Plot (differenziert nach Kerncluster)




# Acknowledgments

This work has been partially funded by the German Research Foundation (DFG) under Grant No. KU 1359/4-1 and by the German Federal Ministry of Education and Research (BMBF) under Grant No. 01IS18036A.

# References

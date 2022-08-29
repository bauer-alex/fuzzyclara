---
title: 'fuzzyclara: Efficient medoid-based clustering algorithms for large and fuzzy data'

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
date: 30 June 2022
bibliography: paper.bib
---

# Summary
Partitioning clustering The R package \code{fuzzyclara} offers a flexible framework to c

# Statement of Need

Statement of the problem, partitioning cluster analysis for large
data, fuzzy algorithms, general package for whole cluster analysis process,
own distance function

Motivation: Clustering problems can be fuzzy. For large datasets 

The concepts of fuzzy clustering. Together with this, we aim to provide a package
reflecting the pipeline for performing 
\\ R offers several packages containing. 
\\ Absatz zu existierenden Paketen: cluster,
\begin{itemize}
\item Allgemeine Pakete für partionierendes Cluster: cluster
\item Fuzzy Clustering: fclust, vegclust, ...
\item Subsampling: clara (cluster-Paket), clarans
\end{itemize}
However, a combination of both strategies is not available.

# Combination of fuzzy and CLARA clustering
For our we build on the clara clustering algorithm which was originally
designed as a effficient variant of the partitioning around medoid algorithm
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




# Acknowledgements

# References

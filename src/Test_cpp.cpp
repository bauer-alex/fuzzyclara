#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double dist_tourist(DataFrame x, DataFrame y, DataFrame dist_file) {
  
  /* Accessing columns of distance file */
  CharacterVector name = dist_file["Variable"];
  NumericVector weight = dist_file["weight_total"];
  CharacterVector level = dist_file["Skalenniveau"];
  
  /* Definition of distance for individual variables: */
  NumericVector dist_individual = dist_file.nrow();
  for (int i = 0; i < dist_file.nrow(); ++i) {
    
    /* Continuous variables */
    if (level(i) == "metrisch") {
      NumericVector xi = x[i];
      NumericVector yi = y[i];
      if (!NumericVector::is_na(xi(0)) && !NumericVector::is_na(yi(0))) {
        dist_individual(i) = weight(i) * sqrt(pow(xi(0) - yi(0), 2.0));
      }
    }
    
    /* Nominal or ordinal variables */
    if (level(i) == "nominal" || level(i) == "ordinal") {
      CharacterVector xi = x[i];
      CharacterVector yi = y[i];
      dist_individual(i) = 0;
      if (xi(0) != yi(0)) {
        dist_individual(i) = weight(i) * 1;
      }
      if (CharacterVector::is_na(xi(0)) && CharacterVector::is_na(yi(0))) {
        dist_individual(i) = 0;
      }
      if (CharacterVector::is_na(xi(0)) || CharacterVector::is_na(yi(0))) {
        dist_individual(i) = 0;
      }
    }
  }
  
  /* Return of sum of all distances: */
  double dist = sum(dist_individual);
  return dist;
}






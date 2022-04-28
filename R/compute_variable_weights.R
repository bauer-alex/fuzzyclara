#' Computation of individual weights for each variable of a distance file
#'
#' Function to compute individual weights for variables given by a distance
#' file
#'
#' @param dist_file data.frame containing information about variables
#' supposed to be used for clustering
#' @return dist_file with new column "weight_total"
#' @import dplyr
#' @export
compute_variable_weights <- function(dist_file) {

  # Input checking:
  # TO DO

  # Total weight for each variable of distance file:
  dist_file <- dist_file %>% mutate(weight_total = 1)


  # Computation of individual weights of all variables:

  # Weighting of all "Oberbereiche":
  number_oberbereiche <- length(unique(dist_file$Oberbereich))
  dist_file <- dist_file %>%
    mutate(weight_total = weight_total * Oberbereichsgewicht *
             (1 / number_oberbereiche))

  # Weighting of all "Bereiche" within a "Oberbereich":
  for (i in unique(dist_file$Oberbereich)) {
    index_oberbereich <- which(dist_file$Oberbereich == i)
    number_bereiche <- length(unique(dist_file$Bereich[index_oberbereich]))
    dist_file$weight_total[index_oberbereich] <-
      dist_file$weight_total[index_oberbereich] *
      dist_file$Bereichsgewicht[index_oberbereich] *
      (1 / number_bereiche)

    # Weighting of all individual variables within a "Bereich":
    for (j in unique(dist_file$Bereich[dist_file$Oberbereich == i])) {
      index_bereich <- which(dist_file$Bereich == j)
      number_variables <- nrow(dist_file[dist_file$Bereich == j, ])
      dist_file$weight_total[index_bereich] <-
        dist_file$weight_total[index_bereich] *
        dist_file$Gewicht[index_bereich] * (1 / number_variables)
    }
  }

  # Return of modified distance file:
  return(dist_file$weight_total)
}




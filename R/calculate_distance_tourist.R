#' Construction of distance function for tourist data
#'
#' Function to define a valid distance function for the tourist dataset
#'
#' @param x observation 1
#' @param y observation 2
#' @param dist_file path to overview file about variables
#' @return distance between two observations
#' @import dplyr readxl Rcpp
#' @export
calculate_distance_tourist <- function(x, y, dist_file) {

  # Input checking:
  # TO DO

  # Definition of distance for individual variables:
  distances_individual <- lapply(X = 1:nrow(dist_file), FUN = function(i) {

    # Extraction of name and weight:
    name <- dist_file$Variable[i]
    weight <- dist_file$weight_total[i]

    if(dist_file$Skalenniveau[i] == "nominal mehrfach") {
      name <- substr(unlist(strsplit(name, ", "))[1], 1, nchar(unlist(strsplit(name, ", "))[1]) - 1)
      x_values <- x[grepl(name, names(x))]
      y_values <- y[grepl(name, names(y))]
    } else {
      x_value <- x[name]
      y_value <- y[name]
    }

    # Continuous variables:
    if (dist_file$Skalenniveau[i] %in% c("metrisch", "ordinal")) {
      # Manhattan distance:
      if (!is.na(x_value) && !is.na(y_value)) {
        dist_individual <- weight * sqrt((x_value - y_value)^2)
      } else { # TODO Error handling (preliminary):
        dist_individual <- 0
        names(dist_individual) <- name
      }
    # } else if (dist_file$Skalenniveau[i] == "ordinal") {
    #   if (!is.na(x_value) && !is.na(y_value)) {
    #
    #     # Convert to metric
    #     x_value_metric <- as.integer(unlist(x[[name]], use.names = FALSE))
    #     y_value_metric <- as.integer(unlist(y[[name]], use.names = FALSE))
    #
    #     # Manhattan distance:
    #     dist_individual <- weight * sqrt((x_value_metric - y_value_metric)^2)
    #
    #   } else { # TODO Error handling (preliminary):
    #     dist_individual <- 0
    #     names(dist_individual) <- name
    #   }
    #
    #   # TO DO (use ordinal structure)
    } else if (dist_file$Skalenniveau[i] == "nominal") {
      # Identity:
      dist_individual <- weight *
        ifelse(test = x_value == y_value, yes = 0, no = 1)
      # Error handling (preliminary):
      if (sum(is.na(x_value), is.na(y_value)) == 1) {
        dist_individual <- 1
      }
      if (sum(is.na(x_value), is.na(y_value)) == 2) {
        dist_individual <- 0
      }
    } else if (dist_file$Skalenniveau[i] == "nominal mehrfach") {
      # TO DO (problem: more dummies belonging to the same variable)
      possibilities <- length(x_values)

      if(sum(is.na(x_values), is.na(y_values)) == 2 * possibilities) {
        dist_individual <- 0
      } else {

        # currenly M coefficient
        confusion_table <- table(x_values == y_values)
        matches <- confusion_table["TRUE"]
        no_matches <- confusion_table["FALSE"]


        s_coef <- as.numeric(matches / (matches + no_matches))
        dist_individual <- weight * s_coef
        }

      }

    return(dist_individual)
  })

  # Return of sum of all distances:
  distance <- sum(unlist(distances_individual))
  return(distance)
}


#' Construction of distance function for tourist data
#'
#' Function to adjust the variable names, especially for "nominal mehrfach" version
#'
#' @param dist_file variable names column of dist_file
#' @return variable names corresponding to the names in the tourist data
#' @import dplyr
#' @export
#'
prepare_variable_names <- function(dist_file) {

  original_variable_names <- dist_file$Variable

  if(sum(dist_file$Skalenniveau %in% "nominal mehrfach") > 0) {

    index <- which(dist_file$Skalenniveau %in% "nominal mehrfach")
    mehrfach_info <- unlist(strsplit(original_variable_names[index] , ", "))
    string <- substr(mehrfach_info[1],1,nchar(mehrfach_info[1])-1)
    length <- as.numeric(mehrfach_info[2])
    mehrfach_variables <- paste0(string,seq(1:length))

    dist_file_variables <- c(original_variable_names[-index],
                             mehrfach_variables)

    return(dist_file_variables)

  } else {
    return(original_variable_names)
  }
}




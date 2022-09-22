
library(dplyr)


# read the data from the 'data-raw' folder --------------------------------
# travel behavior of German travelers
travel <- readRDS("Reiseanalyse_sample.rds")



# save the data in the package --------------------------------------------
usethis::use_data(travel, overwrite = TRUE)

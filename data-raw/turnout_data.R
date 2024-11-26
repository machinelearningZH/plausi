# INFO =========================================================================


# This is how to update the package data "turnout_data".


# GET AND SAVE DATA ============================================================


# install the swissdd package from github
install.packages("devtools")
devtools::install_github("politanch/swissdd")

# access data from opendata.swiss via swissdd package
turnout_data_raw <- swissdd::get_nationalvotes(votedates = c("2020-09-27"))

# drop cantons
turnout_data <- prediction_data_raw[prediction_data_raw$canton_name == "Zürich", ]

# save data
usethis::use_data(turnout_data, overwrite = TRUE)

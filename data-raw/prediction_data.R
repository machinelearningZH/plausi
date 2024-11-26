# INFO =========================================================================


# This is how to update the package data "prediction_data".


# GET AND SAVE DATA ============================================================


# install the swissdd package from github
install.packages("devtools")
devtools::install_github("politanch/swissdd")

# access data from opendata.swiss via swissdd package
prediction_data_raw <- swissdd::get_nationalvotes(from_date = "2017-03-01", to_date = "2020-09-27")

# drop cantons
prediction_data <- prediction_data_raw[prediction_data_raw$canton_name == "Zürich", ]

# save data
usethis::use_data(prediction_data, overwrite = TRUE)

# INFO =========================================================================


# This is how to update the package data "result_data".


# GET AND SAVE DATA ============================================================


# install the swissdd package from github
install.packages("devtools")
devtools::install_github("politanch/swissdd")

# access data from opendata.swiss via swissdd package
result_data_raw <- swissdd::get_nationalvotes(from_date = "2017-03-01", to_date = "2020-09-27")

# drop cantons
result_data <- result_data_raw[result_data_raw$canton_name == "Zürich", ]

# turn into data.frame
result_data <- as.data.frame(result_data)

# save data
usethis::use_data(result_data, overwrite = TRUE)

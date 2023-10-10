library(targets)
library(dplyr)
library(edmaps)
library(parallelly)
library(crew)

n_cores <- max(
  1, floor(parallelly::availableCores(constraints = "multicore") * 0.5)
)

tar_option_set(
  packages = c("terra", "edmaps", "future", "future.apply", "dplyr"),
  controller = crew_controller_group(
    crew::crew_controller_local("controller_multi", workers = n_cores),
    crew::crew_controller_local("controller_single", workers = 1)
  ),
  resources = tar_resources(
    crew = tar_resources_crew(controller = "controller_multi")
  )
)

message("no. of cores: ", n_cores)

# source("R/get_gbif_records.R")
make_edmaps(system.file("extdata", "parameters.xlsx", package = "edmaps"))


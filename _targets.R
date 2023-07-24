library(targets)
library(dplyr)
library(crew)
library(parallelly)
library(edmaps)

n_cores <- max(1, floor(parallelly::availableCores(constraints='multicore') * 0.75))
tar_option_set(packages = c('terra', 'edmaps', 'future', 'future.apply', 'dplyr'),
               controller=crew::crew_controller_local(workers=n_cores))
message('no. of cores: ', n_cores)
source('R/get_gbif_records.R')
source('functions.R')
make_plan('inst/extdata/parameters.xlsx')

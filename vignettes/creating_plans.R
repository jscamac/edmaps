## ---- echo=FALSE---------------------------------------------------------
library(edmaps)

## ------------------------------------------------------------------------
dorsalis <- species_plan(
  species='Bactrocera dorsalis',
  aggregated_res=c(10000, 10000),
  clum_path='risk_layers/biotic/raw_data/ACLUM/clum_50m1218m.tif',
  nvis_path='risk_layers/biotic/raw_data/NVIS_5.1/aus5_1e_mvs',
  ndvi_path='risk_layers/biotic/raw_data/NDVI/NDVI_Oct18_Mar19.grid',
  fertiliser_data_path='risk_layers/pathway/raw_data/Fertiliser/fertiliser2016_17.csv',
  nrm_path='risk_layers/pathway/raw_data/Fertiliser/nrm_regions/NRMR_2016_AUST.shp',
  containers_data_path='risk_layers/pathway/raw_data/Containers/containers_bypostcode.xls',
  postcode_path='risk_layers/pathway/raw_data/Containers/postal_areas/POA_2011_AUST.shp', 
  clum_classes=c(340, 341, 342, 344, 345, 347, 348, 349, 350, 351, 353, 365,
                 440, 441, 442, 444, 445, 447, 448, 449, 450, 451, 453, 540, 
                 541, 542, 543, 544),
  pathways=c('tourists', 'residents', 'torres'),
  include_abiotic_weight=TRUE,
  occurrence_path='risk_layers/abiotic/occurrences/oriental_fruitfly/dorsalis_occurrences.csv',
  cabi_path='risk_layers/abiotic/occurrences/oriental_fruitfly/cabi_dorsalis_20April19.csv',
  use_gbif=TRUE,
  manual_check_flagged_records=FALSE,
  gbif_species=c("Bactrocera dorsalis","Bactrocera invadens", 
                 "Bactrocera papayae", "Bactrocera philippinensis"),
  total_tourists=13941270,
  prob_tourists=2.381577e-06*(30/45),
  total_returning=15486050,
  prob_returning=2.381577e-06*(15/45),
  total_torres=51000,
  prob_torres=21/51000)

## ------------------------------------------------------------------------
# Let's take a look
dorsalis

# List the targets
dorsalis$target

## ---- eval=FALSE---------------------------------------------------------
#  f <- system.file('extdata/parameters.xlsx', package='edmaps')
#  all <- excel_to_plan(f)
#  
#  # Let's take a look
#  all
#  
#  # List the targets
#  all$target


library( terra )
library( sf )
library( doSNOW )
library( cppRouting )
library( RcppParallel )

## ------------------------------- USER INPUT ----------------------------------

## Inputs
rdName                 <- "CompiledData/NV_Analysis/NV_Analysis_Roads_UTM11N.shp" 
rdField                <- "MAX_MPH"
facilityName           <- "CompiledData/NV_Analysis/mills.gpkg"
facilityField          <- "PSITE_ID"
resourceName           <- "CompiledData/NV_Analysis/NV_Plots_withCN_UTM11N.shp"
resourceField          <- "PLOT_CN"
useCompiledCode        <- FALSE ## logical
rasterResolution       <- 50 ## meters
nIterations            <- 10
connectedNodeThreshold <- 0.75

## Outputs
roadRasterName         <- "CompiledData/NV_Analysis/NV_rd_speed_50m_test.tif"
movedFacilityName      <- "CompiledData/NV_Analysis/NV_moved_mills_50m_test.gpkg"
movedResourceName      <- "CompiledData/NV_Analysis/NV_moved_plots_50m_test.gpkg"
outCSVName             <- "CompiledData/NV_Analysis/NV_cost_50m_test.csv"


## ---------------------------- END OF USER INPUT ------------------------------

source( "BIOSUM_functions.R" )

checkInputs( rdName, rdField,
             facilityName, facilityField,
             resourceName, resourceField,
             rasterResolution, roadRasterName,
             movedFacilityName, movedResourceName,
             useCompiledCode, 
             outCSVName )
rasterizeRoads( rdName, rdField, rasterResolution, roadRasterName )
graph <- createGraph( roadRasterName, nIterations, connectedNodeThreshold, useCompiledCode )
movePt2RdSegment( roadRasterName, facilityName, movedFacilityName )
movePt2RdSegment( roadRasterName, resourceName, movedResourceName )
checkConnectivity( roadRasterName, movedFacilityName )
checkConnectivity( roadRasterName, movedResourceName )
calculateCost( graph, roadRasterName,
               movedFacilityName, facilityField,
               movedResourceName, resourceField,
               outCSVName )

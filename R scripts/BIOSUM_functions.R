stopFun <- function( ) {
  flag <- TRUE
  answer <- readline( prompt="\rContinue? (Y/N)  " )
  if( toupper(answer) %in% c("Y", "YES") ) {
    cat( "\nContinuing ...  \n" )
    flag <- FALSE
  }
  if( toupper(answer) %in% c("N", "NO") )
    stop( "\rExecution interrupted by user\n\n")
  if( flag ) {
    cat( "\r" )
    stopFun()
  }
}

checkInputs <- function( rdName, rdField,
                         facilityName, facilityField,
                         resourceName, resourceField,
                         res, roadRasterName,
                         movedFacilityName, movedResourceName,
                         compileRcpp,
                         outCSVName ) {

  warningFlag <- FALSE
  
  ## input spatial info
  s      <- c( rdName, facilityName, resourceName )
  sAtt   <- list( c("LINESTRING", "MULTILINESTRING"), "POINT", "POINT" )
  sNames <- c( rdField, facilityField, resourceField )
  for( j in s ) {
    if( !file.exists(j) )
      stop( "\ncheckInputs(): ", j, " does not exist\n" ) 
  }

  ## check if spatial inputs have .shp or .gpkg extenstion
  for( j in 1:length(s) ) {
    if( toupper(substring(s[j], nchar(s[j])-3, nchar(s[j]))) != ".SHP" &
        toupper(substring(s[j], nchar(s[j])-4, nchar(s[j]))) != ".GPKG" )
      stop( "\ncheckInputs(): ", s[j], " should be either a shapefile (.shp extension) or a geopackage (.gpkg extension)\n" )
  }
  
  ## input fields and projection
  compareList <- list()
  for( j in 1:length(s) ) {
    if( toupper(substring(s[j], nchar(s[j])-3, nchar(s[j]))) == ".SHP" )
      layerName <- gsub( ".SHP", "", toupper(basename(s[j])) )
    if( toupper(substring(s[j], nchar(s[j])-4, nchar(s[j]))) == ".GPKG" )
      layerName <- gsub( ".GPKG", "", toupper(basename(s[j])) )
    shpInfo <- sf::st_read( s[j],
                            query=sprintf("SELECT * FROM %s LIMIT 1", layerName ),
                            quiet=TRUE )
    if( ( sf::st_geometry_type(shpInfo) %in% sAtt[[j]] ) == FALSE ) {
      stop( "\ncheckInputs(): ", s[j], " should be of ", paste0(sAtt[[j]], sep= " "), "type. It is ",
            as.character(sf::st_geometry_type(shpInfo)), " instead\n" )
    }
    if( ( sNames[j] %in% names(shpInfo) == FALSE ) ) {
      stop( "\ncheckInputs(): Attribute ", sNames[j], " does not exist in ", s[j], "\n" )
    }
    compareList[[j]] <- shpInfo
  }
  ## does projection support terra::linearUnits()?
  unit <- linearUnits( rast( crs=crs(compareList[[1]]) ) )
  if( (abs(unit - 0.3048) > 0.0001) & (abs(unit - 1) > 0.0001) )
    stop( "\ncheckInputs(): Unit for ", rdName, " should be either meters or feet\n" )
  
  if( (st_crs(compareList[[1]])$input != st_crs(compareList[[1]])$input) |
      (st_crs(compareList[[1]])$units != st_crs(compareList[[1]])$units)    ) ## minimal test
    stop( "\ncheckInputs(): ", s[1], " and ", s[2], " do not share the same projection\n" )
  if( (st_crs(compareList[[1]])$input != st_crs(compareList[[3]])$input) |
      (st_crs(compareList[[1]])$units != st_crs(compareList[[3]])$units) )
    stop( "\ncheckInputs(): ", s[1], " and ", s[3], " do not share the same projection\n" )
  
  ## output
  s <- c( movedFacilityName, movedResourceName )
  for( j in s ) {
    if( file.exists(j) ) {
      warning( "\ncheckInputs(): ", j, " already exists and will be overwritten\n", immediate.=TRUE, call.=FALSE )
      warningFlag <- TRUE
    }
  }
  
  ## TIF, output of road rasterization
  if( toupper( substring( roadRasterName, nchar(roadRasterName)-3, nchar(roadRasterName) ) ) != ".TIF" )
    stop( "\ncheckInputs(): ", roadRasterName, " does not have .tif extension" )
  if( file.exists(roadRasterName) ) {
    warning( "\ncheckInputs(): ", roadRasterName, " already exists and will be overwritten\n", immediate.=TRUE, call.=FALSE )
    warningFlag <- TRUE
  }
  if( file.exists(outCSVName) ) {
    warning( "\ncheckInputs(): ", outCSVName, " already exists and will be overwritten\n", immediate.=TRUE, call.=FALSE )
    warningFlag <- TRUE
  }
  
  ## numeric parameters
  if( !is.numeric(res) | res <= 0.0 )
    stop( "\ncheckInputs(): Raster resolution should be positive. You specified ", res, "\n" )
  
  ## logical parameters
  if( !is.logical(compileRcpp) )
    stop( "\ncheckInuts():", compileRcpp, "should be either TRUE or FALSE\n" )
  if( compileRcpp ) {
    if( !file.exists("BIOSUM_functions.cpp") )
      stop( "\ncheckInputs(): BIOSUM_functions.cpp does not exist\n" )
    else {
      cat("\ncheckInputs(): compiling BIOSUM_functions.cpp ...\n" )
      sourceCpp( "BIOSUM_functions.cpp" )
    }
  }

  ## value uniqueness for input shapefile fields
  s       <- c( facilityName, resourceName )
  sFields <- c( facilityField, resourceField )
  for( j in 1:2 ) {
    shp <- vect( s[j] )
    v <- data.frame( shp[, sFields[j]] )
    if( length(v) != length( unique(v) ) )
      stop( "\ncheckInputs(): Values in field ", sFields[j], " of file ", s[j], " are not unique\n")
  }
  
  if( warningFlag ) stopFun()
}

reportTime <- function( t1, t2 ) {
  s <- as.numeric( as.character( round( difftime( t2, t1, units="secs"), 2 ) ) )
  sUnit <- "second(s)"
  if( s > 60 ) { s <- round( s / 60.0, 2 ); sUnit <- "minute(s)" }
  if( s > 60 ) { s <- round( s / 60.0, 2 ); sUnit <- "hour(s)" }
  return( paste( s, sUnit ) )
}

setCellValueToInteger <- function( r, name ) {
  cat( "\nsetCellValueToInteger(): setting cell values to integers to conserve memory ..." ); flush.console()
  if( ncell(r) < (2^31-1) ) {
    setValues( r, as.integer(values(r)) )
    writeRaster(r, filename=name, overwrite=TRUE, datatype="INT1U" )
  } else {
    cat("\n")
    warning( "setCellValueToInteger(): owing to its size, ", ncell(r), " cells, ", 
             "the raster will be processed in tiles", immediate.=TRUE, call.=FALSE )
    
    tmpName <- basename( tempfile() )
    tileNames <- makeTiles( r, y=c(40000,40000),
                            filename=paste0(tmpName, "_tile_.tif") )
    for( j in 1:length(tileNames) ) {
      tile.r <- rast( tileNames[j] )
      setValues( tile.r, as.integer(values(tile.r)) )
      invisible( gc() )
      writeRaster( tile.r, gsub(".tif", "_int.tif", tileNames[j]), datatype="INT1U" )
      rm( tile.r ); invisible( gc() )
      unlink( tileNames[j] )
    }
    fl <- list.files( pattern=tmpName )
    fl <- fl[substring(fl, nchar(fl)-3, nchar(fl)) == ".tif"] # avoid .ovr, .xml, etc
    r <- vrt( fl )
    writeRaster(r, filename=name, overwrite=TRUE, datatype="INT1U" )
    unlink( list.files(pattern=tmpName) )
    invisible( gc() )
  }
}

rasterizeRoads <- function ( shpName, shpField, res, rasterName ) {

  startTime <- Sys.time()
  
  cat( "\nrasterizeRoads(): reading", shpName, "..." ); flush.console()
  shp <- vect( shpName )
  shp.ext <- ext( shp )
  
  shpValRange <- as.numeric( range( shp[, shpField] ) )
  if( shpValRange[1] <= 0.0 | shpValRange[2] > 199.0 ) {
    stop( "\n\nrasterizeRoads(): ", shpField, " values in ", shpName, " should be in (0,199]\n\n" )
  }
  
  ## modify bounding box to ensure origin is at [0,0]. Include one cell wide border
  if( shp.ext[1] %% res == 0.0 ) shp.ext[1] <- shp.ext[1] - 0.0001 ## xmin
  if( shp.ext[3] %% res == 0.0 ) shp.ext[3] <- shp.ext[3] - 0.0001 ## ymin
  if( shp.ext[2] %% res == 0.0 ) shp.ext[2] <- shp.ext[2] + 0.0001 ## xmax
  if( shp.ext[4] %% res == 0.0 ) shp.ext[4] <- shp.ext[4] + 0.0001 ## ymax
  
  shp.ext[1] <- floor(   shp.ext[1] / res ) * res - res ## xmin
  shp.ext[3] <- floor(   shp.ext[3] / res ) * res - res ## ymin
  shp.ext[2] <- ceiling( shp.ext[2] / res ) * res + res ## xmax
  shp.ext[4] <- ceiling( shp.ext[4] / res ) * res + res ## ymax
  
  r <- rast( shp.ext, res=res )
  crs( r ) <- crs( shp )
  
  mem_info( r )
  stopFun()

  invisible( gc() )
  
  cat( "\nrasterizeRoads(): performing rasterization ..." ); flush.console()
  r <- rasterize( shp, r, field=shpField, fun=max, filename=rasterName, background=255, wopt=list(datatype="INT1U", NAflag=255), overwrite=T )

  cat( "\nrasterizeRoads(): Output saved as", rasterName ); flush.console()
  cat( paste( "\nrasterizeRoads(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" ) )
}

checkBackgroundCellValue <- function( rasterName ) {
  startTime <- Sys.time()
  cat( "\ncheckBackgroundCellValue(): Checking validity of", rasterName, "background cell values" ); flush.console()
  r <- rast( rasterName )
  tbl <- table( values( r ) ) / ncell( r )
  df <- data.frame( cbind( as.numeric( names( tbl ) ), tbl ) )
  names( df ) <- c( "speed", "ratio" )
  w <- df$ratio > 0.85
  if( nrow( df[w,] ) > 0 ) {
    v <- df$speed[w]
    cat( "\ncheckBackgroundCellValue(): Converting", rasterName, "background value from", v, "to NA" ); flush.console()
    r[r == v] <- NA
    cat( "\ncheckBackgroundCellValue(): Overwriting", rasterName, "...\n" ); flush.console()
    writeRaster( r, rasterName,
                 datatype="INT1U",
                 gdal=c( "COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "NUM_THREADS=ALL_CPUS" ),
                 overwrite=TRUE )
  }
  cat( "\ncheckBackgroundCellValue(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" )
}

prepareGraph <- function( rasterName, compileCpp ) {
  startTime <- Sys.time()
  cat( "\n\tprepareGraph(): processing ..." ); flush.console()
  r  <- rast( rasterName )
  if( compileCpp ) {
    options(warn=-1)
    cellValueMat = readTIFF( rasterName, as.is=TRUE) ## raster should be of integer type
    ## NA values are treated as 255 by readTIFF()
    options(warn=0)
    cat( "\n\tprepareGraph(): calculating node-to-node cost ..." ); flush.console()
    df <- rcpp_buildNodeDF(cellValueMat, res(r)[1], linearUnits(r) )
  } else {
    nc <- ncol(r)
    nr <- nrow(r)
  
    from <- cells( r )
    n.cells <- length(from)
    sqrt.v <- rep(sqrt(2), n.cells)
    one.v  <- rep(1, n.cells)
    nw <- from-nc-1
    n  <- from-nc
    ne <- from-nc+1
    w  <- from-1
    
    cat( "\n\tprepareGraph(): merging neighbor nodes ..." ); flush.console()
    df <- data.frame(from = rep(from,4),
                     to   = c(nw, n, ne, w),
                     d2n  = c(sqrt.v, one.v, sqrt.v, one.v))
    rm(sqrt.v, one.v, nw, n, ne, w)
    df <- df[!is.na( r[df[,2]] ),]
    names(df) <- c("from", "to", "d2n")
    invisible( gc() )
    
    cat( "\n\tprepareGraph(): calculating node-to-node cost ..." ); flush.console()
    scaleValue <- 3600 * res(r)[1] * linearUnits(r) * 2 / 1609.34
    df$cost <- scaleValue * df$d2n / ( r[df$from][,1] + r[df$to][,1] )
    df$d2n <- NULL
    invisible( gc() )
  }
  cat( paste( "\n\tprepareGraph(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" ) )
  return( df )
}

maskCells <- function( rasterName, IDs, tmpName ) {
  r <- rast( rasterName )
  p <- vect( xyFromCell( r, IDs ), type="points", crs=crs(r) )
  r <- mask( r, p, inverse=TRUE )
  writeRaster( r, tmpName, datatype="INT1U" )
}

identifyDisconnectedSegments <- function( rasterName, graph, node.df, numberOfIterations=10, nodeThreshold=0.75 ) {
  # if( missing(numberOfIterations) ) numberOfIterations <- 10
  # if( missing(nodeThreshold) ) nodeThreshold <- 0.75
  
  startTime <- Sys.time()
  flag <- TRUE
  s <- node.df[sample(1:nrow(node.df), nIterations), "nodeIDs"]
  j <- 1
  while( flag ) {
    cat( "\n\tidentifyDisconnectedSegments()" )
    if( j > 1) cat( ": Iteration", j )
    flush.console()
    iso <- get_isochrone( graph, from=s[j], lim=1E12, long=FALSE )
    if( length( iso[[as.character(s[j])]] ) > ( nrow(node.df) * nodeThreshold ) ) {
      flag <- FALSE
      disconnectedNodeIDs <- setdiff( node.df$nodeIDs, as.numeric(iso[[as.character(s[j])]]) )
      if( length( disconnectedNodeIDs ) > 0 ) {
        cat( "\n\tidentifyDisconnectedSegments(): found", length(disconnectedNodeIDs), "disconnected segments"); flush.console()
        disconnectedNodes <- xyFromCell( rast(rasterName), disconnectedNodeIDs )
        disconnectedNodes <- vect( disconnectedNodes[, c("x","y")], type="points", crs=crs( rast(rasterName )) )
        outName <- paste0("dc_", basename(tempfile()), ".gpkg")
        writeVector( disconnectedNodes, outName )
        cat( "\n\tidentifyDisconnectedSegments(): Identified segments saved as", outName, "\n" ); flush.console()
      } else {
        cat( "\n\tidentifyDisconnectedSegments(): there are no disconnected segments\n\n" )
        return()
      }
    }
    if( j == numberOfIterations & flag ) {
      cat("\n")
      warning( "identifyDisconnectedSegments(): failed to identify any disconnected road segments after ", numberOfIterations, " iteration(s)", immediate.=TRUE, call.=FALSE )
      cat( "\n\tidentifyDisconnectedSegments(): completed in", reportTime( startTime, Sys.time() ), "\n" )
    }
    j <- j + 1
  }
  if( !flag ) {
    cat( "\n\tidentifyDisconnectedSegments(): removing disconnected segments from", rasterName ); flush.console()
    tmpName <- paste0( basename(tempfile()), ".tif" )
    maskCells( rasterName, disconnectedNodeIDs, tmpName )
    unlink( rasterName )
    file.rename( tmpName, rasterName )
    cat( "\n\tidentifyDisconnectedSegments():", rasterName, "has been updated\n"); flush.console()
  }
  cat( "\n\tidentifyDisconnectedSegments(): completed in", reportTime( startTime, Sys.time() ), "\n" )
}

createGraph <- function( rasterName, nIterations=10, connectedNodeThreshold=0.75, compileCpp ) {
  startTime <- Sys.time()
  graph.df    <- prepareGraph( rasterName, compileCpp )
  cat( "\ncreateGraph(): arranging nodes ..." ); flush.console()
  nodeIDs     <- sort( unique( c( graph.df$from, graph.df$to ) ) )
  nIDs        <- length( nodeIDs )
  node.df     <- data.frame( cbind( nodeIDs, xyFromCell(rast(rasterName), nodeIDs) ) )
  cat( "\ncreateGraph(): assembling nodes onto a graph ..." ); flush.console()
  graph       <- makegraph( graph.df, directed = FALSE, coords = node.df )
  cellIDs     <- cells( rast(rasterName) )
  if( nrow(graph$coords) < length(cellIDs) ) {
    cat( "\ncreateGraph(): checking for orphan graph nodes ..." ); flush.console()
    diffIDs     <- setdiff( cellIDs, graph$coords$nodeIDs )
    tmpName     <- paste0( "orphan_", basename(tempfile()), ".gpkg" )
    orphanMat   <- xyFromCell( rast(rasterName), diffIDs )
    orphanP     <- vect( orphanMat, type="points", crs=crs(rast(rasterName)) )
    writeVector( orphanP, tmpName )
    cat( "\ncreateGraph(): Orphan nodes saved to", tmpName ); flush.console()
    tmpName     <- paste0( basename(tempfile()), ".tif" )
    cat( "\ncreateGraph(): removing", length(diffIDs), "orphan graph nodes ..." ); flush.console()
    maskCells( rasterName, diffIDs, tmpName )
    unlink( rasterName )
    file.rename( tmpName, rasterName )
  }
  identifyDisconnectedSegments( rasterName, graph, node.df, numberOfIterations=nIterations, nodeThreshold=connectedNodeThreshold )
  
  invisible( gc() )
  cat( "\ncreateGraph(): Completed in", reportTime( startTime, Sys.time() ), "\n" )
  return( graph )
}

movePt2RdSegment <- function( rasterName, inName, outName ) {
  startTime <- Sys.time()
  cat( "\nmovePt2RdSegment(): Reading inputs\n\t", inName, "\n\t", rasterName ); flush.console()
  
  p <- vect( inName )
  r <- rast( rasterName )
  
  nPts <- length( p )
  
  angle.rad.vec <- seq(0, 355, 5) * pi / 180
  
  circleFun <- function( buffer, rasterName ) {
    circle.v <- vect( cbind( cos(angle.rad.vec) * buffer,
                             sin(angle.rad.vec) * buffer ),
                      type="polygons", crs=crs(rast(rasterName)) )
    return( circle.v )
  }
  
  nP <- length( p )
  
  cat( "\nmovePt2RdSegment(): Calculating distances ...\n" ); flush.console()
  crds.mat <- crds( p )
  res <- res(rast(rasterName))[1]
  
  cl <- makeCluster( min(10, parallel::detectCores() / 2) )
  registerDoSNOW(cl)
    
  if( nP > 1000 ) {
    pb <- txtProgressBar( max=nP, initial=1, style=3 )
    progress <- function( n ) {
      setTxtProgressBar( pb, n )
    }
  } else {
    progress <- NULL
  }
    
  resList <- foreach( j = 1:nrow(crds.mat), .packages="terra", .options.snow=list(progress=progress) ) %dopar% {
    buffer  <- 10 * res
    circle.v <- shift( circleFun(buffer, rasterName), dx=crds.mat[j,1], dy=crds.mat[j,2] )
    while( is.null( intersect(ext(rast(rasterName)), ext(circle.v)) ) ) {
      ## this while loop is necessary when the buffered point is outside the bounding
      ## box of the road raster. Example: fuzzed FIA plot locations well into the ocean
      buffer = buffer * 5
      circle.v <- shift( circleFun(buffer, rasterName), dx=crds.mat[j,1], dy=crds.mat[j,2] )
    }
    sub.r   <- crop( rast(rasterName), circle.v, mask=TRUE )
    flag <- TRUE
    while( flag ) {
      cellIDs <- cells( sub.r )
      if( length( cellIDs ) > 0 ) {
        flag <- FALSE
        rdCellXY.mat <- xyFromCell( sub.r, cellIDs )
        dist.mat <- distance( matrix(crds.mat[j,], ncol=2), rdCellXY.mat, lonlat=FALSE )
        minDist <- min( dist.mat[1,] )
        w <- dist.mat[1,] == minDist
        newCoords <- matrix(rdCellXY.mat[w,], ncol=2)[1,]
      } else {
        buffer <- buffer * 5
        sub.r   <- crop( rast(rasterName),
                         shift( circleFun(buffer, rasterName), dx=crds.mat[j,1], dy=crds.mat[j,2] ),
                         mask=TRUE )
      }
    }
    c( newCoords, minDist )
  }
    
  if( nP > 1000 ) rm( pb )
  stopCluster(cl)
    
  res.mat <- do.call( rbind, resList )
  p$MOVEDIST <- res.mat[,3]
  p <- vect( res.mat[,1:2], type="points", atts=data.frame(p), crs=crs(p) )

  writeVector( p, outName, overwrite=T )
  
  cat( "\n\nmovePt2RdSegment()" )
  cat( "\n\tmove distance metrics:" )
  cat( "\n\tMean :  ", formatC( mean( p$MOVEDIST ),   format="f", digits=2, width=10 ), "units" )
  cat( "\n\tMedian: ", formatC( median( p$MOVEDIST ), format="f", digits=2, width=10 ), "units" )
  cat( "\n\tMinimum:", formatC( min( p$MOVEDIST ),    format="f", digits=2, width=10 ), "units" )
  cat( "\n\tMaximum:", formatC( max( p$MOVEDIST ),    format="f", digits=2, width=10 ), "units" )
  cat( "\n\tStDev:  ", formatC( sd( p$MOVEDIST ),     format="f", digits=2, width=10 ), "units" )

  cat( "\n\nmovePt2RdSegment(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" )
  flush.console()
  
}

checkConnectivity <- function( rasterName, pName ) {
  r   <- rast( rasterName )
  p   <- vect( pName )
  v   <- extract( r, p )
  w   <- is.na( v[, names(r)[1]] )
  nNA <- length( v[w, names(r)[1]] )
  if( nNA > 0 ) {
    stop( "\ncheckConnectivity(): ", nNA, " Element(s) of ", pName, " is(are) on NA ", rasterName, " cells\n\n" )
  } else {
    cat( "\ncheckConnectivity(): All elements of", pName, "are on value", rasterName, "cells" )
  }
}

calculateCost <- function( graph, rasterName, fromName, fromField, toName, toField, outName ) {
  startTime <- Sys.time()
  cat( "\ncalculateCost(): calculating departure and destination nodes ..." ); flush.console()
  fromCellIDs   <- cellFromXY( rast(rasterName), crds(vect(fromName)) )
  fromIDs       <- data.frame(vect(fromName))[,fromField ]
  toCellIDs     <- cellFromXY( rast(rasterName), crds(vect(toName)) )
  toIDs         <- data.frame(vect(toName))[,toField ]
  cat( "\ncalculateCost(): optimizing graph ..." ); flush.console()
  graph         <- cpp_simplify( graph, keep=c(fromCellIDs, toCellIDs) )
  cat( "\ncalculateCost(): calculating cost matrix ..." ); flush.console()
  RcppParallel::setThreadOptions( parallel::detectCores() )
  mat           <- get_distance_matrix( graph, from=fromCellIDs, to=toCellIDs )
  colnames(mat) <- toIDs
  rownames(mat) <- fromIDs
  mat           <- t( round( mat/3600, 3 ) ) ## convert secs to hours and transpose
  cat( "\ncalculateCost(): writing output ..." ); flush.console()
  write.csv( mat, outName, row.names=T )
  cat( "\ncalculateCost(): Cost matrix saved to", outName ); flush.console()
  cat( "\ncalculateCost(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" )
}

exportGeopackageToShapefile <- function( gpkgName, shpName ) {
  if( !file.exists( gpkgName ) ) {
    stop( "\nexportGeopackageToShapefile(): file", gpkgName, " does not exist\n\n" )
  }
  if( file.exists( shpName ) ) {
    cat( "\n" ); flush.console()
    warning( "exportGeopackageToShapefile(): file", shpName, " already exists and will be overwritten\n\n",
             immediate.=TRUE, call.=FALSE )
    stopFun()
  }

  startTime <- Sys.time()
  cat( "\nExporting Geopackage", gpkgName, "to Shapefile", shpName )
  writeVector( vect(gpkgName), shpName, overwrite=TRUE )
  cat( "\nexportGeopackageToShapefile(): completed in", reportTime( startTime, Sys.time() ), "\n" )
}

getNodeID <- function( rasterName, vectorName, vectorField, vectorValue ) {
  if( !file.exists(rasterName) ) {
    stop( "\ngetNodeID(): ", rasterName, " does not exist\n\n" )
  }
  if( !file.exists(vectorName) ) {
    stop(  "\ngetNodeID(): ", vectorName, " does not exist\n\n" )
  }
  
  if( toupper(substring(rasterName, nchar(rasterName)-3, nchar(rasterName))) != ".TIF")
    stop( "\ngetNodeID(): ", rasterName, " should be either a TIFF file (.tif extension)\n" )
    
    
  if( toupper(substring(vectorName, nchar(vectorName)-3, nchar(vectorName))) != ".SHP" &
      toupper(substring(vectorName, nchar(vectorName)-4, nchar(vectorName))) != ".GPKG" )
    stop( "\ngetNodeID(): ", vectorName, " should be either a shapefile (.shp extension) or a geopackage (.gpkg extension)\n" )
  
  if( toupper(substring(vectorName, nchar(vectorName)-3, nchar(vectorName))) == ".SHP" )
    layerName <- gsub( ".SHP", "", toupper(basename(vectorName)) )
  if( toupper(substring(vectorName, nchar(vectorName)-4, nchar(vectorName))) == ".GPKG" )
    layerName <- gsub( ".GPKG", "", toupper(basename(vectorName)) )
  shpInfo <- sf::st_read( vectorName,
                          query=sprintf("SELECT * FROM %s LIMIT 1", layerName),
                          quiet=TRUE )
  if( ( vectorField %in% names(shpInfo) == FALSE ) ) {
    stop( "\ngetNodeID(): attribute ", vectorField, " does not exist in ", vectorName, "\n" )
  }

  v <- vect( vectorName )
  w <- (data.frame(v))[, vectorField] == vectorValue
  if( nrow(v[w,]) == 0 ) {
    cat( "\ngetNodeID(): value", vectorValue, "does not exist in field", vectorField, " of", vectorName, "\n" )
    IDs <- NULL
  } else {
    IDs <- cellFromXY( rast(rasterName), crds(v[w]) )
  }
  return( IDs )
}

plotIsochrone <- function( graph, node, maxTinHours, label ) {
  maxTinSeconds <- maxTinHours * 3600
  breaks <- seq( 0, maxTinSeconds, length.out=1024 )
  iso <- get_isochrone( graph, from=node, lim=breaks, setdif=T, long=T )
  names(iso)[2] <- "nodeIDs"
  iso <- merge(iso, graph$coords, by="nodeIDs" )
  iso$lim <- as.numeric(iso$lim)
  rangeX <- range( iso$x )
  rangeY <- range( iso$y )
  deltaX <- rangeX[2] - rangeX[1]
  deltaY <- rangeY[2] - rangeY[1]
  if( deltaX > deltaY  ) res <- deltaX / 1999
  if( deltaX <= deltaY ) res <- deltaY / 1999
  ext <- ext( c( floor(rangeX[1]/res)*res, ceiling(rangeX[2]/res)*res,
                 floor(rangeY[1]/res)*res, ceiling(rangeY[2]/res)*res ) )
  r <- rast( ext, res=res )
  r <- rasterize( as.matrix(iso[, c("x", "y")]), r, values=iso$lim, fun="mean" ) / 3600
  
  rbPal <- colorRampPalette( c("blue4", "blue", "aquamarine", "lightgreen", "green", "darkgreen",
                               "yellow", "orange", "purple", "red") )
  
  png( paste0("isochrone_zones_", gsub(" ", "_", label), ".png"), height=1600, width=1600, pointsize=16 )
  par( mar=c(2,2,2,0.1) )
  plot( r, col=rbPal(256), maxcell=ncell(r), colNA="gray",
        main=paste("Isochrone transportation zones for", label),
        plg=list(title="Hours") )
  points( graph$coords[graph$coords$nodeIDs == node, c("x", "y")], pch=21, col="black", bg="white", lwd=3, cex=2 )
  dev.off()
}

  
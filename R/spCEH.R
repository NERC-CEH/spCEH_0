#Define geographic projections to be used
# lat / lon 
projlonlat <- CRS("+proj=longlat +datum=WGS84")
# OSGB 1936 / British National Grid 
projOSGB <-  CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
#usethis::use_data(projlonlat, projOSGB, overwrite = TRUE)


#' Function to initialise an empty raster for 
#'   the UK or a sub-region
#'
#' @param domain Domain of the output raster: "UK", "Scotland".
#' @param res Resolution of the output raster in metres.
#' @return An empty raster object covering the UK.
#' @examples
#' r <- getRasterTemplate(domain = "Scotland", res = 100)
#' r <- getRasterTemplate(domain = "UK", res = 2000)
#' r <- getRasterTemplate(domain = "NT_10km", res = 100)
getRasterTemplate <- function(domain = "UK", res = 100){
  # OSGB 1936 / British National Grid 
  projOSGB <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
  if (domain == "UK"){xmin <- 0; xmax <- 700000; ymin <- 0; ymax <- 1300000}
  if (domain == "Scotland"){xmin <- 0; xmax <- 500000; ymin <- 500000; ymax <- 1300000}
  if (domain == "England"){xmin <- 0; xmax <- 700000; ymin <- 0; ymax <- 700000}
  if (domain == "Wales"){xmin <- 150000; xmax <- 400000; ymin <- 150000; ymax <- 400000}
  if (domain == "Northern Ireland"){xmin <- 0; xmax <- 200000; ymin <- 450000; ymax <- 650000}
  if (domain == "Edinburgh_100km"){xmin <- 300000; xmax <- 400000; ymin <- 600000; ymax <- 700000}
  if (domain == "NO_100km"){xmin <- 300000; xmax <- 400000; ymin <- 700000; ymax <- 800000}
  if (domain == "NT_100km"){xmin <- 300000; xmax <- 400000; ymin <- 600000; ymax <- 700000}
  if (domain == "NT_10km"){xmin <- 300000; xmax <- 310000; ymin <- 600000; ymax <- 610000}
  if (domain == "NT_8km"){xmin <- 300000; xmax <- 308000; ymin <- 600000; ymax <- 608000}
  ext <- extent(xmin, xmax, ymin, ymax)

  ## S4 method for signature 'Extent'
  r <- raster(ext, resolution = res, crs=projOSGB)
  return(r)
}


#' Function to mask out cells outwith polygons defining a country within the UK  
#'
#' @param r A RasterLayer.
#' @param countryName The name of a country within the UK ("Scotland", "Northern Ireland", "England" or "Wales").
#' @return A RasterLayer masked to the named country.
#' @examples
#' r_masked <- maskByCountry(r_cropArea_lcm, "Scotland")
#' r_masked <- maskByCountry(r, c("England", "Wales", "Scotland", "Northern Ireland"))
#' plot(r_ppt)
#' plot(r_masked)
maskByCountry <- function(r, countryName){
  #ogrInfo("./uk_countries", "uk_countries")
  # read in shapefile for UK borders
  uk.spoly <- readOGR("./uk_countries", "uk_countries")
  # note that readOGR will read the .prj file if it exists
  # However, we need to enforce them to be the same
  projection(uk.spoly) <- projection(r)
  masked.spoly <- subset(uk.spoly, uk.spoly@data$COUNTRY %in% countryName)
  # mask out non-countryName
  r_masked <- mask(r, masked.spoly)
  return(r_masked)
}

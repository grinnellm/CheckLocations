##### Header #####
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO)
# Group:        Quantitative Assessment Methods Section
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: Matthew.Grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      locations
# Code name:    Locations.R
# Version:      1.0
# Date started: 2019-11-28
# Date edited:  2019-11-28
# 
# Overview: 
# Investigate Locations in the new and old locations tables.
# 
# Requirements: 
# Access to the database on the shared drive, and herring shapefiles.
# 
# Notes: 
# 
# 
# References:
# 
# 

##### Housekeeping #####

# General options
rm( list=ls( ) )      # Clear the workspace
sTime <- Sys.time( )  # Start the timer
graphics.off( )       # Turn graphics off

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, locn="https://cran.rstudio.com/" ) {
  # Reverse the list 
  rPkgs <- rev( pkgs )
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(rPkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("suppressPackageStartupMessages(library(", rPkgs[i], 
                           "))", sep="")) )
  }  # End i loop over package names
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("tidyverse", "RODBC", "sp", "rgdal", "rgeos", "raster",
                    "sf") )

##### Controls ##### 

# Select region(s): major (HG, PRD, CC, SoG, WCVI); minor (A27, A2W, JS); All
region <- "All"

# Subset of sections (if desired, otherwise NA)
sectionSub <- NA

# Location of herring databases (catch, biosamples, spawn, etc)
dirDBs <- file.path( "..", "Data" )

# Location of the shapefiles
#dirShape <- file.path( "\\\\dcbcpbsna01a", "hdata$", "Kristen", 
#    "Herring_Shapefiles" )
dirShape <- file.path( dirDBs, "Polygons" )

# Databases: remote (i.e., H:\ for hdata$) or local (e.g., C:\)
dbLoc <- "Remote"

# Database name
dbName <- "HSA_Program_v6.2.mdb"

# Input coordinate reference system
inCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Coordinate reference system (http://spatialreference.org/ref/sr-org/82/)
outCRS <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 
    +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Geographic projection
geoProj <- "Projection: BC Albers (NAD 1983)"

##### Parameters #####

# Year range to include data (data starts at 1928; 1951 for stock assessment)
yrRange <- 1951:2019

##### Sources #####

# Location and name of the location database and tables
areaLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName,
  fns=list(sections="Sections", locations="Location") )

# Location(s) and names of the Sections and land shapefiles
shapesLoc <- list(
  locSec=file.path( dirShape ),
  locLand=file.path( dirShape ),
  fns=list(sections="SectionsIntegrated", land="GSHHS_h_L1_Alb") )

##### Functions #####

# Load helper functions
source( file=file.path( "..", "HerringFunctions", "Functions.R") )

##### Data #####

# Load herring areas
areas <- LoadAreaData( where=areaLoc )

# Get BC land data etc (for plots)
shapes <- LoadShapefiles( where=shapesLoc, a=areas )

##### Main ##### 

# Convert areas to a spatial object
areasSF <- areas %>%
  st_as_sf( coords=c("Longitude", "Latitude"), crs=4326 ) %>%
  st_transform( 3347 )

##### Figures #####

# Show the map
mapview( areasSF )

##### Tables #####



##### Output #####

## Save the workspace image 
#save.image( file="Image.RData"  ) 

##### End ##### 

# Print end of file message and elapsed time
cat( "End of file:" ) ;  print( Sys.time( ) - sTime )

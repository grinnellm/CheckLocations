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
# Access to the database on the shared drive, and herring shapefiles. Use
# 32-bit R to access the MS Access database (not available in RStudio).
# 
# Notes: 
# source(file="Locations.R")
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
                    "sf", "rnaturalearth", "rnaturalearthdata", "mapview",
                    "ggmap", "maptools") )

##### Controls ##### 

# Sections to investigate
<<<<<<< HEAD
iSections <- unique(areasSF$Section) 
=======
# iSections <- c("078", "239", "123")  # unique(areasSF$Section) c("078", "239") 
>>>>>>> bb6fa059dddec61bd9194a8b1ab262652fb51d0e

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

# Output location for maps
mapDir <- "Maps"

# Output location for files
csvDir <- "CSVs"

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

# Remove the output directory: maps
if( mapDir %in% list.files() ) unlink( mapDir, recursive=TRUE )

# Create a directory to store maps
dir.create( mapDir )

# Remove the output directory: text
if( csvDir %in% list.files() ) unlink( csvDir, recursive=TRUE )

# Create a directory to store text
dir.create( csvDir )

##### Data #####

# World shapefile
canada <- ne_countries( scale="large", country="canada", returnclass="sf" )

# Load herring areas
areas <- LoadAreaData( where=areaLoc ) %>%
  filter( Longitude<0, Latitude>0 )

# Get BC land data etc (for plots)
shapes <- LoadShapefiles( where=shapesLoc, a=areas )

##### Main #####

# Convert areas to a spatial object
areasSF <- areas %>%
  st_as_sf( coords=c("Longitude", "Latitude"), crs=4326 ) %>%
  select( SAR, Region, StatArea, Section, LocationCode, LocationName ) %>%
  mutate( Section=formatC(Section, width=3, flag="0"),
          StatArea=formatC(StatArea, width=2, flag="0") ) #%>%
# st_transform( 3347 )

# Convert polygons to a spatial object
sectionsSF <- shapes$secAllSPDF %>%
  st_as_sf( coords=c("Longitude", "Latitude"), crs=3347 ) %>%
  st_transform( 4326 )

##### Figures #####

# Show the maps (interactive)
mapview( areasSF, zcol="Section", layer.name="Section" )
# mapview( shapes$secAllSPDF, zcol="Section", layer.name="Section" )

# Show points and polygons on a map
MakeMap <- function( pts, polys, sec ) {
  # Add a column for inside/outside pts
  pts <- pts %>%
    mutate( Inside="Ok" )
  # Subset points
  ptsSub <- pts %>%
    filter( Section==sec ) %>%
    mutate( Section=as.character(Section) )
  # Subset polygons
  polysSub <- polys %>%
    filter( Section==sec )
  # If there is a polygon
  if( nrow(polysSub) >= 1 ) {
    # Spatial overlay -- which points are in the polygon
    ptsOver <- over( x=as(pts, "Spatial"), y=as(polysSub, "Spatial") ) %>%
      as_tibble( ) %>%
      rename( SectionPolys=Section ) %>%
      mutate( LocationCode=pts$LocationCode,
              LocationName=pts$LocationName,
              SectionPts=pts$Section )
    # Indentify points outside the boundary
    ptsOutside <- ptsOver %>%
      filter( is.na(SectionPolys) & SectionPts==sec )
    # Identify points inside the boundary that aren't classified as such
    ptsInside <- ptsOver %>%
      filter( !is.na(SectionPolys), SectionPolys!=SectionPts )
    # Add these points to the subset of points
    ptsSub <- ptsSub %>%
      rbind( y=filter( pts, LocationCode%in%ptsInside$LocationCode) ) %>%
      mutate( Inside=ifelse(LocationCode%in%ptsInside$LocationCode, "Yes",
                            Inside),
              Inside=ifelse(LocationCode%in%ptsOutside$LocationCode, "No",
                            Inside) )
    # Select bad pts
    badPts <- ptsSub %>%
      filter( Inside!="Ok" )
    # Write bad points to disc if they exist
    if( nrow(badPts) >= 1 )
      write_csv( x=badPts, path=file.path(csvDir, paste(sec, "csv", sep=".")) )
  } else { # End if polygons, otherwise
    # Make a dummy for the plot
    badPts <- ptsSub %>%
      filter( Inside!="Ok" )
  }  # End if no polygons
  # Determine extent: points
  extPts <- extent( ptsSub )
  # Determine extent: polys
  extPolys <- extent( polysSub )
  # Determine the overall extentext
  ext <- c( left=min(extPts@xmin, extPolys@xmin, na.rm=TRUE),
            bottom=min(extPts@ymin, extPolys@ymin, na.rm=TRUE),
            right=max(extPts@xmax, extPolys@xmax, na.rm=TRUE),
            top=max(extPts@ymax, extPolys@ymax, na.rm=TRUE) )
  # Expand x to give a buffer
  xBuf <- 0.05 * ( ext["left"] - ext["right"] )
  # Expand y to give a buffer
  yBuf <- 0.05 * ( ext["top"] - ext["bottom"] )
  # Expand the extent
  extBuf <- c( ext["left"] + xBuf,
               ext["bottom"] - yBuf,
               ext["right"] - xBuf,
               ext["top"] + yBuf )
  # Grab the map data
  map <- get_map( extBuf )
  # Plot the map
  gmap <- ggmap( map ) +
    geom_sf( data=polysSub, colour="blue", fill="transparent",
             inherit.aes=FALSE ) +
    geom_sf( data=ptsSub, mapping=aes(fill=Section, shape=Inside),
             colour="transparent", shape=21, inherit.aes=FALSE, size=3,
             alpha=0.75 ) +
    scale_fill_viridis_d( ) +
    geom_sf_text( data=badPts, mapping=aes(label=LocationCode),
                  inherit.aes=FALSE) +
    labs( title=paste("Section", sec), x="Longitude", y="Latitude" ) +
    ggsave( filename=file.path(mapDir, paste(sec, "png", sep=".")), height=9,
            width=9 )
  # # Plot the map
  # gmap2 <- ggplot( data=canada ) +
  #   geom_sf( colour="transparent", fill="antiquewhite") +
  #   geom_sf( data=polysSub, colour="blue", fill="transparent" ) +
  #   geom_sf( data=ptsSub, mapping=aes(fill=Section) ) +
  #   coord_sf( xlim=c(extBuf["left"], extBuf["right"]),
  #             ylim=c(extBuf["bottom"], extBuf["top"]) ) +
  #   labs( title=paste("Section", sec), x="Longitude", y="Latitude" )
  # print( gmap2 )
}  # End MakeMap function

# Loop over sections
for( iSec in unique(areasSF$Section) )
  MakeMap( pts=areasSF, polys=sectionsSF, sec=iSec )

##### Tables #####



##### Output #####

## Save the workspace image 
#save.image( file="Image.RData"  ) 

##### End ##### 

# Print end of file message and elapsed time
cat( "End of file: " ) ;  print( Sys.time( ) - sTime )


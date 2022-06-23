##### Header #####
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO)
# Group:        Quantitative Assessment Methods Section
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: Matthew.Grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      locations
# Code name:    Locations.R
# Version:      2.0
# Date started: 2019-11-28
# Date edited:  2022-06-20
#
# Overview:
# Investigate Locations in the new and old locations tables.
#
# Requirements:
# Internet to get spawn index from Open Data.
#
# References:
#

##### Housekeeping #####

# General options
rm(list = ls()) # Clear the workspace
s_time <- Sys.time() # Start the timer
graphics.off() # Turn graphics off

# Install missing packages and load required packages (if required)
use_packages <- function(pkgs, locn = "https://cran.rstudio.com/") {
  # Reverse the list
  r_pkgs <- rev(pkgs)
  # Identify missing (i.e., not yet installed) packages
  new_pkgs <- r_pkgs[!(r_pkgs %in% installed.packages()[, "Package"])]
  # Install missing packages if required
  if (length(new_pkgs)) install.packages(new_pkgs, repos = locn)
  # Loop over all packages
  for (i in seq(r_pkgs)) {
    # Load required packages using 'library'
    eval(
      parse(text = paste("suppressPackageStartupMessages(library(", r_pkgs[i],
        "))",
        sep = ""
      ))
    )
  } # End i loop over package names
} # End use_packages function

# Make packages available
use_packages(
  pkgs = c(
    "tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "SpawnIndex", "here",
    "sentimentr", "lexicon", "ggmap", "ggsflabel"
  )
)

# install.packages("devtools")
# devtools::install_github(repo = "grinnellm/SpawnIndex")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

##### Parameters #####

# Coordinate reference system
epsg_crs <- 4326

##### Output #####

# Cache location (for saved Open Data file)
cache_dir <- "Cache"

# Create folder save Open Data
if (!cache_dir %in% list.files(path = here())) {
  dir.create(path = here(cache_dir))
}

# Output location for files
csv_dir <- "CSVs"

# Create folder for text files
if (!csv_dir %in% list.files(path = here())) {
  dir.create(path = here(csv_dir))
}

# Output location for maps
map_dir <- "Maps"

# Create folder for maps
if (!map_dir %in% list.files(path = here())) {
  dir.create(path = here(map_dir))
}

##### Data #####

# Get BC coast
bc_coast <- ne_countries(
  scale = "large", returnclass = "sf", continent = "North America"
) %>%
  st_transform(crs = epsg_crs)

# Get spawn data
get_spawn <- function(fn,
                      quiet = FALSE) {
  # Check if the data exists locally
  is_data <- fn %in% list.files(here(cache_dir))
  # If the data is present
  if (is_data) {
    # Load the data
    dat <- read_csv(file = here(cache_dir, fn), col_types = cols())
  } else { # End if data, otherwise download it (takes a while)
    # Grab the data and save it
    dat <- read_csv(
      file = paste(
        "https://pacgis01.dfo-mpo.gc.ca", "FGPPublic",
        "Pacific_Herring_Spawn_Index_Data",
        "Pacific_herring_spawn_index_data_EN.csv",
        sep = "/"
      ),
      col_types = cols()
    ) %>%
      write_csv(file = here(cache_dir, fn))
  } # End if no data
  # Wrangle the data
  u_dat <- dat %>%
    mutate(
      StatisticalArea = formatC(StatisticalArea, width = 2, flag = "0"),
      Section = formatC(Section, width = 3, flag = "0")
    ) %>%
    select(
      Region, StatisticalArea, Section, LocationCode, LocationName,
      Longitude, Latitude
    ) %>%
    distinct()
  # Check for missing lat/long data
  no_lat_long <- u_dat %>%
    filter(is.na(Longitude) | is.na(Latitude)) %>%
    select(Region, StatisticalArea, Section, LocationCode, LocationName)
  # If not quiet
  if (!quiet) {
    # If missing spatial data
    if (nrow(no_lat_long) >= 1) {
      # Message
      cat(
        "There are ", nrow(no_lat_long),
        " location(s) with missing spatial information: ",
        "see file 'NoLatLong.csv'."
      )
      # Write missing data to csv
      no_lat_long %>%
        write_csv(file = here("NoLatLong.csv"))
    } else { # End if missing, otherwise
      # Message
      cat("No missing spatial data detected.\n")
    } # End if no missing spatial data
  } # End if not quiet
  # Make a spatial object
  dat_sf <- u_dat %>%
    filter(!is.na(Longitude), !is.na(Latitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = epsg_crs)
  # Return the spawn data
  dat_sf
} # End get_spawn function

# Get spawn data
spawn_all <- get_spawn(fn = "spawn_all.csv")

# Get area data
areas <- spawn_all %>%
  select(Region, StatisticalArea, Section) %>%
  as_tibble() %>%
  select(Region, StatisticalArea, Section) %>%
  distinct()

# Load sections polygons
data(sections)

# Add spatial info to sections
sections <- sections %>%
  st_transform(crs = epsg_crs) %>%
  left_join(y = areas, by = "Section") %>%
  distinct() %>%
  select(Region, StatisticalArea, Section) %>%
  arrange(Region, StatisticalArea, Section)

##### Main #####

# Check profanity
check_profane <- function(dat,
                          quiet = FALSE) {
  # Get distinct location names
  dat <- dat %>%
    select(Region, StatisticalArea, Section, LocationCode, LocationName) %>%
    distinct()
  # Get list of profane words
  all_profane <- c(
    profanity_alvarez, profanity_arr_bad, profanity_banned, profanity_zac_anger,
    profanity_racist
  ) %>%
    tolower() %>%
    unique()
  # Identify potential bad names (no warnings)
  suppressWarnings(
    bad_locs <- dat %>%
      pull(LocationName) %>%
      tolower() %>%
      get_sentences() %>%
      profanity(profanity_list = all_profane) %>%
      as_tibble() %>%
      filter(profanity_count > 0)
  )
  # Vector of names that get flagged, but are actually OK
  names_ok <- c(
    "Hook Pt", "Joachim Spit", "Swallow Is", "Shingle Spit", "Rebecca Spit",
    "Fanny Bay", "Willy Is", "Goose Spit", "Walker Hook", "Steamer Cv",
    "Antons Spit", "Beaver Hrbr (Ft Rupert)", "Gay Pass", "Bull Hrbr",
    "Bull Cv", "Beaver Cv", "Finis Nook"
  )
  # Get bad names
  bad_dat <- dat[bad_locs$element_id, ] %>%
    as_tibble() %>%
    filter(!LocationName %in% names_ok) %>%
    select(Region, StatisticalArea, Section, LocationCode, LocationName)
  # If profanity
  if (nrow(bad_dat) >= 1) {
    # Write bad names to csv
    bad_dat %>%
      write_csv(file = here("Profane.csv"))
    # If messages
    if (!quiet) {
      # Message
      cat("Location(s) with possible profane names: see file 'Profane.csv'.\n")
    } # End if not quiet
  } else { # End if profanity, otherwise
    # If not quiet and no profanity
    if (!quiet) {
      # Message
      cat("No profanity detected.\n")
    } # End if not quiet
  } # End if no profanity
  # Return bad names
  bad_dat$LocationName
} # End check_profane function

# Check for potentially profane manes
locs_bad_names <- check_profane(dat = spawn_all)

# Check spatial overlay
check_overlay <- function(pts,
                          poly,
                          buff = 2000,
                          quiet = FALSE) {
  # Check overlay
  inside <- st_intersects(x = pts, y = poly, sparse = FALSE)
  # Add to points
  pts <- pts %>%
    mutate(Inside = inside[, 1])
  # Section extent
  ext_poly <- poly %>%
    st_buffer(dist = buff) %>%
    st_bbox()
  # Points extent
  ext_pts <- pts %>%
    st_buffer(dist = buff) %>%
    st_bbox()
  # Overall extent
  ext_all <- c(
    min(ext_poly$xmin, ext_pts$xmin),
    min(ext_poly$ymin, ext_pts$ymin),
    max(ext_poly$xmax, ext_pts$xmax),
    max(ext_poly$ymax, ext_pts$ymax)
  )
  # Get map tiles (no messages)
  suppressMessages(
    my_map <- get_stamenmap(
      bbox = ext_all, zoom = 10, maptype = "terrain", messaging = FALSE
    )
  )
  # If points outside
  if (any(!pts$Inside)) {
    # No messages
    suppressMessages(
      # Plot the map
      map <- ggmap(my_map) +
        geom_sf(
          data = poly, fill = "transparent", colour = "black", size = 0.5,
          inherit.aes = FALSE
        ) +
        geom_sf(
          data = pts, size = 3, mapping = aes(colour = Inside),
          inherit.aes = FALSE
        ) +
        geom_sf_label_repel(
          data = pts %>% filter(!Inside), mapping = aes(label = LocationCode),
          alpha = 0.75, inherit.aes = FALSE
        ) +
        scale_colour_viridis_d() +
        labs(
          title = paste("Section", unique(poly$Section)),
          x = "Longitude",
          y = "Latitude"
        ) +
        guides(colour = "none") +
        theme(
          panel.border = element_rect(colour = "black", fill = "transparent")
        )
    ) # End of no messages
    # Save the plot (no warnings)
    suppressWarnings(
      ggsave(
        plot = map, width = 7, height = 7,
        filename = here(
          map_dir,
          paste("Section", unique(poly$Section), ".png", sep = "")
        ),
      )
    )
    # Save the points
    pts <- pts %>%
      filter(!Inside) %>%
      select(Region, StatisticalArea, Section, LocationCode, LocationName) %>%
      write_csv(file = here(
        csv_dir,
        paste("Section", unique(poly$Section), ".csv", sep = "")
      ))
    # If messages
    if (!quiet) {
      # Message
      cat("Point(s) outside Section", unique(poly$Section), "polygon.\n")
    } # End if messages
  } else { # End if points outside, otherwise
    # If messages and all points OK
    if (!quiet) {
      cat("All points inside Section", unique(poly$Section), "polygon.\n")
    } # End if messages and all points OK
  } # End if all points inside
} # End check_overlay function

# Determine unique sections
u_sections <- unique(spawn_all$Section)

# Message
cat("Spatial overlay for", length(u_sections), "sections:\n")

# Loop over sections
for (i in seq(u_sections)) {
  # Get ith section
  i_section <- u_sections[i]
  # Get spawn data for section i
  i_spawn <- spawn_all %>%
    filter(Section == i_section)
  # Get section i polygon
  i_polygon <- sections %>%
    filter(Section == i_section)
  # Spatial overlay
  check_overlay(pts = i_spawn, poly = i_polygon)
} # End i loop over sections

##### End #####

# Print end of file message and elapsed time
cat("End of file: ")
print(Sys.time() - s_time)

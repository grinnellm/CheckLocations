# CheckLocations

Check whether Pacific Herring spawn Locations are
inside the boundary of the correct Section polygon.

## Use

Source the file "Locations.R" to check whether spawn Locations are
inside the boundary of the correct Section polygon.
The analysis requires an internet connection
to download spawn index data from Open Data.
If there are point(s) outside the Section polygon boundary:

1. A map showing the points is saved in "./Maps/", and

1. A text file is created and saved in "./CSVs/".

In addition, this scrip check for:

1. Profanity in Location names and
saves potentially profane Location names in "Profane.csv", and

1. Missing spatial data (i.e., no latitude or longitude information)
and saves the points in "NoLatLong.csv".

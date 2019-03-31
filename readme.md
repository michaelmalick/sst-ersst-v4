This project is deprecated, please use <https://github.com/michaelmalick/r-ersst>


# ERSST.v4 SST Data Processing

This project downloads, processes, and visualizes the NOAA ERSST.v4 SST data
set. There are also functions for exporting 'tidy' CSV files for use in other
projects.


## Data notes

The full metadata for the SST data are located in the headers of the downloaded
SST netCDF files. The metadata can be viewed using the `ncdump` utility with the
`-h` flag.

- Data website: <https://www.ncdc.noaa.gov/data-access/marineocean-data/extended-reconstructed-sea-surface-temperature-ersst-v4>
- Data files:   <https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v4/netcdf/>
- Longitude: degrees east, specifies center of grid cell
- Latitude: degrees north, specifies center of grid cell


## Code notes

- Files:
    - load.R:   load libraries and functions for data processing
    - fun.R:    R functions for the project
    - main.R:   control/run the analysis
    - export.R: script for exporting 'tidy' SST data for use in other
                projects

The functions in this analysis are designed to work with the gridded SST data
using three dimensional R arrays, which are considerably more efficient than
using data frames, but require some extra care to make sure the proper info is
kept with the data. In order to keep both the array data and other info, e.g.,
vectors of years, months, latitude, and longitude, together, most functions in
this analysis expect as input (and output data as) a list with the following
elements:

    $lon    vector of longitudes, corresponds to the rows in the $sst array
    $lat    vector of latitudes, corresponds to the columns in the $sst array
    $year   vector of years, corresponds to the 3rd dimension of the $sst array
                i.e. there is one year for each matrix in the $sst array
    $month  vector of months, corresponds to the 3rd dimension of the $sst array
                i.e. there is one month for each matrix in the $sst array
    $sst    array of gridded sst data with dimension lon x lat x time

The `sst_dataframe()` and `sst_write_csv()` functions can be used to convert the
array data into a 'tidy' data frame and write the data to a CSV file, which can
be easier to work with in other projects.

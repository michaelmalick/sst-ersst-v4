## SST analysis functions
## Michael Malick


## sst_write_csv() -----------------------------------------
sst_write_csv <- function(data, file,
                          date,
                          contact,
                          data.type,
                          years,
                          months,
                          spatial.extent) {

    ## Write SST data to a CSV file
    ##
    ## This function takes output from sst_dataframe() and writes the dataframe
    ## to a CSV file.
    ##
    ## **WARNING** the CSV files can get extremely large and it is recommended
    ## that only a spatial subset of the data is written to CSV files in this
    ## format, rather than the entire global dataset.
    ##
    ## data = data.frame of sst data to write
    ## file = path + filename to write data to
    ## date = date data were written
    ## contact = name of person writing data
    ## data.type = type of data written, e.g., raw SST or anomalies
    ## years = extent of years
    ## months = extent of months
    ## spatial.extent = extent of lat and lon

    if(class(data) != "data.frame")
        stop("data is not of class data.frame")

    datafile <- file(file, open = 'wt')
    header <- c("## ERSST.v4 SST data",
                "##",
                paste("## Written: ", date, sep = ""),
                paste("## Contact: ", contact, sep = ""),
                paste("## Data type: ", data.type, sep = ""),
                paste("## Spatial extent: ", spatial.extent, sep = ""),
                "##",
                paste("## year: ", years, sep = ""),
                paste("## month: ", months, sep = ""),
                "## lon: longitude degrees east",
                "## lat: latitude degrees north",
                "## id: unique cell id",
                "## sst: degrees C")
    writeLines(header, con = datafile)
    close(datafile)
    write.table(data,
                file      = file,
                sep       = ",",
                row.names = FALSE,
                col.names = TRUE,
                append    = TRUE)
}

## Testing
if(FALSE) {

    test <- sst_load(1950:1951, 1:12, "./data/rawdata/")
    anom <- sst_anomaly(test, ref.years = 1950:2016, "./data/rawdata/")
    anom.df <- sst_dataframe(anom)

    sst_write_csv(data = anom.df,
                  file = "~/Desktop/test.csv",
                  date = "2017-02-19",
                  creator = "Michael Malick <malickmj@gmail.com>",
                  data.type = "SST anomalies, reference period = 1950-2016",
                  years = "1950-2016",
                  months = "1-12",
                  spatial.extent = "20-80N and 130-260E")

}


## sst_dataframe() -----------------------------------------
sst_dataframe <- function(data) {

    ## Convert SST data array to a data.frame
    ##
    ## Output data.frame columns:
    ##  $year   numeric year
    ##  $month  numeric month
    ##  $lon    longitude (degrees east)
    ##  $lat    latitude (degrees north)
    ##  $id     unique cell id
    ##  $sst    sst data (degrees C)
    ##
    ## data = list of sst data output from sst_load() or sst_anomaly()

    lat      <- data$lat
    lon      <- data$lon
    n        <- length(data$year)
    sst.list <- vector("list", n)

    for(i in 1:n) {

        i.year  <- data$year[i]
        i.month <- data$month[i]
        i.sst   <- data$sst[ , , i]

        sst.df        <- as.data.frame(i.sst)
        names(sst.df) <- lat
        sst.df$lon    <- lon

        sst.long <- reshape(sst.df,
                            direction = "long",
                            varying   = names(sst.df)[names(sst.df) != "lon"],
                            v.names   = "sst",
                            times     = lat,
                            timevar   = "lat")
        row.names(sst.long) <- NULL
        sst.long$year  <- i.year
        sst.long$month <- i.month

        sst.list[[i]] <- sst.long
    }

    out    <- do.call("rbind", sst.list)
    out.df <- data.frame(year  = out$year,
                         month = out$month,
                         lon   = out$lon,
                         lat   = out$lat,
                         id    = out$id,
                         sst   = out$sst)
    return(out.df)
}

## Testing
if(FALSE) {

    test <- sst_load(1950:1951, 1:12, "./data/rawdata/")
    anom <- sst_anomaly(test, ref.years = 1950:2016, "./data/rawdata/")
    dim(anom$sst)

    anom.df <- sst_dataframe(anom)
    head(anom.df)
    tail(anom.df)

    levelplot(sst ~ lon * lat, data = anom.df,
              subset = month == 1 & year == 1950,
              col.regions = chroma::dpal(500, hue = c(240, 0),
                                         chroma = 70,
                                         power = 1.0),
              at = seq(-5, 5, 0.5),
              contour = TRUE,
              ylab = "Latitude", xlab = "Longitude")


    test <- sst_load(1950:2016, 1:12, "./data/rawdata/")
    anom <- sst_anomaly(test, ref.years = 1950:2016, "./data/rawdata/")
    anom.nep <- sst_subset_space(anom,
                                 lat.min = 20,
                                 lat.max = 80,
                                 lon.min = 130,
                                 lon.max = 260)
    anom.df <- sst_dataframe(anom.nep)
    head(anom.df)
    tail(anom.df)
    object.size(anom.df) / object.size(anom.nep)

}


## sst_subset_time() ---------------------------------------
sst_subset_time <- function(data, years, months) {

    ## Temporally subset SST data
    ##
    ## This function takes SST data read in using the sst_load() function and
    ## temporally subsets the array.
    ##
    ## data = array of SST data (lon x lat x time)
    ## years = vector of years to subset
    ## months = vector of months to subset

    ind  <- which(data$month %in% months & data$year %in% years)

    out <- list(lon   = data$lon,
                lat   = data$lat,
                year  = data$year[ind],
                month = data$month[ind],
                sst   = data$sst[ , , ind])
    return(out)
}

## Testing
if(FALSE) {

    test <- sst_load(1950:2016, 1:12, "./data/rawdata/")

    test.sub <- sst_subset_time(test, 1950:1951, 1:2)
    dim(test.sub$sst)
    test.sub$month
    test.sub$year

}


## sst_subset_space() --------------------------------------
sst_subset_space <- function(data,
                             lat.min,
                             lat.max,
                             lon.min,
                             lon.max) {

    ## Spatially subset SST data
    ##
    ## This function takes SST data read in using the sst_load() function and
    ## spatially subsets the array.
    ##
    ## data = array of SST data (lon x lat x time)
    ## lat.min = minimum latitude for data subset (inclusive)
    ## lat.max = maximum latitude for data subset (inclusive)
    ## lon.min = minimum longitude for data subset (inclusive)
    ## lon.max = maximum longitude for data subset (inclusive)

    lat.sub.ind <- which(data$lat >= lat.min & data$lat <= lat.max)
    lon.sub.ind <- which(data$lon >= lon.min & data$lon <= lon.max)

    out <- list(lon   = data$lon[lon.sub.ind],
                lat   = data$lat[lat.sub.ind],
                year  = data$year,
                month = data$month,
                sst   = data$sst[lon.sub.ind, lat.sub.ind, ])
    return(out)
}

## Testing
if(FALSE) {

    test <- sst_load(1950:2016, 1:12, "./data/rawdata/")

    test.sub <- sst_subset_space(test,
                                 lat.min = 20,
                                 lat.max = 65,
                                 lon.min = 150,
                                 lon.max = 250)
    dim(test.sub$sst)
    length(test.sub$lat)
    length(test.sub$lon)

    image(x = test.sub$lon,
          y = test.sub$lat,
          z = test.sub$sst[ , , 1])

}


## sst_map() -----------------------------------------------
sst_map <- function(data, plot.dir, ...) {

    ## Map SST values using heatmaps
    ##
    ## This functions creates monthly heat maps of SST. One file (map) is
    ## created for each month and year of SST data.
    ##
    ## data = sst data output from sst_load or sst_anomaly
    ## plot.path = directory to save plots
    ## ... = passed to levelplot()

    n <- length(data$year)

    for(i in 1:n) {

        year  <- data$year[i]
        month <- data$month[i]

        plot.name <- paste("sst_", year, "_", month, ".jpeg", sep = "")
        jpeg(paste(plot.dir, plot.name, sep = ""), 800, 500)

        g <- levelplot(data$sst[ , , i],
                       col = "grey60",
                       cuts = 200,
                       contour = TRUE,
                       main = paste("year = ", year, "   month = ", month,
                                    sep = ""),
                       row.values = data$lon,
                       column.values = data$lat,
                       ylab = "Latitude", xlab = "Longitude", ...,
                       panel = function(...) {
                           panel.fill(col = "grey40")
                           panel.levelplot(...)
                           mp <- map('world2', fill = TRUE, plot = FALSE)
                           lpolygon(mp$x, mp$y,
                                    col = "grey25",
                                    border = "grey25")
                       })
        print(g)
        dev.off()
    }
}


## sst_anomaly() -------------------------------------------
sst_anomaly <- function(data, ref.years, read.dir) {

    ## Compute monthly grid cell specific SST anomalies
    ##
    ## This function calculates monthly grid cell specific anomalies of SST.
    ## Anomalies are calculated as the difference between a grid cell specific
    ## SST value for a given year/month and the long-term monthly mean (defined
    ## by ref.years) for that grid cell. This follows the methods outlined in
    ## Mueter et al. 2002, Canadian Journal of Fisheries and Aquatic Sciences.
    ##
    ## data = list of SST data output from sst_load()
    ## ref.years = years over which to calculate the long-term average
    ## read.dir = directory where the raw sst is stored in netcdf format


    ## 1. load data for reference years
    sst.ref <- sst_load(years  = ref.years,
                        months = 1:12,
                        read.dir = read.dir)

    ## 2. create empty array to hold monthly long-term averages
    ar.ref <- array(NA, dim = c(length(sst.ref$lon), length(sst.ref$lat), 12))

    ## 3. calculate monthly long-term means for each grid cell
    for(i in 1:12)
        ar.ref[ , , i] <- apply(sst.ref$sst[ , , which(sst.ref$month == i)],
                                c(1, 2), mean, na.rm = TRUE)

    ## 4. create empty array to hold monthly anomalies
    ar.out <- array(NA, dim = c(length(data$lon),
                                length(data$lat),
                                length(data$month)))

    ## 5. calculate monthly anomalies for each grid cell
    for(i in seq_along(data$month)) {
        i.month <- data$month[i]
        ar.out[ , , i] <- data$sst[ , , i] - ar.ref[ , , i.month]
    }

    ## 6. create list with anomalies, lat, lon, and time indices
    out <- list(lon   = data$lon,
                lat   = data$lat,
                year  = data$year,
                month = data$month,
                sst   = ar.out)
    return(out)
}

## Testing
if(FALSE) {

    test <- sst_load(1950:2016, 1:12, "./data/rawdata/")
    anom <- sst_anomaly(test, ref.years = 1950:2016, "./data/rawdata/")

    anom$lon
    anom$lat
    anom$year
    anom$month
    class(anom$sst)
    dim(anom$sst)
    class(anom$sst[ , , 1])
    head(anom$sst[ , , 1])
    tail(anom$sst[ , , 1])

    image(x = anom$lon,
          y = anom$lat,
          z = anom$sst[ , , 13])

    levelplot(anom$sst[ , , 1], aspect = "fill",
              row.values = anom$lon, column.values = anom$lat,
              ylab = "Latitude", xlab = "Longitude")

}


## sst_load() ----------------------------------------------
sst_load <- function(years, months, read.dir) {

    ## Load ERSST.v4 SST data into R
    ##
    ## This function loads ERSST.v4 SST data in netCDF format into R. The
    ## function outputs a list with five elements:
    ##  1. $lon    vector of longitudes
    ##  2. $lat    vector of latitudes
    ##  3. $year   vector of years
    ##  4. $month  vector of months
    ##  5. $sst    array of SST data
    ## The SST array has dimensions:
    ##      lon (rows) x lat (columns) x month/year (slices)
    ##
    ## Each matrix/slice in the array is gridded SST data for a given month and
    ## year, which can be indexed using the $year and $month vectors in the
    ## list.
    ##
    ## years = numeric vector of years of sst to read
    ## months = numeric vector of months to read
    ## read.dir = directory where the raw sst is stored in netcdf format

    n         <- length(years) * length(months)
    vec.year  <- sort(rep(years, length(months)))
    vec.month <- rep(months, length(years))
    sst.list  <- vector("list", n)

    for(i in 1:n) {

        i.year  <- vec.year[i]
        i.month <- vec.month[i]

        if(i.month < 10) i.month <- paste("0", i.month, sep = "")

        sst.ncdf.name  <- paste(read.dir, "ersst.v4.", i.year,
                                i.month, ".nc", sep = "")
        sst.name  <- "sst"
        lat.name  <- "lat"
        lon.name  <- "lon"

        ## read in netcdf SST data
        sst.ncdf <- ncdf4::nc_open(sst.ncdf.name)
        dat.lat  <- ncdf4::ncvar_get(sst.ncdf, varid = lat.name)
        dat.lon  <- ncdf4::ncvar_get(sst.ncdf, varid = lon.name)
        dat.sst  <- ncdf4::ncvar_get(sst.ncdf, varid = sst.name)
        ncdf4::nc_close(sst.ncdf)

        ## replace missing values
        dat.sst[dat.sst < -9.98] <- NA

        ## save sst matrix to list
        sst.list[[i]] <- dat.sst

    }

    ## array: lon x lat x month
    sst <- array(do.call("c", sst.list),
                 dim = c(length(dat.lon),
                         length(dat.lat),
                         n))

    out <- list(lon   = dat.lon,
                lat   = dat.lat,
                year  = vec.year,
                month = vec.month,
                sst   = sst)
    return(out)
}

## Testing
if(FALSE) {

    test <- sst_load(1950:2016, 1:12, "./data/rawdata/")
    names(test)
    test$lon
    test$lat
    test$year
    test$month
    class(test$sst)
    dim(test$sst)
    class(test$sst[ , , 1])
    head(test$sst[ , , 1])
    tail(test$sst[ , , 1])

    image(x = test$lon,
          y = test$lat,
          z = test$sst[ , , 13])

    levelplot(test$sst[ , , 1], aspect = "fill",
              row.values = test$lon, column.values = test$lat,
              ylab = "Latitude", xlab = "Longitude")

}


## sst_download() ------------------------------------------
sst_download <- function(years, months, save.dir) {

    ## Download monthly ERSST.v4 SST data in netCDF format
    ##
    ## This function downloads ERSST.v4 SST data and saves it to disk in netCDF
    ## format. Running the function requires the `wget` utility be available in
    ## the path (on MacOS `wget` can be installed using homebrew).
    ##
    ## Filename convention is:
    ##   ersst.VERSION.yyyymm.nc
    ##   yyyy=four digit year
    ##   mm=two digit month
    ##
    ## years = numeric vector of years to download data for, e.g., 1950:2016
    ## months = numeric vector of months to download data, e.g., 1:12
    ## save.dir = directory to save sst data files in, e.g., "./data/"

    cnt <- 0
    for(i in years) {

        for(j in months) {

            if(j < 10) j <- paste("0", j, sep = "")

            web <- paste("https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/",
                         "v4/netcdf/ersst.v4.", i, j, ".nc", sep = "")

            fname <- paste(save.dir, "ersst.v4.", i, j, ".nc", sep = "")

            if(file.exists(fname) == FALSE) {
                download.file(web, destfile = fname,
                              method = "wget", mode = "wb")
                cnt <- cnt + 1
            }
        }
    }

    ## check if all data files were downloaded
    n.possible <- length(years) * length(months)
    n.exist    <- length(list.files(save.dir))
    cat(cnt, "files downloaded of", n.possible, "requested", "\n")
    cat(n.exist, "files exist in", save.dir, "\n")
}


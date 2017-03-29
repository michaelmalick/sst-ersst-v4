## SST analysis
## Michael Malick
## v1.0


## 1. Download ERSST.v4 data -------------------------------
sst_download(years    = 1950:2016,
             months   = 1:12,
             save.dir = "./data/rawdata/")


## 2. Load SST data into R ---------------------------------
sst.raw <- sst_load(years = 1950:2016,
                    months = 1:12,
                    read.dir = "./data/rawdata/")
min(sst.raw$sst, na.rm = TRUE)
max(sst.raw$sst, na.rm = TRUE)


## 3. Check missing data -----------------------------------
cnt <- apply(sst.raw$sst, c(1, 2), function(x) sum(!is.na(x)))
dim(sst.raw$sst)[3] # max number of values per grid cell
min(cnt[cnt > 0])
max(cnt)


## 4. Map global raw SST -----------------------------------
cols <- chroma::spal(200, hue.start = 220,  hue.end = 70, power = 1.1)
at   <- seq(-2, 35, 2)
dir  <- "./plots/global-monthly-raw/"
sst_map(sst.raw,
        plot.dir = dir,
        col.regions = cols,
        at = at)


## 5. Compute monthly anomalies ----------------------------
sst.anom <- sst_anomaly(sst.raw,
                        ref.years = 1950:2016,
                        read.dir = "./data/rawdata/")
min(sst.anom$sst, na.rm = TRUE)
max(sst.anom$sst, na.rm = TRUE)


## 6. Map global SST anomalies -----------------------------
cols <- chroma::dpal(500, hue = c(240, 0), chroma = 70, power = 1.0)
at   <- seq(-5, 5, 0.5)
dir  <- "./plots/global-monthly-anomaly/"
sst_map(sst.anom,
        plot.dir = dir,
        col.regions = cols,
        at = at)


## 7. NE Pacific region ------------------------------------
sst.anom.nep <- sst_subset_space(sst.anom,
                                 lat.min = 20,
                                 lat.max = 80,
                                 lon.min = 130,
                                 lon.max = 260)
min(sst.anom.nep$sst, na.rm = TRUE)
max(sst.anom.nep$sst, na.rm = TRUE)

## map anomalies
cols <- chroma::dpal(500, hue = c(240, 0), chroma = 70, power = 1.0)
at   <- seq(-4.5, 4.5, 0.5)
dir  <- "./plots/nep-anomaly/"
sst_map(sst.anom.nep,
        plot.dir = dir,
        col.regions = cols,
        at = at,
        labels = TRUE)


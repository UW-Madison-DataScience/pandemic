R/pandemic

Package to show growth of pandemic relative to hospital beds.
Use by the following sequence (once package is installed):

```
app.R
```

This app is now live at <https://brianyandell.shinyapps.io/pandemic/>.
It was deployed using 

```
rsconnect::deployApp(appFiles = "app.R")
```

During testing, it is helpful to have a local copy of the timeseries data.

```
dirpath <- "https://coronadatascraper.com/timeseries.csv"
tmp <- read.csv(dirpath)
dirpath <- "data/timeseries.csv"
write.csv(tmp, dirpath)
```

There is also a JSON version, but I don't know how to use it.


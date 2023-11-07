# weather data
f <- system.file("extdata/Netherlands_Swifterbant.csv", package="meteor")
w <- read.csv(f)
w$date <- as.Date(w$date)
head(w)

# crop and soil parameters
crop <- wofost_crop("barley")
soil <- wofost_soil("ec1")

# "control" parameters
contr <- wofost_control()
contr$modelstart <- as.Date("1980-02-06")
contr$latitude=52.57
contr$elevation=50


# run model
d <- wofost(crop, w, soil, contr)

# output
head(d)
tail(d)
plot(d[,"step"], d[, "LAI"])


## Another example
crop <- wofost_crop("rapeseed_1001")
soil <- wofost_soil("soil_5")
contr$modelstart <- as.Date("1977-01-01")

rp <- wofost(crop, w, soil, contr)
plot(rp[,"step"], rp[, "LAI"])

# yield
plot(rp[, 1], rp[,"WSO"])


## water limited
contr$water_limited <- TRUE
contr$modelstart <- as.Date("1985-01-01")

crop <- wofost_crop("maize_1")
f <- system.file("extdata/Philippines_IRRI.csv", package="meteor")
wth <- read.csv(f)
wth$date <- as.Date(wth$date)
contr$elevation <- 21
contr$latitude <- 14.18

ma <- wofost(crop, wth, soil, contr)
plot(ma[,"step"], ma[, "LAI"])
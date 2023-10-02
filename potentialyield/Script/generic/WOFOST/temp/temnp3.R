#Planting 8th Aug and harvest Dec 8th
#Planting 15th Aug harvest 15 Dec
#Planting 22 Aug and harvest 22 Dec
#Planting 30 Aug harvest 30 Dec

as.Date(ISOdate(xy.df$year,"08", "08"))

data.frame(pmonth=08, pday=c(8,15,22,30), hmonth=12, hday=c(8,15,22,30))

rm("totprecfour")
rm("dmyieldfour")

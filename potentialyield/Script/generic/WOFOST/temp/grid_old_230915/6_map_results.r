#visualize
mdata<-xy.df
mdata$seasoncol<-as.factor(mdata$season)
levels(mdata$seasoncol)<-c("red", "blue", "black")

#ymax
m<-leaflet(data=mdata)
m<-addProviderTiles(map=m, provider="Stamen.TerrainBackground")
m<-addCircles(map=m, lat=~lat, lng=~lon, radius = ~ymax^2, 
              weight=1,opacity=1, color=~seasoncol, fill=NULL)
m

saveWidget(m, file="m.html")
saveWidget(m, file=file.path(outdir1, outdir2,"ymax.html"))

#wso
m<-leaflet(data=mdata)
m<-addProviderTiles(map=m, provider="Stamen.TerrainBackground")
m<-addCircles(map=m, lat=~lat, lng=~lon, radius = ~(0.5*wso)^2, 
              weight=1,opacity=1, color=~seasoncol, fill=NULL)
m
saveWidget(m, file=file.path(outdir1, outdir2,"wso.html"))

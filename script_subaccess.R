library(sf)
library(tmap)


full=st_read('ADA/Stations_ADA_Full.shp', layer="Stations_ADA_Full")
partial=st_read('ADA/Stations_ADA_Partial.shp', layer="Stations_ADA_Partial")
const=st_read('ADA/Stations_ADA_UnderConstruction.shp', layer="Stations_ADA_UnderConstruction")
noplan=st_read('NoADA/Stations_NoADA_NoPlans.shp', layer="Stations_NoADA_NoPlans")
ff=st_read('NoADA/Stations_NoADA_FastForwardIdentified.shp', layer="Stations_NoADA_FastForwardIdentified")
cg=st_read('NoADA/Stations_NoADA_CoverageGroups.shp', layer="Stations_NoADA_CoverageGroups")
sublines=st_read('Subway_Lines_2019/geo_export_d6a69987-1ce0-4e2c-8f9e-6ad76041a2bf.shp', layer="geo_export_d6a69987-1ce0-4e2c-8f9e-6ad76041a2bf")

#may need to get subway stops
#substops=

names(sublines)
#adding subway line colors
#http://web.mta.info/developers/resources/line_colors.htm
sublines$colors=c(rep("",length(sublines)))
sublines[which(sublines$rt_symbol=='A'),8]<-"#0039A6"
sublines[which(sublines$rt_symbol=='B'),8]<-"#FF6319"
sublines[which(sublines$rt_symbol=='G'),8]<-"#6CBE45"
sublines[which(sublines$rt_symbol=='J'),8]<-"#996633"
sublines[which(sublines$rt_symbol=='L'),8]<-"#A7A9AC"
sublines[which(sublines$rt_symbol=='N'),8]<-"#FCCC0A"
sublines[which(sublines$rt_symbol=='S'),8]<-"#808183"
sublines[which(sublines$rt_symbol=='1'),8]<-"#EE352E"
sublines[which(sublines$rt_symbol=='4'),8]<-"#00933C"
sublines[which(sublines$rt_symbol=='7'),8]<-"#B933AD"
sublines=sublines[,c(2,1,3:8)]

#mapping
#mapping parameters
tmap_options_reset()
tmap_mode("view")
tm_view(bbox = st_bbox(w1$geometry))
opts <- tmap_options(basemaps = c(Canvas = "CartoDB.Positron"), 
                     overlays = c(Labels = paste("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png")))
#council categorical color palette
council_pal<- c("#D05D4E","#12B886","#BE4BDB", "#F59F00", "#228AE6", "#A07952", "#82C91E")

#map
m<-
  tm_shape(sublines) + tm_lines(col = "colors", lwd=2) + 
  tm_shape(full) + tm_dots(size = 0.06, col = 'Incident', palette = council_pal, n = length(unique(w2$Incident)), 
                         popup.vars=c("Incidents"="Incident","Date"="creation_date","Duration (Hrs)"="duration")) + 
  tm_shape(geo_all) + tm_polygons(col="white", alpha = 0.7, border.col = "white", border.alpha = 0, popup.vars=FALSE)



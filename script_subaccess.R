#libraries ----
library(sf)
library(tmap)
library(leaflet)
library(councildown)
library(htmlwidgets)
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(purrr)
library(leaflet.extras)


#for filtering subway lines -----
full=st_read('ADA/Stations_ADA_Full.shp', layer="Stations_ADA_Full") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  as.data.frame() 
partial=st_read('ADA/Stations_ADA_Partial.shp', layer="Stations_ADA_Partial") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  as.data.frame()
const=st_read('ADA/Stations_ADA_ConstructionInProgress.shp', layer="Stations_ADA_ConstructionInProgress") %>%
  st_transform("+proj=longlat +datum=WGS84")%>%
  as.data.frame()
noplan=st_read('NoADA/Stations_NoADA_NoPlans.shp', layer="Stations_NoADA_NoPlans") %>%
  st_transform("+proj=longlat +datum=WGS84")%>%
  as.data.frame()
ff=st_read('NoADA/Stations_NoADA_UnderConsideration.shp', layer="Stations_NoADA_UnderConsideration") %>%
  st_transform("+proj=longlat +datum=WGS84")%>%
  as.data.frame()


allstops=rbind(full, partial, const, ff, noplan)
allstops<-st_as_sf(allstops)

allstops1=data.table(allstops[,c(3,5,7:8)])

#fixing line column to filter -----
allstops1$line=gsub(" Express", "", allstops$line)
#####
s=list()
for (i in 1:nrow(allstops1)){
  s[i]=strsplit(allstops1$line[i],"-")
}
#####make lines unique
u2=c()
for (i in 1:length(s))
{
  u2[i]=paste(sort(unique(unlist(s[[i]]))),  collapse="-")
}

s1=list()
for (i in 1:length(u2)){
  s1[i]=strsplit(u2[i],"-")
}

#s1[1]

####add unique lines to dataframe
allstops1$s=s1
########################
#creating station/point for each line
allstops1=allstops1 %>% 
  mutate(s=map(s,~tibble(s=.))) %>% 
  unnest(s, .drop = FALSE)
#add subway colors from sublines for filtering by subway lines-----
allstops1$colors<-c(rep("",nrow(allstops1)))
allstops1[which(allstops1$s=='A'|allstops1$s=='C'|allstops1$s=='E' ),6]<-"#0039A6"
allstops1[which(allstops1$s=='B'|allstops1$s=='D'|allstops1$s=='F'|allstops1$s=='M'),6]<-"#FF6319"
allstops1[which(allstops1$s=='G'),6]<-"#6CBE45"
allstops1[which(allstops1$s=='J'|allstops1$s=='Z'),6]<-"#996633"
allstops1[which(allstops1$s=='L'),6]<-"#A7A9AC"
allstops1[which(allstops1$s=='N'|allstops1$s=='Q'|allstops1$s=='R'|allstops1$s=='W'),6]<-"#FCCC0A"
allstops1[which(allstops1$s=='S'),6]<-"#808183"
allstops1[which(allstops1$s=='1'|allstops1$s=='2'|allstops1$s=='3'),6]<-"#EE352E"
allstops1[which(allstops1$s=='4'|allstops1$s=='5'|allstops1$s=='6'),6]<-"#00933C"
allstops1[which(allstops1$s=='7'),6]<-"#B933AD"

#converting into shapefile ----
allstops1<-st_as_sf(allstops1) %>%
  st_transform("+proj=longlat +datum=WGS84")


#subsetting each lines into its own shapefile for leaflet layer selector ----
sub_w=allstops1[which(allstops1$s==sort(unique(allstops1$s))[22]),]
sub_z=allstops1[which(allstops1$s==sort(unique(allstops1$s))[23]),]
sub1=allstops1[which(allstops1$s==sort(unique(allstops1$s))[1]),]
sub2=allstops1[which(allstops1$s==sort(unique(allstops1$s))[2]),]
sub3=allstops1[which(allstops1$s==sort(unique(allstops1$s))[3]),]
sub4=allstops1[which(allstops1$s==sort(unique(allstops1$s))[4]),]
sub5=allstops1[which(allstops1$s==sort(unique(allstops1$s))[5]),]
sub6=allstops1[which(allstops1$s==sort(unique(allstops1$s))[6]),]
sub7=allstops1[which(allstops1$s==sort(unique(allstops1$s))[7]),]
sub_a=allstops1[which(allstops1$s==sort(unique(allstops1$s))[8]),]
sub_b=allstops1[which(allstops1$s==sort(unique(allstops1$s))[9]),]
sub_c=allstops1[which(allstops1$s==sort(unique(allstops1$s))[10]),]
sub_d=allstops1[which(allstops1$s==sort(unique(allstops1$s))[11]),]
sub_e=allstops1[which(allstops1$s==sort(unique(allstops1$s))[12]),]
sub_f=allstops1[which(allstops1$s==sort(unique(allstops1$s))[13]),]
sub_g=allstops1[which(allstops1$s==sort(unique(allstops1$s))[14]),]
sub_j=allstops1[which(allstops1$s==sort(unique(allstops1$s))[15]),]
sub_l=allstops1[which(allstops1$s==sort(unique(allstops1$s))[16]),]
sub_m=allstops1[which(allstops1$s==sort(unique(allstops1$s))[17]),]
sub_n=allstops1[which(allstops1$s==sort(unique(allstops1$s))[18]),]
sub_q=allstops1[which(allstops1$s==sort(unique(allstops1$s))[19]),]
sub_r=allstops1[which(allstops1$s==sort(unique(allstops1$s))[20]),]
sub_s=allstops1[which(allstops1$s==sort(unique(allstops1$s))[21]),]


#for filtering by accessibility type ---------
full = st_read('ADA/Stations_ADA_Full.shp', layer = "Stations_ADA_Full") %>%
  st_transform("+proj=longlat +datum=WGS84")
partial = st_read('ADA/Stations_ADA_Partial.shp', layer = "Stations_ADA_Partial") %>%
  st_transform("+proj=longlat +datum=WGS84")
const = st_read('ADA/Stations_ADA_ConstructionInProgress.shp', layer = "Stations_ADA_ConstructionInProgress") %>%
  st_transform("+proj=longlat +datum=WGS84")
noplan = st_read('NoADA/Stations_NoADA_NoPlans.shp', layer = "Stations_NoADA_NoPlans") %>%
  st_transform("+proj=longlat +datum=WGS84")
ff = st_read('NoADA/Stations_NoADA_UnderConsideration.shp', layer = "Stations_NoADA_UnderConsideration") %>%
  st_transform("+proj=longlat +datum=WGS84")
sublines = st_read('Subway_Lines_2019/geo_export_d6a69987-1ce0-4e2c-8f9e-6ad76041a2bf.shp',
                   layer = "geo_export_d6a69987-1ce0-4e2c-8f9e-6ad76041a2bf") %>%
  st_transform("+proj=longlat +datum=WGS84")



#adding subway line colors for filtering by accessibility type----------
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


#reorder columns for popup -----
sublines=sublines[,c(2,1,3:8)]
full=full[,c(3,5,7,1,2,4,6,8)]
partial=partial[,c(3,5,7,1,2,4,6,8)]
const=const[,c(3,5,7,1,2,4,6,8)]
ff=ff[,c(3,5,7,1,2,4,6,8)]
noplan=noplan[,c(3,5,7,1,2,4,6,8)]

#council categorical color palette
council_pal<- c("#D05D4E","#12B886","#BE4BDB", "#F59F00", "#228AE6", "#A07952", "#82C91E")


#using leaflet


#for legend icons in layor selector for overlay groups ------------
un1=unname(paste0("<div style='background-color:","#228AE6",
                  ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                  'Full Accessibility'))
un2=unname(paste0("<div style='background-color:","#82C91E",
                  ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                  'Partial Accessibility'))
un3=unname(paste0("<div style='background-color:","#BE4BDB",
                  ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                  'In Construction'))
un4=unname(paste0("<div style='background-color:","#D05D4E",
                  ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                  'No ADA: Under Consideration'))
un5=unname(paste0("<div style='background-color:","#666666",
                  ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                  'No ADA: No Funding Plans'))


#for legend icons in layor selector for base groups -------------------
sub1_l=unname(paste0("<div style='background-color:","#EE352E",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '1'))
sub2_l=unname(paste0("<div style='background-color:","#EE352E",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '2'))
sub3_l=unname(paste0("<div style='background-color:","#EE352E",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '3'))
sub4_l=unname(paste0("<div style='background-color:","#00933C",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '4'))
sub5_l=unname(paste0("<div style='background-color:","#00933C",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '5'))
sub6_l=unname(paste0("<div style='background-color:","#00933C",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '6'))
sub7_l=unname(paste0("<div style='background-color:","#B933AD",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     '7'))
sub_al=unname(paste0("<div style='background-color:","#0039A6",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'A'))
sub_bl=unname(paste0("<div style='background-color:","#FF6319",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'B'))
sub_cl=unname(paste0("<div style='background-color:","#0039A6",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'C'))
sub_dl=unname(paste0("<div style='background-color:","#FF6319",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'D'))
sub_fl=unname(paste0("<div style='background-color:","#FF6319",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'F'))
sub_gl=unname(paste0("<div style='background-color:","#6CBE45",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'G'))
sub_jl=unname(paste0("<div style='background-color:","#996633",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'J'))
sub_ll=unname(paste0("<div style='background-color:","#A7A9AC",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'L'))
sub_ml=unname(paste0("<div style='background-color:","#FF6319",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'M'))
sub_nl=unname(paste0("<div style='background-color:","#FCCC0A",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'N'))
sub_ql=unname(paste0("<div style='background-color:","#FCCC0A",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'Q'))
sub_rl=unname(paste0("<div style='background-color:","#FCCC0A",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'R'))
sub_sl=unname(paste0("<div style='background-color:","#808183",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'S'))
sub_wl=unname(paste0("<div style='background-color:","#FCCC0A",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'W'))
sub_zl=unname(paste0("<div style='background-color:","#996633",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'Z'))
sub_el=unname(paste0("<div style='background-color:","#0039A6",
                     ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
                     'E'))



#mapping ------------
map <- leaflet() %>% 
  addCouncilStyle() %>% 
  #base groups ----
  addCircleMarkers(data = sub1,color =sub1$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub1$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub1$line, "<br><b>","ADA Status:", "</b>","<br>",sub1$ADA_Status)),
                   group = sub1_l, fillOpacity = 1,weight = 0.5,label = sub1$name,opacity = 0) %>%
  addCircleMarkers(data = sub2,color =sub2$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub2$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub2$line, "<br><b>","ADA Status:", "</b>","<br>",sub2$ADA_Status)),
                   group = sub2_l, fillOpacity = 1,weight = 0.5,label = sub2$name,opacity = 0) %>%
  addCircleMarkers(data = sub3,color =sub3$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub3$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub3$line, "<br><b>","ADA Status:", "</b>","<br>",sub3$ADA_Status)),
                   group = sub3_l, fillOpacity = 1,weight = 0.5,label = sub3$name,opacity = 0) %>%
  addCircleMarkers(data = sub4,color =sub4$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub4$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub4$line, "<br><b>","ADA Status:", "</b>","<br>",sub4$ADA_Status)),
                   group = sub4_l, fillOpacity = 1,weight = 0.5,label = sub4$name,opacity = 0) %>%
  addCircleMarkers(data = sub5,color =sub5$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub5$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub5$line, "<br><b>","ADA Status:", "</b>","<br>",sub5$ADA_Status)),
                   group = sub5_l, fillOpacity = 1,weight = 0.5,label = sub5$name,opacity = 0) %>%
  addCircleMarkers(data = sub6,color =sub6$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub6$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub6$line, "<br><b>","ADA Status:", "</b>","<br>",sub6$ADA_Status)),
                   group = sub6_l, fillOpacity = 1,weight = 0.5,label = sub6$name,opacity = 0) %>%
  addCircleMarkers(data = sub7,color =sub7$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub7$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub7$line, "<br><b>","ADA Status:", "</b>","<br>",sub7$ADA_Status)),
                   group = sub7_l, fillOpacity = 1,weight = 0.5,label = sub7$name,opacity = 0) %>%
  addCircleMarkers(data = sub_a,color =sub_a$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_a$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_a$line, "<br><b>","ADA Status:", "</b>","<br>",sub_a$ADA_Status)),
                   group = sub_al, fillOpacity = 1,weight = 0.5,label = sub_a$name,opacity = 0) %>%
  addCircleMarkers(data = sub_b,color =sub_b$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_b$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_b$line, "<br><b>","ADA Status:", "</b>","<br>",sub_b$ADA_Status)),
                   group = sub_bl, fillOpacity = 1,weight = 0.5,label = sub_b$name,opacity = 0) %>%
  addCircleMarkers(data = sub_c,color =sub_c$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_c$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_c$line, "<br><b>","ADA Status:", "</b>","<br>",sub_c$ADA_Status)),
                   group = sub_cl, fillOpacity = 1,weight = 0.5,label = sub_c$name,opacity = 0) %>%
  addCircleMarkers(data = sub_d,color =sub_d$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_d$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_d$line, "<br><b>","ADA Status:", "</b>","<br>",sub_d$ADA_Status)),
                   group = sub_dl, fillOpacity = 1,weight = 0.5,label = sub_d$name,opacity = 0) %>%
  addCircleMarkers(data = sub_e,color =sub_e$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_e$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_e$line, "<br><b>","ADA Status:", "</b>","<br>",sub_e$ADA_Status)),
                   group = sub_el, fillOpacity = 1,weight = 0.5,label = sub_e$name,opacity = 0) %>%
  addCircleMarkers(data = sub_f,color =sub_f$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_f$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_f$line, "<br><b>","ADA Status:", "</b>","<br>",sub_f$ADA_Status)),
                   group = sub_fl, fillOpacity = 1,weight = 0.5,label = sub_f$name,opacity = 0) %>%
  addCircleMarkers(data = sub_g,color =sub_g$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_g$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_g$line, "<br><b>","ADA Status:", "</b>","<br>",sub_g$ADA_Status)),
                   group = sub_gl, fillOpacity = 1,weight = 0.5,label = sub_g$name,opacity = 0) %>%
  addCircleMarkers(data = sub_j,color =sub_j$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_j$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_j$line, "<br><b>","ADA Status:", "</b>","<br>",sub_j$ADA_Status)),
                   group = sub_jl, fillOpacity = 1,weight = 0.5,label = sub_j$name,opacity = 0) %>%
  addCircleMarkers(data = sub_l,color =sub_l$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_l$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_l$line, "<br><b>","ADA Status:", "</b>","<br>",sub_l$ADA_Status)),
                   group = sub_ll, fillOpacity = 1,weight = 0.5,label = sub_l$name,opacity = 0) %>%
  addCircleMarkers(data = sub_m,color =sub_m$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_m$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_m$line, "<br><b>","ADA Status:", "</b>","<br>",sub_m$ADA_Status)),
                   group = sub_ml, fillOpacity = 1,weight = 0.5,label = sub_m$name,opacity = 0) %>%
  addCircleMarkers(data = sub_n,color =sub_n$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_n$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_n$line, "<br><b>","ADA Status:", "</b>","<br>",sub_n$ADA_Status)),
                   group = sub_nl, fillOpacity = 1,weight = 0.5,label = sub_n$name,opacity = 0) %>%
  addCircleMarkers(data = sub_q,color =sub_q$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_q$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_q$line, "<br><b>","ADA Status:", "</b>","<br>",sub_q$ADA_Status)),
                   group = sub_ql, fillOpacity = 1,weight = 0.5,label = sub_q$name,opacity = 0) %>%
  addCircleMarkers(data = sub_r,color =sub_r$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_r$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_r$line, "<br><b>","ADA Status:", "</b>","<br>",sub_r$ADA_Status)),
                   group = sub_rl, fillOpacity = 1,weight = 0.5,label = sub_r$name,opacity = 0) %>%
  addCircleMarkers(data = sub_w,color =sub_w$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_w$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_w$line, "<br><b>","ADA Status:", "</b>","<br>",sub_w$ADA_Status)),
                   group = sub_wl, fillOpacity = 1,weight = 0.5,label = sub_w$name,opacity = 0) %>%
  addCircleMarkers(data = sub_z,color =sub_z$colors, radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",sub_z$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>",
                           "Lines:","</b>", sub_z$line, "<br><b>","ADA Status:", "</b>","<br>",sub_z$ADA_Status)),
                   group = sub_zl, fillOpacity = 1,weight = 0.5,label = sub_z$name,opacity = 0) %>%
  
  
  #overlay groups ----
  addPolylines(data = sublines,weight = 3,color = sublines$colors,label = sublines$name,group = 'Lines') %>%
  addCircleMarkers(data = full,color = '#228AE6', radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",full$name,"</h3>","<hr>", "<b>","<font size=","0.5","'>","Lines:","</b>", 
                           full$line, "<br><b>","ADA Status:", "</b><br>",full$ADA_Status)),
                   group = un1, fillOpacity = 1,weight = 0.5,label = full$name,opacity = 0) %>%
  addCircleMarkers(data = partial,color = '#82C91E', radius = 4,
                 popup = councilPopup(
                   paste("<h3 class=","header-tiny",">",partial$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>","Lines:","</b>", 
                         partial$line, "<br><b>","ADA Status:", "</b><br>",partial$ADA_Status)),
                 group = un2, fillOpacity = 1,label = partial$name,weight = 0.5,opacity = 0) %>%
  addCircleMarkers(data = const,color = '#BE4BDB', radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",const$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>","Lines:","</b>", 
                           const$line, "<br><b>","ADA Status:", "</b><br>",const$ADA_Status)),
                   group = un3,label = const$name,fillOpacity = 1,weight = 0.5, opacity = 0) %>%
addCircleMarkers(data = ff, color = '#D05D4E', radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",ff$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>","Lines:","</b>", 
                           ff$line, "<br><b>","ADA Status:", "</b><br>",ff$ADA_Status)),
                   group = un4,label = ff$name,fillOpacity = 1,weight = 0.5,opacity = 0) %>%
  addCircleMarkers(data = noplan, color = '#666666', radius = 4,
                   popup = councilPopup(
                     paste("<h3 class=","header-tiny",">",noplan$name,"</h3>", "<hr>", "<b>","<font size=","0.5","'>","Lines:","</b>", 
                           noplan$line, "<br><b>","ADA Status:", "</b><br>",noplan$ADA_Status)),
                   group = un5,label = noplan$name,fillOpacity = 1,weight = 0.5,opacity = 0) %>%
  #layers control -----
  addLayersControl(overlayGroups = c(un1,un2,un3,un4,un5),
                   baseGroups = c(sub1_l,sub2_l,sub3_l,sub4_l,sub5_l,sub6_l,sub7_l,sub_al,sub_cl,sub_el,sub_bl,sub_dl,sub_fl,
                                  sub_ml, sub_gl, sub_ll,sub_nl,sub_ql,sub_rl,sub_wl,sub_al,sub_jl,sub_zl),
                   position = "bottomright", 
                   options = layersControlOptions(collapsed = FALSE, sortLayers = FALSE)) %>%
  #search control -----
  addResetMapButton() %>%   
  addControl("<P>Search by Station Name</P>",position='topright') %>%   
  addSearchFeatures(targetGroups =  c(un1,un2,un3,un4,un5,sub1_l,sub2_l,sub3_l,sub4_l,sub5_l,sub6_l,sub7_l,sub_al, 
                                      sub_cl,sub_el,sub_bl,sub_dl,sub_fl,sub_ml, sub_gl, sub_ll,sub_nl,sub_ql,
                                      sub_rl,sub_wl,sub_al,sub_jl,sub_zl),
                    options = searchFeaturesOptions(zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
                                                    autoCollapse = TRUE, hideMarkerOnCollapse = TRUE, position = "topright" )) %>%
  hideGroup(c(un1,un2,un3,un4,un5,sub1_l,sub2_l,sub3_l,sub4_l,sub5_l,sub6_l,sub7_l, sub_cl,sub_el,sub_bl,sub_dl,sub_fl,
              sub_ml, sub_gl, sub_ll,sub_nl,sub_ql,sub_rl,sub_wl,sub_al,sub_jl,sub_zl)) %>%
  
  #zoom parameters
  setView(-73.88099670410158,40.72540497175607,  zoom = 10.5)


#save a stand-alone, interactive map as an html file -------------
getwd()
htmlwidgets::saveWidget(map, file = 'subaccessmap.html', selfcontained = F)

    

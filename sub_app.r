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
library(shinyWidgets)
library(RSocrata)
library(readxl)
options(scipen = 999)

#borough geojson
bb=st_read('BoroughBoundaries.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84") %>% 
  st_simplify(dTolerance = .001)
bb=bb[,-1]
saveRDS(bb,"data/boroughs.rds")
#stations-----
ss=st_read('Subway_Stops_2019/stops_nyc_subway_may2019.shp', layer="stops_nyc_subway_may2019") %>%
  st_transform("+proj=longlat +datum=WGS84")

#for filtering subway lines -----
full=st_read('ADA/Stations_ADA_Full.shp', layer="Stations_ADA_Full") %>%
  st_transform("+proj=longlat +datum=WGS84")
partial=st_read('ADA/Stations_ADA_Partial.shp', layer="Stations_ADA_Partial") %>%
  st_transform("+proj=longlat +datum=WGS84")
partial_full=st_read('ADA/Stations_ADA_NextCapitalPlan_Partial.shp', layer="Stations_ADA_NextCapitalPlan_Partial") %>%
  st_transform("+proj=longlat +datum=WGS84")
const=st_read('ADA/Stations_ADA_ConstructionInProgress.shp', layer="Stations_ADA_ConstructionInProgress") %>%
  st_transform("+proj=longlat +datum=WGS84")
noplan=st_read('NoADA/Stations_NoADA.shp', layer="Stations_NoADA") %>%
  st_transform("+proj=longlat +datum=WGS84")
ff=st_read('NoADA/Stations_NoADA_NextCapitalPlan_Full.shp', layer="Stations_NoADA_NextCapitalPlan_Full") %>%
  st_transform("+proj=longlat +datum=WGS84")
ff$ADA_Status=rep("No Access - Committed to Fund", nrow(ff))
ff=ff[,c(1:6,8,7)]

full_sir=st_read('SIR/SIRail_ADA.shp', layer="SIRail_ADA") %>%
  st_transform("+proj=longlat +datum=WGS84") 
full_sir$ADA_Status=rep("Full ADA Access", nrow(full_sir))
full_sir=full_sir[,c(2,3,11,1,10)]

noplan_sir=st_read('SIR/SIRail_NoADA.shp', layer="SIRail_NoADA") %>%
  st_transform("+proj=longlat +datum=WGS84")
noplan_sir$ADA_Status=rep("No Access - No Plans for Funding", nrow(noplan_sir))
noplan_sir=noplan_sir[,c(2,3,11,1,10)]


ff_sir=st_read('SIR/SIRail_NextCapital.shp', layer="SIRail_NextCapital") %>%
  st_transform("+proj=longlat +datum=WGS84")
ff_sir$ADA_Status=rep("No Access - Under Consideration", nrow(ff_sir))
ff_sir=ff_sir[,c(2,3,11,1,10)]

#combining all stops ----
allstops=rbind(full, partial, partial_full, const, ff, noplan)
allstops=allstops[,c(3,5,7,4,8)]
all_sir=rbind(data.frame(full_sir),data.frame(noplan_sir),data.frame(ff_sir))
names(all_sir)<-names(allstops)
allstops=rbind(data.frame(allstops),data.frame(all_sir))

###changing ada status labels -----
allstops$ADA_StatusLayer=as.character(allstops$ADA_Status)
allstops[allstops$ADA_StatusLayer=='Partial ADA Acccess southbound only',6]<-"Partial ADA Access"
allstops[which(allstops$ADA_StatusLayer=='Partial ADA Access northbound only'),6]<-'Partial ADA Access'
allstops[which(allstops$ADA_StatusLayer=='Partial ADA Access soutbound only'),6]<-'Partial ADA Access'
allstops[which(allstops$ADA_StatusLayer=='Partial ADA Access Southbound Only'),6]<-'Partial ADA Access'

allstops[which(allstops$objectid=='123'),6]<-'Partial: Funding for Full ADA Access'
allstops[which(allstops$objectid=='286'),6]<-'Partial: Funding for Full ADA Access'
allstops[which(allstops$objectid=='344'),6]<-'Partial: Funding for Full ADA Access'

allstops[which(allstops$ADA_StatusLayer=='ADA Access Under Construction'),6]<-'Construction in Progress'
allstops[which(allstops$ADA_StatusLayer=='No Access - Committed to Fund'),6]<-'No ADA: Funding Committed'
allstops[which(allstops$ADA_StatusLayer=='No Access - Under Consideration'),6]<-'No ADA Access'
allstops[which(allstops$ADA_StatusLayer=='No Access - No Plans for Funding'),6]<-'No ADA Access'
allstops[which(allstops$ADA_StatusLayer=='No Access - Under Consideration'),6]<-'No ADA Access'

allstops[which(allstops$objectid=='S16'),6]<-'No ADA: Funding Committed'
allstops[which(allstops$objectid=='S22'),6]<-'No ADA: Funding Committed'
allstops[which(allstops$objectid=='S28'),6]<-'No ADA: Funding Committed'

#adding station ids
st_ids=read.csv('Subway_Stops_2019/stopsmatch.csv', stringsAsFactors = FALSE)
st_ids$match1=paste(st_ids$name,st_ids$objectid,sep = " ")
allstops$match1=paste(allstops$name,allstops$objectid,sep = " ")
allstops=left_join(allstops,st_ids,by='match1', suffix=c("",".y"))
allstops=allstops[,-c(8:16)]

#allstops 1 
allstops1=data.table(allstops)

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
##add clean lines
allstops1$lines2=u2
########################
#creating station/point for each line
allstops1=allstops1 %>% 
  mutate(s=map(s,~tibble(s=.))) %>% 
  unnest(s, .drop = FALSE)
#add subway colors from sublines for filtering by subway lines-----
allstops1$linecolors<-c(rep("",nrow(allstops1)))
allstops1[which(allstops1$s=='A'|allstops1$s=='C'|allstops1$s=='E' ),]$linecolors<-"#0039A6"
allstops1[which(allstops1$s=='B'|allstops1$s=='D'|allstops1$s=='F'|allstops1$s=='M'),]$linecolors<-"#FF6319"
allstops1[which(allstops1$s=='G'),]$linecolors<-"#6CBE45"
allstops1[which(allstops1$s=='J'|allstops1$s=='Z'),]$linecolors<-"#996633"
allstops1[which(allstops1$s=='L'),]$linecolors<-"#A7A9AC"
allstops1[which(allstops1$s=='N'|allstops1$s=='Q'|allstops1$s=='R'|allstops1$s=='W'),]$linecolors<-"#FCCC0A"
allstops1[which(allstops1$s=='S'),]$linecolors<-"#808183"
allstops1[which(allstops1$s=='1'|allstops1$s=='2'|allstops1$s=='3'),]$linecolors<-"#EE352E"
allstops1[which(allstops1$s=='4'|allstops1$s=='5'|allstops1$s=='6'),]$linecolors<-"#00933C"
allstops1[which(allstops1$s=='7'),]$linecolors<-"#B933AD"
allstops1[which(allstops1$s=='SIR'),]$linecolors<-"#053159"

###changing ada status labels -----
# allstops1$ADA_StatusLayer=as.character(allstops1$ADA_Status)
# allstops1[allstops1$ADA_StatusLayer=='Partial ADA Acccess southbound only',8]<-"Partial ADA Access"
# allstops1[which(allstops1$ADA_StatusLayer=='Partial ADA Access northbound only'),8]<-'Partial ADA Access'
# allstops1[which(allstops1$ADA_StatusLayer=='Partial ADA Access soutbound only'),8]<-'Partial ADA Access'
# allstops1[which(allstops1$ADA_StatusLayer=='Partial ADA Access Southbound Only'),8]<-'Partial ADA Access'
# allstops1[which(allstops1$ADA_StatusLayer=='ADA Access Under Construction'),8]<-'Construction in Progress'
# allstops1[which(allstops1$ADA_StatusLayer=='No Access - Under Consideration'),8]<-'No ADA: Under Consideration'
# allstops1[which(allstops1$ADA_StatusLayer=='No Access - No Plans for Funding'),8]<-'No ADA: No Funding Plans'

#add subway colors from sublines for filtering by ada status type-----
allstops1$adacolors<-c(rep("",nrow(allstops1)))
allstops1[which(allstops1$ADA_StatusLayer=="Full ADA Access"),]$adacolors<-"#1D5ED7"
allstops1[which(allstops1$ADA_StatusLayer=="Partial ADA Access"),]$adacolors<-"#007535"
allstops1[which(allstops1$ADA_StatusLayer=="Partial: Funding for Full ADA Access"),]$adacolors<-"#A427C4"
allstops1[which(allstops1$ADA_StatusLayer=="Construction in Progress"),]$adacolors<-"#6C4BCE"
allstops1[which(allstops1$ADA_StatusLayer=="No ADA: Funding Committed"),]$adacolors<-"#A80000"
allstops1[which(allstops1$ADA_StatusLayer=="No ADA Access"),]$adacolors<-"#4A4A4A"


#converting into shapefile ----
allstops1<-st_as_sf(allstops1) %>%
  st_transform("+proj=longlat +datum=WGS84")

#search by station name and line -----
allstops1$stationline=paste(allstops1$name, ": ", allstops1$s, sep = "")

#adding elevator outages ----
ee=read.csv('elevator/out_lines_new.csv', stringsAsFactors = FALSE)
ee$stationline=paste(ee$name_in_Rose_Data_set, ": ", ee$s, sep = "")
ee$X1st_Quarter_2019_24_Hour_Availability_=
  as.numeric(substr(ee$X1st_Quarter_2019_24_Hour_Availability_,1,nchar(ee$X1st_Quarter_2019_24_Hour_Availability_)-1))
ee$X1st_Quarter_2019_24_Hour_Availability_AM_Peak=
  as.numeric(substr(ee$X1st_Quarter_2019_24_Hour_Availability_AM_Peak,1,nchar(ee$X1st_Quarter_2019_24_Hour_Availability_AM_Peak)-1))
ee$unavailability=100-ee$X1st_Quarter_2019_24_Hour_Availability_
#calculate the number of elevators throughout system (135/494)
ee$locationline=paste(ee$Location, ee$Lines, sep=" ")
length(unique(ee$locationline))

#add replacement definition column ----
ee$status2=rep("",nrow(ee))
ee[grep('[**]',ee$Elevator_ID),]$status2<-"Removed from service during first quarter of 2019 for replacement"
ee$Elevator_ID=gsub("[*]", "", ee$Elevator_ID)

#ADA elevator performance-----
e1=read.csv('elevator/elevators_jan19.csv', stringsAsFactors = FALSE)
e2=read.csv('elevator/elevators_feb19.csv', stringsAsFactors = FALSE)
e3=read.csv('elevator/elevators_march19.csv', stringsAsFactors = FALSE)
e4=read.csv('elevator/elevators_april19.csv', stringsAsFactors = FALSE)
e5=read.csv('elevator/elevators_may19.csv', stringsAsFactors = FALSE)
e6=read.csv('elevator/elevators_june19.csv', stringsAsFactors = FALSE)
e7=read.csv('elevator/elevators_july19.csv', stringsAsFactors = FALSE)
e8=read.csv('elevator/elevators_aug19.csv', stringsAsFactors = FALSE)

#add month column
e1$month=rep('January', nrow(e1))
e2$month=rep('February', nrow(e2))
e3$month=rep('March', nrow(e3))
e4$month=rep('April', nrow(e4))
e5$month=rep('May', nrow(e5))
e6$month=rep('June', nrow(e6))
e7$month=rep('July', nrow(e7))
e8$month=rep('August', nrow(e8))

#rbind months
etm=rbind(e1,e2,e3,e4,e5,e6,e7,e8)

#add accessibility status column
etm$ADA_Status=rep('ADA', nrow(etm))

#make availability percent numeric
etm$X24Hr=as.numeric(substr(etm$X24Hr,1,nchar(etm$X24Hr)-2))/100
etm$AM=as.numeric(substr(etm$AM,1,nchar(etm$AM)-2))/100
etm$PM=as.numeric(substr(etm$AM,1,nchar(etm$PM)-2))/100

#Not ADA elevator performance----
ne1=read.csv('elevator/not_elevators_jan19.csv', stringsAsFactors = FALSE)
ne2=read.csv('elevator/not_elevators_feb19.csv', stringsAsFactors = FALSE)
ne3=read.csv('elevator/not_elevators_march19.csv', stringsAsFactors = FALSE)
ne4=read.csv('elevator/not_elevators_april19.csv', stringsAsFactors = FALSE)
ne5=read.csv('elevator/not_elevators_may19.csv', stringsAsFactors = FALSE)
ne6=read.csv('elevator/not_elevators_june19.csv', stringsAsFactors = FALSE)
ne7=read.csv('elevator/not_elevators_july19.csv', stringsAsFactors = FALSE)
ne8=read.csv('elevator/not_elevators_aug19.csv', stringsAsFactors = FALSE)

#add month column
ne1$month=rep('January', nrow(ne1))
ne2$month=rep('February', nrow(ne2))
ne3$month=rep('March', nrow(ne3))
ne4$month=rep('April', nrow(ne4))
ne5$month=rep('May', nrow(ne5))
ne6$month=rep('June', nrow(ne6))
ne7$month=rep('July', nrow(ne7))
ne8$month=rep('August', nrow(ne8))


#rbind months
netm=rbind(ne1,ne2,ne3,ne4,ne5,ne6,ne7,ne8)

#add accessibility status column
netm$ADA_Status=rep('Not ADA', nrow(netm))

#make availability percent numeric
netm$X24Hr=as.numeric(substr(netm$X24Hr,1,nchar(netm$X24Hr)-2))/100
netm$AM=as.numeric(substr(netm$AM,1,nchar(netm$AM)-2))/100
netm$PM=as.numeric(substr(netm$AM,1,nchar(netm$PM)-2))/100

#rbind netm and etm -----
fetm=rbind(etm,netm)
#adding whether those elevators are ADA or not ADA -----
t1=fetm[!duplicated(fetm$EquipmentNo),]


es=c()
el_list=c()
for (i in 1:length(ee$Elevator_ID)){
  w1=unique(ee$Elevator_ID)
  es[i]=t1[which(t1$EquipmentNo %in% ee$Elevator_ID[i]),]$ADA_Status
  el_list[i]=t1[which(t1$EquipmentNo %in% ee$Elevator_ID[i]),]$Station.Name
}

ee$ADA_Status=es
ee$Station.Name=el_list

#getting real time info ----
#t2=ee[!duplicated(ee$stationline),]
#t2$Station.Name=substr(t2$Station.Name,1,nchar(t2$Station.Name)-8)
#t2=t2[,c("stationline","Station.Name")]
#write.csv(t2,"rt_elevnames.csv", row.names = FALSE)

#outages ------
# so=c()
# for (i in 1:length(unique(ee$locationline))){
# so[i]=ee[which(ee$locationline==unique(ee$locationline)[i]),]$Scheduled_Outages
# }
# uo=c()
# for (i in 1:length(unique(ee$locationline))){
#   uo[i]=ee[which(ee$locationline==unique(ee$locationline)[i]),]$Non_Scheduled_Outages
# }

#outages ------
fetm[is.na(fetm$Outages)==TRUE,]$Outages<-0
fetm[is.na(fetm$Scheduled)==TRUE,]$Scheduled<-0
fetm[is.na(fetm$Non.Scheduled)==TRUE,]$Non.Scheduled<-0
so=c()
for (i in 1:length(unique(fetm$Station.Name))){
so[i]=sum(fetm[which(fetm$Station.Name==unique(fetm$Station.Name)[i]),]$Scheduled)
}
uo=c()
for (i in 1:length(unique(fetm$Station.Name))){
  uo[i]=sum(fetm[which(fetm$Station.Name==unique(fetm$Station.Name)[i]),]$Non.Scheduled)
}

uu=c()

for (i in 1:length(unique(fetm$Station.Name))){
  uu[i]=sum(fetm[which(fetm$Station.Name==unique(fetm$Station.Name)[i]),]$Outages)
}

#outages stats for 2019 to aug-----
sum(fetm$Outages, na.rm = TRUE)
sum(fetm$Scheduled, na.rm = TRUE)
sum(fetm$Non.Scheduled, na.rm = TRUE)

out=as.data.frame(cbind(so,uo,uu,unique(fetm$Station.Name) ))

#elevator info for map popup ----
r=list()
r1=list()
no_service=c()
num_el=c()
el_stat=c()

for (i in 1:length(unique(ee$stationline))){
  num_el[i]=nrow(ee[ee$stationline==unique(ee$stationline)[i],])
  r[[i]]=ee[ee$stationline==unique(ee$stationline)[i],]$unavailability
  r1[[i]]=ee[ee$stationline==unique(ee$stationline)[i],]$ADA_Status

  if (length(unique(round(unlist(r[[i]]),1)))>1){
    no_service[i]=paste(c(min(unique(round(unlist(r[[i]]),1)))," - ",max(unique(round(unlist(r[[i]]),1))), "%"),  collapse="")
  } else{no_service[i]=paste(c(unique(round(unlist(r[[i]]),1)), "%"),  collapse="")}
  if (length(unlist(r[i]))>1) {
  el_stat[i]=paste(unique(unlist(r1[[i]])), collapse=" and ")}
  else{el_stat[i]=unlist(r1[[i]])}

}



ee2=data.frame(cbind(num_el, no_service, el_stat, unique(ee$stationline) ))
names(ee2)[4]<-"stationline"
ee2$stationline=as.character(ee2$stationline)

#join elevator info to stations (allstops) ----
allstops1=left_join(allstops1,ee2,by="stationline", suffix=c("",".y"))
#saving allstops as and rds file ----
st_write(allstops1, "allstops.geojson", driver = "GeoJSON", delete_dsn=TRUE)
saveRDS(allstops1,"data/allstops.rds")

#baruch gis file has colors 
#https://www.baruch.cuny.edu/confluence/pages/viewpage.action?pageId=28016896
sublines2 = st_read('Subway_Lines_2019/routes_nyc_subway_may2019.shp',
                    layer = "routes_nyc_subway_may2019", stringsAsFactors=FALSE) %>%
  st_transform("+proj=longlat +datum=WGS84")

st=sublines2[sublines2$group=='SIR',]%>% 
  st_simplify(dTolerance = as.numeric(.0001))
#making sublines load faster
os= st_read('Subway_Lines_2019/SubwayLines.geojson') %>%
  st_transform("+proj=longlat +datum=WGS84")
#add color column
#adding subway line colors for filtering by accessibility type----------
#http://web.mta.info/developers/resources/line_colors.htm
os$color<-c(rep("",nrow(os)))
os[which(os$name=='A'|os$name=='C'|os$name=='E'|os$name=='A-C'|os$name=='A-C-E'),]$color<-"#0039A6"
os[which(os$name=='B'|os$name=='D'|os$name=='F'|os$name=='M'|os$name=='B-D'|os$name=='B-D-F-M'|os$name=='F-M'),]$color<-"#FF6319"
os[which(os$name=='G'),]$color<-"#6CBE45"
os[which(os$name=='J-Z'),]$color<-"#996633"
os[which(os$name=='L'),]$color<-"#A7A9AC"
os[which(os$name=='N'|os$name=='Q'|os$name=='R'|os$name=='N-Q'|os$name=='N-Q-R'|os$name=='N-R'|os$name=='N-R-W'|os$name=='N-W'|os$name=='R-W'|os$name=='N-Q-R-W'),]$color<-"#FCCC0A"
os[which(os$name=='S'),]$color<-"#808183"
os[which(os$name=='1'|os$name=='2'|os$name=='3'|os$name=='1-2-3'|os$name=='2-3'),]$color<-"#EE352E"
os[which(os$name=='4'|os$name=='5'|os$name=='6'|os$name=='4-5'|os$name=='4-5-6'),]$color<-"#00933C"
os[which(os$name=='7'),]$color<-"#B933AD"
#subsetting----
os=os[,c(1,8,7)]
st=st[,c(2,4,6)]
names(st)[1]<-"name"

os=as.data.frame(os)
st=as.data.frame(st)
nt=rbind(os,st)%>%
  st_as_sf()

st_write(nt,"Subway_Lines_2019/newsublines.geojson", driver = "GeoJSON", delete_dsn=TRUE)

saveRDS(nt,"data/sublines.rds")

#elevator mta complaints-----
cc=read.socrata('https://data.ny.gov/resource/tppa-s6t6.json?$where=year>2013&commendation_or_complaint=Complaint&subject_detail=Elevators&agency=Subways')
sort(table(cc[cc$year==2018,]$issue_detail), decreasing = TRUE)[1:5]
table(cc[cc$issue_detail=='Add More / Not Enough',]$year,cc[cc$issue_detail=='Add More / Not Enough',]$branch_line_route )
sort(table(cc[cc$issue_detail=='Add More / Not Enough',]$branch_line_route),decreasing = TRUE)[1:5]
sort(table(cc[cc$year==2018 & cc$issue_detail=='Add More / Not Enough',]$branch_line_route),decreasing = TRUE)[1:5]
library(shiny)
library(maps)
library(mapdata)
library(Rmisc)
library(tidyverse)
sta_data=read.csv("/Users/zhangxiaomeng/Dropbox/ocean/CCG_AIS_Static_Data_2018-05-01.csv")
dy_data=read.csv("/Users/zhangxiaomeng/Dropbox/ocean/CCG_AIS_Dynamic_Data_2018-05-01.csv")
head(sta_data)
head(dy_data)
factor(dy_data$hour)

name
dy_data[which(dy_data$Longitude_decimal_degrees>   -64& dy_data$Latitude_decimal_degrees <47),]

dy_data<-dy_data%>%
  filter(Longitude_decimal_degrees<   -64& Latitude_decimal_degrees >47)%>%
  select("Station_Location","hour","MMSI","speed"="Speed_Over_Ground_SOG_knots","lon"="Longitude_decimal_degrees","lat"="Latitude_decimal_degrees",
        "COG"="Course_Over_Ground_COG_degrees")%>%
        filter(Station_Location!="Unknown")
dim(dy_data)
size(dy_data_ocean)
dy_data_mean<-dy_data%>%
  group_by(Station_Location)%>%
  summarise(Meanlon=mean(lon),Maxlon=max(lon),Minlon=min(lon),
            Meanlat=mean(lat),Maxlat=max(lat),Minlat=min(lat))
dy_data_mean<-dy_data_mean%>%
  filter(Minlon>-200)
install.packages("geosphere")
dim(dy_data_mean)

dy_data_mean<-as.data.frame(dy_data_mean)

canada <- map_data("worldHires", "Canada")

ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data_mean, aes(x=Meanlon, y=Meanlat,fill=Station_Location), shape=21, size=2.0)+
  geom_point(data=dy_data_mean, aes(x=Minlon, y=Minlat,fill=Station_Location), shape=21, size=2.0)+
  geom_point(data=dy_data_mean, aes(x=Maxlon, y=Maxlat,fill=Station_Location), shape=21, size=2.0)+
geom_line()
library(geosphere)

dy_hour<-dy_data%>%
  group_by(hour)
  
ggplot(df)+
  geom_line(aes(x=1:22,y=X..i..))+
  ylab("the total times detected by the station")+
  xlab("hour")
  

ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data[which(dy_data$hour==10),], aes(x=lon, y=lat,size=speed,fill=Station_Location,alpha=0.2), shape=21)+
  xlim(-138,-120)+
  ylim(45,58)+
  scale_alpha(guide=F)+
 scale_size(guide = F)
#######################
ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data[which(dy_data$lon>-137.5&dy_data$lon<    -125&dy_data$lat>50&dy_data$lat<55),], aes(x=lon, y=lat,size=speed,fill=Station_Location,alpha=0.2), shape=21)+
  
  scale_alpha(guide=F)+
  scale_size(guide = F)+
  xlim(-140,-125)+
  ylim(50,55)

dy_density<-dy_data[which(dy_data$lon>-137.5&dy_data$lon<    -125&dy_data$lat>50&dy_data$lat<55),]
ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_density[which(dy_density$hour==3),], aes(x=lon, y=lat,size=speed,fill=Station_Location,alpha=0.2), shape=21)+
  
  scale_alpha(guide=F)+
  scale_size(guide = F)+
  xlim(-140,-125)+
  ylim(50,55)


dy_data[which(dy_data$lon>-137.5&dy_data$lon<    -125&dy_data$lat>50&dy_data$lat<55),]

dy_density[which(dy_density$hour==3),4]
dy_density

ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data2[which(dy_data$hour==15),], aes(x=lon, y=lat,size=speed,fill=Station_Location,alpha=0.2), shape=21)+
  
  scale_alpha(guide=F)+
  scale_size(guide = F)



  scale
dy_data
dy_data_speed<-dy_data

#########mean of speed<=5===0.5
dy_data[which(dy_data$speed<=5),4]<-0.5

########mean7.8

mean(dy_data[which(dy_data$speed>5&dy_data$speed<=10),4])
dy_data[which(dy_data$speed>5&dy_data$speed<=10),4]<-7.8
#########mean 12.4

mean(dy_data[which(dy_data$speed>10&dy_data$speed<=15),4])
dy_data[which(dy_data$speed>10&dy_data$speed<=15),4]<-12.4

#############19.2
mean(dy_data[which(dy_data$speed>=15),4])
dy_data[which(dy_data$speed>=15),4]<-19.2

dy_data
l=lapply(2:23,function(x) dim(dy_data[which(dy_data$hour==x),]))

dim(l)
library (plyr)
df <- ldply (l, data.frame)
df<-df%>%
  filter(X..i..!=7)
df
df(which(df[1,]))
df[which(df[,1])==7]

dy_data_mean$Meanlon
l<-lapply(1:length(dy_data_mean$Meanlon), function(y) lapply(1:length(dy_data_mean$Meanlon),
                                                          function(x) {xy=rbind(c(dy_data_mean$Meanlon[x],dy_data_mean$Meanlat[x])
                                                             ,c(dy_data_mean$Meanlon[y],dy_data_mean$Meanlat[y]))
                                                          a=distm(xy)
                                                          
                                                        
                     
  
}))
l<-as.data.frame(l)

l

head(dy_data_mean)

dy_data_mean
tail(dy_data)
levels(factor(dy_data$MMSI))
sum(levels(factor(dy_data$MMSI)))
sum(levels(factor(dy_data$MMSI)))
dy_data[which(dy_data$Station_Location=="Unknown"),]
is.na(dy_data)

select()
name(sta_data)
duplicated(sta_data$MMSI)
length(unique(sta_data$MMSI))
unique(dy_data$MMSI)
sum(is.na(sta_data$MMSI))
sum(is.na(sta_data))
sum(is.na(dy_data))
sum(is.na(dy_data$MMSI))
dy_data[which(is.na(dy_data$MMSI)),]
length(unique(dy_data$MMSI))
setdiff(unique(dy_data$MMSI),unique(sta_data$MMSI))

ls()
canada <- map_data("worldHires", "Canada")





ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data, aes(x=lon, y=lat,fill=Station_Location,shape=21, size=2.0))

xy=rbind(c(40,30)
         ,c(41,30))
a=distm(xy)
a



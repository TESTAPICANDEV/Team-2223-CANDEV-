#sta_data2=read.csv("/Users/zhangxiaomeng/Dropbox/ocean/CCG_AIS_Static_Data_2018-05-01.csv")
dy_data2=read.csv("/Users/zhangxiaomeng/Dropbox/ocean/CCG_AIS_Dynamic_Data_2018-05-02.csv")

head(dy_data2)
factor(dy_data2$hour)

name
dy_data[which(dy_data$Longitude_decimal_degrees>   -64& dy_data$Latitude_decimal_degrees <47),]

dy_data2<-dy_data2%>%
  filter(Longitude_decimal_degrees<   -64& Latitude_decimal_degrees >47)%>%
  select("Station_Location","hour","MMSI","speed"="Speed_Over_Ground_SOG_knots","lon"="Longitude_decimal_degrees","lat"="Latitude_decimal_degrees",
         "COG"="Course_Over_Ground_COG_degrees")%>%
  filter(Station_Location!="Unknown")
head(dy_data2)
dim(dy_data2)
size(dy_data_ocean)
dy_data_mean2<-dy_data2%>%
  group_by(Station_Location)%>%
  summarise(Meanlon=mean(lon),Maxlon=max(lon),Minlon=min(lon),
            Meanlat=mean(lat),Maxlat=max(lat),Minlat=min(lat))
dy_data_mean2<-dy_data_mean2%>%
  filter(Minlon>-200)


head(dy_data_mean2)
install.packages("geosphere")
dim(dy_data_mean)

dy_data_mean2<-as.data.frame(dy_data_mean2)

canada <- map_data("worldHires", "Canada")

ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data_mean2, aes(x=Meanlon, y=Meanlat,fill=Station_Location), shape=21, size=2.0)+
  geom_point(data=dy_data_mean2, aes(x=Minlon, y=Minlat,fill=Station_Location), shape=21, size=2.0)+
  geom_point(data=dy_data_mean2, aes(x=Maxlon, y=Maxlat,fill=Station_Location), shape=21, size=2.0)+
  geom_line()
library(geosphere)


####################
ggplot()+
  geom_line(data=df,aes(x=1:22,y=X..i..),color="#CC0000")+###red
  geom_line(data=df2,aes(x=1:22,y=X..i..),color="#008B00")+###second day
  
  ylab("the total times detected by the station")+
  xlab("hour")
##################
ggplot()+
  geom_line(data=df4,aes(x=1:22,y=X..i..),color="#CC0000")+###red
  geom_line(data=df3,aes(x=1:22,y=X..i..),color="#008B00")+###second day
  
  ylab("the total number of ships in 1 hour")+
  xlab("hour")


ggplot() + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_point(data=dy_data2[which(dy_data2$hour==10),], aes(x=lon, y=lat,size=speed,fill=Station_Location,alpha=0.2), shape=21)+
  xlim(-138,-120)+
  ylim(45,58)+
  scale_alpha(guide=F)+
  scale_size(guide = F)


scale
dy_data
sum(is.na(dy_data2))
dy_data_speed<-dy_data

#########mean of speed<=5===0.5
mean(dy_data2[which(dy_data2$speed<=5),4])
dy_data2[which(dy_data2$speed<=5),4]<-0.5
########mean7.8

mean(dy_data2[which(dy_data2$speed>5&dy_data2$speed<=10),4])
dy_data2[which(dy_data2$speed>5&dy_data2$speed<=10),4]<-7.7
#########mean 12.4

mean(dy_data2[which(dy_data2$speed>10&dy_data2$speed<=15),4])
dy_data2[which(dy_data2$speed>10&dy_data2$speed<=15),4]<-12.3

#############19.2
mean(dy_data2[which(dy_data2$speed>=15),4])
dy_data2[which(dy_data2$speed>=15),4]<-19.4

dy_data2
dy_data
l2=lapply(2:23,function(x) dim(dy_data2[which(dy_data2$hour==x),]))
dim(dy_data2[which(dy_data2$hour==3),])
dim(l2)
dim(dy_data[which(dy_data$hour==3),])
head(dy_data2)
library (plyr)
length(unique(dy_data2[,MMSI]))
l3=lapply(2:23,function(x) length(unique(dy_data2[which(dy_data2$hour==x),3])))
l4=lapply(2:23,function(x) length(unique(dy_data2[which(dy_data$hour==x),3])))
l3

df4 <- ldply (l4, data.frame)
df3<-df3%>%
  filter(X..i..!=7)
df3
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



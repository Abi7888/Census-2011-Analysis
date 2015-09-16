
##--Summary--##

##- In this code, the following has been done ##
# 1)Getting the census data shape files
# 2)Converting it into a format that is helpful for plotting heat maps
# 3)Merging the co-ordinates data and the Data needed for plotting(eg state,religion,population nos etc)
# 4)Using ggplots and googleVis for plotting

#Data used
# 1)IND_adm.zop
# 2)data1.csv
# 3)final_merge.csv

require("rgdal")  # needed to load shapefiles
setwd("D:\\world bank data")
# Obtain India administrative shapefiles and unzip
download.file("http://biogeo.ucdavis.edu/data/diva/adm/IND_adm.zip", 
              destfile = "IND_adm.zip")
unzip("IND_adm.zip", overwrite = TRUE)

# load shapefiles
#You can download census data 2011 online(IND_adm1).It is generally stored as shape files

india <- readOGR("D:\\world bank data","IND_adm1",verbose = TRUE, stringsAsFactors = FALSE)

head(india@polygons)

india.df <- fortify(india)
str(india.df)
unique(india.df$id)
length(india.df$id)
india.df$id<-as.numeric(india.df$id)

unique(india@data$ID_1)
length(india@data$ID_1)
head(india.df)
summary(india.df)

india@data$ID_1<-unique(india.df$id)
shapeM<-merge(india.df,india@data,by.x="id",by.y="ID_1")
dim(shapeM)

# Sample dataframe of unemployment info
library(ggmap)
map<-get_map("india",maptype="terrain",zoom=5)

ggmap(map)+geom_polygon(data=shapeM,aes(x=long,y=lat,group=id,fill=Shape_Area),alpha=0.4,colour="black")+scale_fill_gradientn(colours = c("red","white","blue"))

names(shapeM)
unique(shapeM$)
pal3 <- colorRampPalette(c("red", "blue" ,"green"))
breaks <- c(quantile(shapeM$Shape_Area))

# Plot the map with ggplot2
q <- ggplot(data = shapeM, aes(x = long, y = lat, group = id))
q <- q + geom_polygon(color = "lightgrey", aes(fill = factor(Shape_Area),alpha=0.4))
q <- q + ggtitle("Area by Indian States")
q <- q + scale_fill_manual( values = pal3(35), breaks= breaks ) 
q <- q + labs(fill="Shape_Area")
q <- q + theme(panel.background = element_rect(fill = "white", colour = "grey"),
               panel.grid.major = element_line(colour = "grey")) 
q

##-----------------------------Data Cleaning -------------------------
# check they've loaded correctly with a plot
#plot(india)

data<-read.csv("D:\\world bank data\\data1.csv")
data<-data[-1,-1]
str(data)

data$STATE<-as.character(data$STATE)
data$STATE[1]

 data$STATE[1]<-"JAMMU AND KASHMIR"
 data$STATE[2]<-"HIMACHAL PRADESH"
 data$STATE[4]<-"CHANDIGARH"
 data$STATE[22]<-"CHHATTISGARH"
 data$STATE[7]<-"DELHI"
 data$STATE[9]<-"UTTAR PRADESH"
 data$STATE[12]<-"ARUNACHAL PRADESH"
 data$STATE[19]<-"WEST BENGAL"
 data$STATE[23]<-"MADHYA PRADESH"
 data$STATE[25]<-"DAMAN AND DIU"
 data$STATE[26]<-"DADRA AND NAGAR HAVELI"
 data$STATE[28]<-"ANDHRA PRADESH"
 data$STATE[31]<-"LAKSHADWEEP"
 data$STATE[33]<-"TAMIL NADU"
 data$STATE[34]<-"PUDUCHERRY"
 data$STATE[35]<-"ANDAMAN AND NICOBAR"

unique(data$STATE)
unique(shapeM$NAME_1)

data<-data[order(data$STATE),]
data$IND<-as.numeric(as.factor(data$STATE))

shapeM1<-shapeM[,c(1:3,11,21,22)]
shapeM1$IND<-as.numeric(as.factor(shapeM1$STATE))

str(shapeM1)
final_merge<-merge(shapeM1,data,by="IND",all.x=TRUE)
dim(final_merge)

names(data)

write.csv(data,"data2.csv",row.names=F)
write.csv(final_merge,"final_merge.csv",row.names=F)

##Plot new columns now :
names(final_merge)
final_merge<-read.csv("final_merge.csv")
names(final_merge)
unique(final_merge$STATE.x)

library(ggplot2)
##By Literacy Rate
pal3 <- colorRampPalette(c("red", "blue" ,"green"))
breaks <- c(quantile(final_merge$Literacy.rate.total))

# Plot the map with ggplot2
# q <- ggplot(data = final_merge, aes(x = long, y = lat, group = id))
# q <- q + geom_polygon(color = "lightgrey", aes(fill = factor(Literacy.rate.total)))
# q <- q + ggtitle("Literacy.rate.total by Indian States")
# q <- q + scale_fill_manual( values = pal3(35), breaks= breaks ) 
# q <- q + labs(fill="Literacy rate total")
# q <- q + theme(panel.background = element_rect(fill = "white", colour = "grey"),
#                panel.grid.major = element_line(colour = "grey")) 
# q + ggmap(map) 
# ggmap(map)


##----------------------------------------------Heat Maps --------------------------------------------------##
##Literacy Rate by States

## check
data[order(data$Literacy.rate.total),c("STATE","Literacy.rate.total")]
ggmap(map)+
  geom_polygon(data=final_merge,
  aes(x=long,y=lat,group=id,fill=factor(Literacy.rate.total)),alpha=0.4)+
  ggtitle("Census 2011 - Heat map of Literacy Rate") +
  scale_fill_manual( values = pal3(35), breaks= breaks )+
  labs(fill="Heat map of Literacy Rate ")+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_line(colour = "grey"))

rm(q)
rm(breaks)

##POPULATION - heat map
# pal3 <- colorRampPalette(c("green", "blue" ,"red"),alpha=0.9)
# breaks <- c(quantile(final_merge$POPULATION))
# # Plot the map with ggplot2
# library(ggplot2)
# 
# q <- ggplot(data = final_merge, aes(x = long, y = lat, group = id)) 
# q <- q + geom_polygon(color = "lightgrey", aes(fill = factor(POPULATION)))
# q <- q + ggtitle("Population by Indian States") 
# q <- q + scale_fill_manual( values = pal3(35), breaks= breaks ) 
# q <- q + labs(fill="POPULATION")
# q <- q + theme(panel.background = element_rect(fill = "white", colour = "grey"),
#                panel.grid.major = element_line(colour = "grey"))  
#   
# q

##Population by States
library(ggmap)
library(ggplot2)
map<-get_map("india",maptype="terrain",zoom=5)

pal3 <- colorRampPalette(c("green", "blue" ,"red"))
breaks <- c(quantile(final_merge$POPULATION))

ggmap(map)+
  geom_polygon(data=final_merge,
               aes(x=long,y=lat,group=id,fill=factor(POPULATION)),alpha=0.4)+
  ggtitle("Census 2011 - Heat map - Population") +
  scale_fill_manual( values = pal3(35), breaks= breaks )+
  labs(fill="POPULATION")+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_line(colour = "grey"))


names(final_merge)
##Decal Growth in Population
library(ggmap)
library(ggplot2)
map<-get_map("india",maptype="terrain",zoom=5)

pal3 <- colorRampPalette(c("green", "blue" ,"red"))
breaks <- c(quantile(final_merge$X.Decadal.Growth.2001.to.2011.))

ggmap(map)+
  geom_polygon(data=final_merge,
               aes(x=long,y=lat,group=id,fill=factor(X.Decadal.Growth.2001.to.2011.)),alpha=0.4)+
  ggtitle("Census 2011 - Heat map - Decal Growth Rate of Population from 2001 to 2011") +
  scale_fill_manual( values = pal3(35), breaks= breaks )+
  labs(fill="Decadal Growth Rate")+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_line(colour = "grey"))

#Sex ratio
pal3 <- colorRampPalette(c("red", "blue" ,"green"))
breaks <- c(quantile(final_merge$sex.ratio))

ggmap(map)+
  geom_polygon(data=final_merge,
               aes(x=long,y=lat,group=id,fill=factor(sex.ratio)),alpha=0.4)+
  ggtitle("Census 2011 - Heat map - Sex Ratio") +
  scale_fill_manual( values = pal3(35), breaks= breaks )+
  labs(fill="Sex Ratio")+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        panel.grid.major = element_line(colour = "grey"))

##Plotting of Religion data - done in Excel
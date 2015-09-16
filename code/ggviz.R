

##--Summary--##

##- In this code, the following have been done ##
# 1)Reading the census data 
# 2)Experimenting with package googleVis to plot interactive charts of the data and gain some useful insights

#Datasets used
# 1)data1.csv
# 2)lr.csv
# 3)religion.csv
# 4)rsg.csv

##----------------------------------Census Data of India------------------------------------------##

## Example 1 Facetted Scatterplot

##Read the data
setwd("D:\\world bank data")
data<-read.csv("D:\\world bank data\\data1.csv")
data<-data[-1,-1]
str(data)
unique(as.character(data$STATE))

##--------------Package Used------------------#

library(googleVis)
demo(googleVis)
#Source : #https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis.pdf

##Bubble chart 
##Looking at the relationship between population and Total Literacy Rate by Each state

Bubble <- gvisBubbleChart(data, idvar="STATE", 
                          xvar="POPULATION", yvar="Literacy.rate.total",
                          colorvar="STATE", sizevar="POPULATION",
                          options=list(
                            explorer="{actions: ['dragToZoom', 
                 'rightClickToReset'],
                 maxZoomIn:0.5}",
                            chartArea="{width:'75%',height:'60%'}",
                            title="POPULATION Vs Literacy Rate",
                            width=800, height=600,
                            legend="none"))
plot(Bubble)

##Observation : 
#1)states though have less population and very high literacy Rate :Kerala and Delhi
#1)states though have high population and very low literacy Rate : Uttar Pradesh and Bihar


##Analyse decadal growth
data_sub1<-data[,c(1,11,12)]
Line <- gvisLineChart(data_sub1)
plot(Line)

data_pop<-data[,c(1,3,4)]

#MALES VS FEMALES POPULATION
library(googleVis)

object1<-gvisBarChart(data_pop,options=list(title='Population by Gender and State',width=700, height=700
                                              , tooltip = "{text:'percentage'}"))
object2<-gvisBarChart(data[,c(1,2)],options=list(title='Total Population distribution by State',width=700, height=700
                                              , tooltip = "{text:'percentage'}"))
p <- gvisMerge(object1, object2,
               horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"")
plot(p)

#Studying Literacy Rate
#Literacy Rate by year and Gender
lr<-read.csv("lr.csv")
names(lr)
lr$Census.Year<-as.factor(lr$Census.Year)
str(lr)

plot(
  gvisColumnChart(lr[,c(2,4,5)], 
                  options=list(
                    explorer="{actions: ['dragToZoom', 
                    'rightClickToReset'],
                    maxZoomIn:0.20}",
                 chartArea="{width:'85%',height:'80%'}",
                 hAxis="{title: 'Year', 
                 titleTextStyle: {color: '#000000'}}",
                 vAxis="{title: 'Literacy Rate by Census Year', 
                 titleTextStyle: {color: '#000000'}}",
                 title="Literacy Rate by Census Year and Gender",
                 width=1000, height=500,
                 legend="none"),
               chartid="ZoomZoom")
)

##2011 Census
###Literacy Rate by Gender and State

plot(
  gvisColumnChart(data_sub1, 
               options=list(
                 explorer="{actions: ['dragToZoom', 
                 'rightClickToReset'],
                 maxZoomIn:0.20}",
                 chartArea="{width:'85%',height:'80%'}",
                 hAxis="{title: 'Literacy Rate by State and Gender', 
                 titleTextStyle: {color: '#000000'}}",
                 vAxis="{title: 'STATE', 
                 titleTextStyle: {color: '#000000'}}",
                 title="Literacy Rate by State and Gender",
                 width=1200, height=500,
                 legend="none"),
               chartid="ZoomZoom"))
  


#RELIGION
#Year Wise comparison of %Religions in India

r<-read.csv("religion.csv")
names(r)
str(r)

plot(
  gvisColumnChart(r, 
                  options=list(
                    explorer="{actions: ['dragToZoom', 
                 'rightClickToReset'],
                 maxZoomIn:0.20}",
                    chartArea="{width:'85%',height:'80%'}",
                    hAxis="{title: 'Population %', 
                 titleTextStyle: {color: '#000000'}}",
                    vAxis="{title: 'Religion', 
                 titleTextStyle: {color: '#000000'}}",
                    title="Population % by Religion",
                    width=1500, height=700,
                    isStacked=TRUE,
                    legend="none"),
                  chartid="ZoomZoom"))
#Pie charts
#Plotting multiple plots on a given page

myplot1<-gvisPieChart(r[,c(1,2)],options=list(title='Population % 1951 Census',width=500, height=500
                                       , tooltip = "{text:'percentage'}"))
myplot2<-gvisPieChart(r[,c(1,3)],options=list(title='Population % 1961 Census',width=500, height=500
                                             , tooltip = "{text:'percentage'}"))
myplot3<-gvisPieChart(r[,c(1,4)],options=list(title='Population % 1971 Census',width=500, height=500
                                              , tooltip = "{text:'percentage'}"))
myplot4<-gvisPieChart(r[,c(1,5)],options=list(title='Population % 1981 Census',width=500, height=500
                                              , tooltip = "{text:'percentage'}"))

myplot5<-gvisPieChart(r[,c(1,6)],options=list(title='Population % 1991 Census',width=500, height=500
                                              , tooltip = "{text:'percentage'}"))

myplot6<-gvisPieChart(r[,c(1,7)],options=list(title='Population % 2001 Census',width=500, height=500
                                              , tooltip = "{text:'percentage'}"))
myplot7<-gvisPieChart(r[,c(1,8)],options=list(title='Population % 2011 Census',width=500, height=500
                                              , tooltip = "{text:'percentage'}"))
plot(myplot7)
plot(myplot)

#Merge row charts

merged_data <- gvisMerge(gvisMerge(myplot1, myplot2),gvisMerge(myplot3, myplot4),horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"") 
plot(merged_data)
merged_data1 <- gvisMerge(myplot5, myplot6,horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"") 
plot(merged_data1)
plot(myplot7)


#To understand relationship between State, population and Religion

Bubble <- gvisBubbleChart(data, idvar="STATE", 
                          xvar="STATE", yvar="POPULATION",
                          colorvar="Religion", sizevar="POPULATION",
                          options=list(
                            explorer="{actions: ['dragToZoom', 
                            'rightClickToReset'],
                            maxZoomIn:0.5}",
                            chartArea="{width:'75%',height:'60%'}",
                            title="POPULATION Vs Literacy Rate",
                            width=800, height=600,
                            legend="none"))
plot(Bubble)

#Religion by state and gender

##---Data Cleaning :The data file changes from rsg.csv to rsg4.csv(multiple levels of cleaning)
rsg<-read.csv("D:\\world bank data\\rsg.csv")
dim(rsg)
head(rsg)
names(rsg)
library(reshape2)
rsg1<-melt(rsg,id.var=c("State","Pop.type"))
write.csv(rsg1,"D:\\world bank data\\rsg1.csv",row.names=F)

rsg2<-read.csv("rsg2.csv")
names(rsg2)

check<-which(rsg2$state=="INDIA" | rsg2$pop_type=="Total")
length(check)
rsg3<-rsg2[-check,]

dim(rsg3)
names(rsg3)
unique(rsg3$state)
unique(rsg3$pop_type)
unique(rsg3$Religion)
unique(rsg3$gender)


check1<-which(rsg3$gender=="all")
length(check1)
rsg4<-rsg3[-check,]
dim(rsg4) #final data
rsg3[rsg3$gender=="all",]

##ggplots

write.csv(rsg4,"rsg4.csv",row.names=F)

final_merge<-read.csv("final_merge.csv")
names(final_merge)
unique(final_merge$STATE.x)

rsg4$IND<-as.numeric(as.factor(rsg4$state))
summary(rsg4$IND)

write.csv(rsg4,"rsg4.csv",row.names=F)
final_merge_next<-merge(final_merge,rsg4,by="IND",all.x=TRUE)
dim(final_merge_next)
dim(final_merge)

#Analyse rsg4
rsg4<-read.csv("D:\\world bank data\\rsg4.csv")
names(rsg4)

dim(rsg4)
names(rsg4)
p<-ggplot(rsg4,aes(x=state,y=population))
p+geom_barplot()

summary<-aggregate(population~state + gender + Religion,data=rsg4,sum)
summary
summary<-aggregate(population~state + Religion,data=rsg4,sum)
summary

#Population by State and Religion
class(summary)
names(summary)
p<-gvisBarChart(summary,options=list(title='Population by Religion and State',width=700, height=700
                                            , tooltip = "{text:'percentage'}"))
plot(p)
plot(
  gvisColumnChart(summary[,1:3], 
                  options=list(
                    explorer="{actions: ['dragToZoom', 
                    'rightClickToReset'],
                    maxZoomIn:0.20}",
                    chartArea="{width:'85%',height:'80%'}",
                    hAxis="{title: 'State', 
                    titleTextStyle: {color: '#000000'}}",
                    vAxis="{title: 'Population', 
                    titleTextStyle: {color: '#000000'}}",
                    title="Population by State and Religion",
                    width=1000, height=500,
                    legend="none"),
                  chartid="ZoomZoom")
)


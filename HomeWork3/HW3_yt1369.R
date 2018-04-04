#HW3 Computational Question
#1.draw the temporal distribution of flow, average speed, 
#and average occupancy for each of the two consecutive sensor stations 
#and comment on the results. Please generate plots for only 
#temporal distributions not the flow-speed-density relationship plots. (25 PTS)

# Before read raw data:
# You can refer to the website above to check the definition of each column
# col_1=Timestamp; col_2=Station; col_3: District; col_4=Freeway #; col_5=Directon (NSEW)
# col_6=Lane Type; col_7=Station Length; col_8=Samples; col_9=% Observed; col_10=Total Flow
# col11=Avg Occupancy; col_12=Ave Speed; 
# the remaining columns show individual info (Lane N Samples, Flow, Avg Occ, Avg Speed,Obverved(=1observed; 0=imputed))

#set working directory
setwd('/Users/yixuantang/Desktop/18Spring/Mobility/HW3')

#read file
Filename1 <- "d04_text_station_5min_2018_01_05.txt"

PeMS1 <- read.delim(gzfile(Filename1), sep="," ,header=FALSE)
dim(PeMS1)
PeMS1[1,]

# Selection a loop detector station with a given ID, here SelectedID=403430 (lanes=3)
SelectedID <- 401513   
StationData <- subset(PeMS1,PeMS1 [,2]==SelectedID)
StationData[1,] #check the subset data
dim(StationData)

# Consecutive sensor stations 
SelectedID <- 401512   
StationData1 <- subset(PeMS1,PeMS1 [,2]==SelectedID)
StationData1[1,] #check the subset data
dim(StationData1)

#second sensor station
SelectedID2 <- 403430 
StationData2 <- subset(PeMS1, PeMS1[,2] == SelectedID2)
StationData2[1,]
dim(StationData2)

dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3)) #2row, 3plot each row

TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename1)*24*60/TimeInterval
#draw the plot
plot(ts(StationData[,1]),StationData[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Time vs. Flow",col="blue")
plot(ts(StationData[,1]),StationData[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Time vs. Average Speed",col="blue")
plot(ts(StationData[,1]),StationData[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Time vs. Average Occupancy",col="blue")
#station1
plot(ts(StationData1[,1]),StationData1[,10],type='l',xlim=c(LowTime,UpTime),
     ylim=c(0,max(StationData1[,10]*1.1)),xlab="Timestamp(Interval=5min)",
            ylab="Flow(veh/5min)",main='Time vs Flow',col="Red")
plot(ts(StationData1[,1]),StationData1[,12],type="l",xlim=c(LowTime,UpTime),
     ylim=c(0,100),xlab="Timestamp(Interval=5min)",ylab="Average Speed(mph)",
     main="Time va Average Speed",col="red")
plot(ts(StationData1[,1]),StationData1[,11]*100,type="l",xlim=c(LowTime,UpTime),
     ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",
     main="Time vs. Average Occupancy",col="red")

#problem2
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
#Assume that capacity is 600 vehicles for each lane, 
#the number of lanes 4, and the normal occupancy is 5%.
lanes=4
obliqueQ <- 600*lanes/12*(1:288) #288timestamp
plot(ts(StationData[,1]),cumsum(StationData[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData[,1]),cumsum(StationData[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData[,10])-obliqueQ)*1.1,max(cumsum(StationData[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

# Assume normal occupancy is 5%
obliqueO <- 0.05*(1:288)*100
plot(ts(StationData[,1]),cumsum(StationData[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData[,1]),cumsum(StationData[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData[,11])*100-obliqueO)*1.1,max(cumsum(StationData[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")

#station2
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
plot(ts(StationData2[,1]),cumsum(StationData2[,10]),type="l",
     xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData2[,10]))*1.5),
     xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",
     main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData2[,1]),cumsum(StationData2[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData2[,10])-obliqueQ)*1.1,max(cumsum(StationData2[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

plot(ts(StationData2[,1]),cumsum(StationData2[,11])*100,type="l",
     xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData2[,11]))*100*1.5),
     xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData2[,1]),cumsum(StationData2[,11])*100-obliqueO,type="l",
     xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData2[,11])*100-obliqueO)*1.1,max(cumsum(StationData2[,11])*100-obliqueO)*1.1),
     xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")



f <- function(x)(2.9*sqrt(x) - x**2)

#a. To at least 3 post-decimal digits of accuracy,
#what is the true integral value when a = 0, b1? a = 0, b= 2? (5 Pts)
a <- 0
b1 <- 1
b2 <- 2
integrate(f, a, b1) # interval a to b1
integrate(f, a, b2) # interval a to b2

#b. Using n = 100, 1000, and 10,000, estimate (via MC) the integral for the 
#two combinations of a and b in part (a). Please keep in mind that the area of the plot is different than 1. (10 Pts)
n1 <- 100
n2 <- 1000
n3 <- 10000

#1) b = 1 
set.seed(1) #set the random seed in order to get the same result each time
# n = 100
x <- runif(n1, min = a, max = b1) #generate 100 random values from a to b1 from uniform distribution
I = sum(f(x)) / n1 * (b1 - a) #monte carlo integration
I
# n = 1000
x <- runif(n2, min = a, max = b1)
I = sum(f(x)) / n2 * (b1 - a)
I
# n = 1000
x <- runif(n3, min = a, max = b1)
I = sum(f(x)) / n3 * (b1 - a)
I
#2) b = 2
#n = 100
x <- runif(n1, min = a, max = b2)
I = sum(f(x)) / n1 * (b2 - a)
I
#n = 1000
x <- runif(n2, min = a, max = b2)
I = sum(f(x)) / n2 * (b2 - a)
I
#n = 10000
x <- runif(n3, min = a, max = b2)
I = sum(f(x)) / n3 * (b2 - a)
I

#P2
getwd()
#setwd("/Users/yixuantang/Desktop/18Spring/Mobility/hw4")
library(ggplot2)
library(reshape2)
# Reference letters
ref = readLines('47429-0.txt') # my book: Windfalls by A. G. Gardiner
ref = toupper(ref)

# Create a transition Matrix
transitionMatrix=matrix(0,27,27)
rownames(transitionMatrix)=colnames(transitionMatrix)=c(toupper(letters),"")
lastletter=""

# Check if this can print the lines
for (ln in 1:3) {
  print(ref[ln])
}

# The check is successful, now move on to reading lines
# Loop through each line, within each line loop through each character
for (ln in 1:length(ref)) {
  # Print line number every 1k
  if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
  for (pos in 1:nchar(ref[ln])) { #nchar: count the number of characters 
    curletter=substring(ref[ln],pos,pos)
    if (curletter %in% toupper(letters)) {
      transitionMatrix[rownames(transitionMatrix)==lastletter,
                       colnames(transitionMatrix)==curletter]=
        transitionMatrix[rownames(transitionMatrix)==lastletter,
                         colnames(transitionMatrix)==curletter]+1
      lastletter=curletter
    } else {
      if (lastletter!="") {
        transitionMatrix[rownames(transitionMatrix)==lastletter,27]=
          transitionMatrix[rownames(transitionMatrix)==lastletter,27]+1
        lastletter=""
      }
    }
  }
  curletter=""
  if (lastletter!="") {
    transitionMatrix[rownames(transitionMatrix)==lastletter,27]=
      transitionMatrix[rownames(transitionMatrix)==lastletter,27]+1
  }
  lastletter=""
}

transitionProb=sweep(transitionMatrix+1,1,rowSums(transitionMatrix+1),FUN="/")

# Let's plot this transition probability matrix 
ggplot(melt(transitionProb),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low="white",high="black",limits=c(0,1))+
  labs(x="Probability of Second Letter",y="Conditioning on First Letter",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(transitionProb)$Var1)))+
  coord_equal()
# b.decode 
# Decoding function
decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}

# Log probability function
log.prob <- function(mapping,decoded) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(transitionProb[rownames(transitionMatrix)==lastletter,
                                         colnames(transitionMatrix)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob+log(transitionProb[rownames(transitionMatrix)==lastletter,27])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(transitionProb[rownames(transitionMatrix)==lastletter,27])
    lastletter=""
  }
  logprob
}


codedQuote="RQG LWY C AGG CR CF YOP WXG BOCJB RO EPCDM W RCVG VWNQCJG CJRO W NWX
LQY JOR MO CR LCRQ AOVG ARYDG"

#set.seed(313435) #origin
set.seed(313230) #reset seed

# MCM procedure with 2000 iterations
mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=2000
cur.decode=decode(mapping,codedQuote)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
  
  prop.decode=decode(prop.mapping,codedQuote)
  prop.loglike=log.prob(prop.mapping,prop.decode)
  
  if (runif(1)<exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    if (i %% 100 ==0) {cat(i,cur.decode,"\n")}
    i=i+1
  }
}

#double the jump probability and re-run the MCMC process. 

set.seed(313230) #reset seed
# MCM procedure with 2000 iterations
mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=10000
cur.decode=decode(mapping,codedQuote)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
  
  prop.decode=decode(prop.mapping,codedQuote)
  prop.loglike=log.prob(prop.mapping,prop.decode)
  
  if (runif(1)<exp(prop.loglike-cur.loglike)*2) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    if (i %% 100 ==0) {cat(i,cur.decode,"\n")}
    i=i+1
  }
}


# Q3
######################################
## Conservation Law(Actual Dataset) ##
######################################

Filename<-"d04_text_station_5min_2013_01_05.txt"
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]
TimeInterval<-5
LowTime<-0
UpTime<-length(Filename)*24*60/TimeInterval

#Selection a loop detector station with a given ID, for instance SelectedID=402278
#402828
TrafficCount<-function(upstreamSensorID, downsteramSensorID){
  SelectedID<-upstreamSensorID
  StationData<-subset(PeMS,PeMS[,2]==SelectedID)
  q<-StationData[,10]
  return (q)
}


sensor1 = TrafficCount(401529)  # Upstream Sensor
sensor2 = TrafficCount(401613)  # Downstream Sensor
sensor3 <- TrafficCount(401613)
sensor4 <- TrafficCount(400536)
# Simulated downstream departure flow
set.seed(10009)
SimulatedDeparture<-(sensor2+sample(1:as.integer(sd(sensor2)),288,replace=T)* sample(c(-1,1),288,replace=T))
SimulatedDeparture[which(SimulatedDeparture<0)]=0

N0=500
dT= 5 # minutes
dX= 1 # mile
k0=N0/dX
simulation<-function(inFlow, outFlow){
  for (i in 1:65){ #time step = 65
    plot(NA,NA,xlim=c(0,140),ylim=c(0,100),main="Verifying Conservation Law Using Sensor Data",xlab="Road Section (X)",ylab="",axes = FALSE,frame.plot = TRUE)
    q1=inFlow[i] # veh/minute
    q2=outFlow[i] # veh/minute
    ki = (N0+q1-q2)/dX
    N0=(N0+q1-q2)
    text(40, 90, paste("Time = ",i,sep=""), pos =4,col=2)
    text(40, 80, paste("N = ",N0,sep=""), pos =4,col=4)
    text(-3, 10, paste("q1 = ",q1,sep=""), pos =4,col=2)
    text(120, 10, paste("q2 = ",q2,sep=""), pos =4,col=2)
    text(80, 90, paste("Ki-1 = ",k0,sep=""), pos =4,col=4)
    text(80, 80, paste("Ki = ",ki,sep=""), pos =4,col=4)
    text(40,70,paste("Delta_q = ", q2-q1), pos =4,col=4)
    text(80,70,paste("Delta_k = ", ki-k0), pos =4,col=4)
    rect(xleft=20, ybottom=20, xright=120, ytop=60, density = NULL, angle = 45, col = "gray", border = 3,  lwd = 1)
    color ="yellow"
    if(120-ki/5000*120>120){
      color="red"
    }
    rect(xleft=120-ki/5000*120, ybottom=20, xright=120, ytop=60, density = NULL, angle = 45, col =color, border = 3,  lwd = 1)
    k0 = ki
    Sys.sleep(0.3)
  }  
}

# Simulate with actual arrival and departure flows
simulation(sensor1, sensor2)

#Repeat above step but use sensor 401613 as input and sensor 400536 as the output, 
#check the results at time step = 65 (Note: The section length is 0.59 miles now. 
simulation(sensor3, sensor4)



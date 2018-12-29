library(mvtnorm)

#Let's make normal observational data

nObs <- 500 # Sample Size

pm <- 0.3# Proportion Missing

sigma <- matrix(
  c(1.0,0.5,0.0,0.5,1.0,0.3,0.0,0.3,1.0),
  ncol=3)

simDat <- as.data.frame(
            rmvnorm(nObs,c(0,0,0),
            sigma)
            )

colnames(simDat)<-c("y","x","z")

x<-simDat$x
y<-simDat$y
z<-simDat$z

#MCAR Missingness

rVec1 <- as.logical(rbinom(nObs,size=1,prob= pm))
y2<-y
y2[rVec1]<-NA

simDat$y2 <- y2

#MAR Missingness

rVec2<-pnorm(x,mean=mean(x),sd=sd(x))<pm
y3<-y
y3[rVec2]<-NA

simDat$y3 <- y3

#MNAR Missingness, Indirect

rVec3<-pnorm(y,mean=mean(y),sd=sd(y))<pm
y4<-y
y4[rVec3]<-NA

simDat$y4 <- y4

#MNAR Missingness, Direct

rVec3<-pnorm(z,mean=mean(z),sd=sd(z))<pm
y5<-y
y5[rVec3]<-NA

simDat$y5 <- y5

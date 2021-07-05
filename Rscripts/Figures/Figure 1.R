#######################################################################################################
###     PART 1: SIMULATE 1.000.000 SETS OF  TWO-INDEPENDANT SAMPLES FOR DIFFERENT SD-RATIO'S        ###                         
###             + RUNNING LEVENE's TEST CENTERED AROUND THE MEAN                                    ###
###               AND LEVENE's TEST CENTERED AROUND THE MEDIAN                                      ###
#######################################################################################################

for (package in "car") {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

vector=seq(5,80,5)  # vector sample sizes

setwd(dir="C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 1/Stored files")

########## when sd2=1.84 & sd1=0.92 (SDR = 2)

SS_simu_SDR2=function(nb) {

	simulation=function(sd1){

	nSims <- 1000000 #number of simulated experiments (large numbers might take a while)
	#create variables for dataframe
	catx<-rep("x",nb)
	caty<-rep("y",nb)
	condition<- c(catx,caty)

	pvalueLevene_median<-numeric(nSims) #set up empty container for all Levene's t-test p-values
	pvalueLevene_mean<-numeric(nSims) #set up empty container for all Levene's t-test p-values

	#run simulations

	for(i in 1:nSims){ #for each simulated experiment
	sim_x<-rnorm(nb, sd = sd1) #simulate participants condition x, as a default option, mean = 0
  	sim_y<-rnorm(nb, sd = 1.84) #simulate participants condition y, as a default option, mean = 0
 
  	#create dataframe for levene's test
  	xy<- c(sim_x,sim_y)
  	alldata<-data.frame(xy,condition)
  	#perform Levene's test
  	pvalueLevene_median[i]<-leveneTest(alldata$xy ~ alldata$condition, data = alldata,center="median")$"Pr(>F)"[1:1]
 	pvalueLevene_mean[i]<-leveneTest(alldata$xy ~ alldata$condition, data = alldata,center="mean")$"Pr(>F)"[1:1]
	}

	observedpowerLevene_median<-sum(pvalueLevene_median < 0.05)/nSims*100 
	observedpowerLevene_mean<-sum(pvalueLevene_mean < 0.05)/nSims*100

	return(cbind(nb,observedpowerLevene_median,observedpowerLevene_mean))
	
	}

	simulation(0.92)

}


power_SDR2 <- t(sapply(vector,SS_simu_SDR2))
write.table(power_SDR2,"levene_power when sd2=1.84&sd1=0.92.txt",sep=";",dec=".")


########## when sd2=1.84 & sd1=1.23 (SDR = 1.5)

SS_simu_SDR1.5=function(nb) {

	simulation=function(sd1){

	nSims <- 1000000 #number of simulated experiments (large numbers might take a while)
	#create variables for dataframe
	catx<-rep("x",nb)
	caty<-rep("y",nb)
	condition<- c(catx,caty)

	pvalueLevene_median<-numeric(nSims) #set up empty container for all Levene's t-test p-values
	pvalueLevene_mean<-numeric(nSims) #set up empty container for all Levene's t-test p-values

	#run simulations

	for(i in 1:nSims){ #for each simulated experiment
	sim_x<-rnorm(nb, sd = sd1) #simulate participants condition x, as a default option, mean = 0
  	sim_y<-rnorm(nb, sd = 1.84) #simulate participants condition y, as a default option, mean = 0
 
  	#create dataframe for levene's test
  	xy<- c(sim_x,sim_y)
  	alldata<-data.frame(xy,condition)
  	#perform Levene's test
  	pvalueLevene_median[i]<-leveneTest(alldata$xy ~ alldata$condition, data = alldata,center="median")$"Pr(>F)"[1:1]
 	pvalueLevene_mean[i]<-leveneTest(alldata$xy ~ alldata$condition, data = alldata,center="mean")$"Pr(>F)"[1:1]
	}

	observedpowerLevene_median<-sum(pvalueLevene_median < 0.05)/nSims*100 
	observedpowerLevene_mean<-sum(pvalueLevene_mean < 0.05)/nSims*100

	return(cbind(nb,observedpowerLevene_median,observedpowerLevene_mean))
	
	}

	simulation(1.23)

}

                                                                                                                                         
power_SDR1.5 <- t(sapply(vector,SS_simu_SDR1.5))
write.table(power_SDR1.5,"levene_power when sd2=1.84&sd1=1.23.txt",sep=";",dec=".")


########## when sd2=1.84 & sd1=1.63 (SDR = 1.1)

SS_simu_SDR1.1=function(nb) {

	simulation=function(sd1){

	nSims <- 1000000 #number of simulated experiments (large numbers might take a while)
	#create variables for dataframe
	catx<-rep("x",nb)
	caty<-rep("y",nb)
	condition<- c(catx,caty)

	pvalueLevene_median<-numeric(nSims) #set up empty container for all Levene's t-test p-values
	pvalueLevene_mean<-numeric(nSims) #set up empty container for all Levene's t-test p-values

	#run simulations

	for(i in 1:nSims){ #for each simulated experiment
	sim_x<-rnorm(nb, sd = sd1) #simulate participants condition x, as a default option, mean = 0
  	sim_y<-rnorm(nb, sd = 1.84) #simulate participants condition y, as a default option, mean = 0
 
  	#create dataframe for levene's test
  	xy<- c(sim_x,sim_y)
  	alldata<-data.frame(xy,condition)
  	#perform Levene's test
  	pvalueLevene_median[i]<-leveneTest(alldata$xy ~ alldata$condition, data = alldata,center="median")$"Pr(>F)"[1:1]
 	pvalueLevene_mean[i]<-leveneTest(alldata$xy ~ alldata$condition, data = alldata,center="mean")$"Pr(>F)"[1:1]
	}

	observedpowerLevene_median<-sum(pvalueLevene_median < 0.05)/nSims*100 
	observedpowerLevene_mean<-sum(pvalueLevene_mean < 0.05)/nSims*100

	return(cbind(nb,observedpowerLevene_median,observedpowerLevene_mean))
	
	}

	simulation(1.63)

}


power_SDR1.1 <- t(sapply(vector,SS_simu_SDR2))
write.table(power_SDR1.1,"levene_power when sd2=1.84&sd1=1.63.txt",sep=";",dec=".")

#######################################################################################################
###                                     PART 2: PLOTTING RESULTS                                    ###                         
#######################################################################################################

########## Read files from PART 1

########## 
power_levene_SDR2 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 1/Stored files/levene_power when sd2=1.84&sd1=0.92.txt",header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

power_levene_SDR1.5 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 1/Stored files/levene_power when sd2=1.84&sd1=1.23.txt",header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

power_levene_SDR1.1 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 1/Stored files/levene_power when sd2=1.84&sd1=1.63.txt",header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)

########## PLOTTING DATA

setwd(dir="C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 1/")

png("Figure1.png",width=3000,height=1750, res = 300)

par(xpd=NA, mar=c(5,5,12,5))

plot(power_levene_SDR1.5[,1],power_levene_SDR1.5[,2],type="o",pch=15,cex.axis=1.3,cex.lab=1.5,xlab="sample sizes per condition",ylim=c(0,100),ylab="power",,xlim=c(0,80), cex=1.5,lty=3)
lines(power_levene_SDR1.5[,1],power_levene_SDR1.5[,3],pch=15,type="o",lty=1, cex=1.5)

lines(power_levene_SDR2[,1],power_levene_SDR2[,2],pch=12,type="o",lty=3,cex=1.5) 
lines(power_levene_SDR2[,1],power_levene_SDR2[,3],pch=12,type="o",lty=1,cex=1.5)

lines(power_levene_SDR1.1[,1],power_levene_SDR1.1[,2],pch=19,type="o",lty=3,cex=1.5)
lines(power_levene_SDR1.1[,1],power_levene_SDR1.1[,3],pch=19,type="o",lty=1,cex=1.5)

legend(5,140,legend=c("Centered around mean","Centered around median"),
      , lty=c(1,3), cex=1.2,bty = "n")
legend(45,150,legend=c("SDR = 2 ","SDR = 1.5 ","SDR = 1.1"),
      , pch=c(12,15,19), cex=1.2,bty = "n")

segments(x0=-3,y0=0,x1=83.5,y1=0,col="black") # line for power = 0%
segments(x0=-3,y0=80,x1=83.5,y1=80,lty=3, col="red") # ilne for power = 80%
segments(x0=-3,y0=95,x1=83.5,y1=95,lty=3, col="red") # line for power = 95%

dev.off()

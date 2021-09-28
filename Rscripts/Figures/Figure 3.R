#######################################################################################
####               COMPUTING P-VALUE OF STUDENT's AND WELCH's T-TEST               #### 
####                           AND PRINT IT IN .TXT FILE                           ####        
#######################################################################################

#for (package in "car") {
#    if (!require(package, character.only=T, quietly=T)) {
#        install.packages(package)
#        library(package, character.only=T)
#    }
#}

setwd(dir="C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 3/stored files")
           
#simulation=function(n1,n2,sd1,sd2){

#nSims <- 1000000 #number of simulated experiments (large numbers might take a while)
#p1 <-numeric(nSims) #set up empty container for all simulated Student's t-test p-values 
#p2 <-numeric(nSims) #set up empty container for all simulated Welch's t-test p-values
#p3 <-numeric(nSims) #set up empty container for all simulated Yuen's t-test p-values

#run simulations

#for(i in 1:nSims){ #for each simulated experiment
#  sim_x<-rnorm(n = n1, sd = sd1) #simulate participants condition x, as a default option, mean = 0
#  sim_y<-rnorm(n = n2, sd = sd2) #simulate participants condition y, as a default option, mean = 0

  #perform Student, Yuen and Welch t-test
#  p1[i]<-t.test(sim_x,sim_y, alternative = "two.sided", var.equal = TRUE)$p.value #perform the t-test and store p-value
#  p2[i]<-t.test(sim_x,sim_y, alternative = "two.sided", var.equal = FALSE)$p.value #perform the t-test and store p-value
#  p3[i]<-yuen.t.test(sim_x,sim_y, alternative = "two.sided")$p.value #perform the t-test and store p-value
#} 

#pvalues <- cbind(p1,p2,p3)
#write.table(pvalues,paste("welch's, student's and Yuen's p-values when n1=",n1,", n2=",n2,"and SDR =",round(sd2/sd1,1),".txt"),sep=";",dec=",")

#}

#simulation(60,40,1.84,1.84)
#simulation(50,50,0.92,1.84)
#simulation(40,60,0.92,1.84)
#simulation(40,60,1.84,0.92)

# simulation(40,60,1.63,1.84)
# simulation(40,60,1.84,1.63)

#######################################################################################
####                               PLOTTING RESULTS                                #### 
#######################################################################################

##### READ FILES

n1_60_n2_40_SDR_1 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 3/stored files/welch's, student's and Yuen's p-values when n1= 60 , n2= 40 and SDR = 1 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)
n1_50_n2_50_SDR_2 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 3/stored files/welch's, student's and Yuen's p-values when n1= 50 , n2= 50 and SDR = 2 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)
n1_40_n2_60_SDR_2 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 3/stored files/welch's, student's and Yuen's p-values when n1= 40 , n2= 60 and SDR = 2 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)
n1_40_n2_60_SDR_0.5 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 3/stored files/welch's, student's and Yuen's p-values when n1= 40 , n2= 60 and SDR = 0.5 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)

##### GENERATE DOT PLOTS

setwd(dir="C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 3/")

#FIGURE 3A
png("Figure3a.png",width=2000,height=2000, res = 300)
plot(n1_60_n2_40_SDR_1[,1],n1_60_n2_40_SDR_1[,2],ylim=c(0,1),xlim=c(0,1),xlab=substitute(paste(italic('p-'),"values from Student's ",italic('t-'), "test")), ylab=substitute(paste(italic('p-'),"values from Welch's ",italic('t-'), "test")),col="grey63",cex=0.5, cex.axis=1.3,cex.lab=1.3)
abline(h=0.05) #0.05 line for Welch's test
abline(v=0.05) #0.05 line for Student's t 
abline(0,1, col="black", lwd=2) #draw diagonal red line
dev.off()

#FIGURE 3B
png("Figure3b.png",width=2000,height=2000, res = 300)
plot(n1_50_n2_50_SDR_2[,1],n1_50_n2_50_SDR_2[,2],ylim=c(0,1),xlim=c(0,1),xlab=substitute(paste(italic('p-'),"values from Student's ",italic('t-'), "test")), ylab=substitute(paste(italic('p-'),"values from Welch's ",italic('t-'), "test")),col="grey63",cex=0.5, cex.axis=1.3,cex.lab=1.3)
abline(h=0.05) #0.05 line for Welch's test
abline(v=0.05) #0.05 line for Student's t 
abline(0,1, col="black", lwd=2) #draw diagonal red line
dev.off()

#FIGURE 3C
png("Figure3c.png",width=2000,height=2000, res = 300)
plot(n1_40_n2_60_SDR_2[,1],n1_40_n2_60_SDR_2[,2],ylim=c(0,1),xlim=c(0,1),xlab=substitute(paste(italic('p-'),"values from Student's ",italic('t-'), "test")), ylab=substitute(paste(italic('p-'),"values from Welch's ",italic('t-'), "test")),col="grey63",cex=0.5, cex.axis=1.3,cex.lab=1.3)
abline(h=0.05) #0.05 line for Welch's test
abline(v=0.05) #0.05 line for Student's t 
abline(0,1, col="black", lwd=2) #draw diagonal red line
dev.off()

#FIGURE 3D
png("Figure3d.png",width=2000,height=2000, res = 300)
plot(n1_40_n2_60_SDR_0.5[,1],n1_40_n2_60_SDR_0.5[,2],ylim=c(0,1),xlim=c(0,1),xlab=substitute(paste(italic('p-'),"values from Student's ",italic('t-'), "test")), ylab=substitute(paste(italic('p-'),"values from Welch's ",italic('t-'), "test")),col="grey63",cex=0.5, cex.axis=1.3,cex.lab=1.3)
abline(h=0.05) #0.05 line for Welch's test
abline(v=0.05) #0.05 line for Student's t 
abline(0,1, col="black", lwd=2) #draw diagonal red line
dev.off()


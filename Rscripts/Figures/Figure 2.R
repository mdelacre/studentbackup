#######################################################################################
####               COMPUTING P-VALUE OF STUDENT's AND WELCH's T-TEST               #### 
####                           AND PRINT IT IN .TXT FILE                           ####        
#######################################################################################

for (package in c("onewaytests","PairedData")) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

setwd(dir="C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 2/stored files")
          
simulation=function(n1,n2,sd1,sd2){

nSims <- 1000000 #number of simulated experiments (large numbers might take a while)
p1 <-numeric(nSims) #set up empty container for all simulated Student's t-test p-values 
p2 <-numeric(nSims) #set up empty container for all simulated Welch's t-test p-values
p3 <-numeric(nSims) #set up empty container for all simulated Yuen's t-test p-values

#run simulations

for(i in 1:nSims){ #for each simulated experiment
  sim_x<-rnorm(n = n1, sd = sd1) #simulate participants condition x, as a default option, mean = 0
  sim_y<-rnorm(n = n2, sd = sd2) #simulate participants condition y, as a default option, mean = 0

  #perform Student, Yuen and Welch t-test
  p1[i]<-t.test(sim_x,sim_y, alternative = "two.sided", var.equal = TRUE)$p.value #perform the t-test and store p-value
  p2[i]<-t.test(sim_x,sim_y, alternative = "two.sided", var.equal = FALSE)$p.value #perform the t-test and store p-value
  p3[i]<-yuen.t.test(sim_x,sim_y, alternative = "two.sided")$p.value #perform the t-test and store p-value
} 

pvalues <- cbind(p1,p2,p3)
write.table(pvalues,paste("welch's, student's and Yuen's p-values when n1=",n1,", n2=",n2,"and SDR =",round(sd2/sd1,1),".txt"),sep=";",dec=",")

}

simulation(60,40,1.84,1.84)
simulation(50,50,0.92,1.84)
simulation(40,60,0.92,1.84)
simulation(40,60,1.84,0.92)

#######################################################################################
####                               PLOTTING RESULTS                                #### 
#######################################################################################

##### READ FILES

n1_60_n2_40_SDR_1 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 2/Stored files/welch's, student's and Yuen's p-values when n1= 60 , n2= 40 and SDR = 1 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)
n1_50_n2_50_SDR_2 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 2/Stored files/welch's, student's and Yuen's p-values when n1= 50 , n2= 50 and SDR = 2 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)
n1_40_n2_60_SDR_2 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 2/Stored files/welch's, student's and Yuen's p-values when n1= 40 , n2= 60 and SDR = 2 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)
n1_40_n2_60_SDR_0.5 <- read.table("C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 2/Stored files/welch's, student's and Yuen's p-values when n1= 40 , n2= 60 and SDR = 0.5 .txt",header=TRUE, sep=";", dec=",",na.strings="NA",strip.white=TRUE)

##### GENERATE HISTOGRAMS

setwd(dir="C:/Users/Admin/Documents/Github projects/studentbackup/scripts outputs/Figures/Figure 2/")

#FIGURE 2A:now plot the histogram for p-value distributions
png("Figure2A.png",width=2000,height=2000, res = 300)
par(mfrow=c(2,1))
hist(n1_60_n2_40_SDR_1[,1], main=paste("Figure 2a: n1=60   n2=40   SDR=1","\n","\n","Student's t-test"), xlab=("Observed p-value"))
hist(n1_60_n2_40_SDR_1[,2], main=paste("Welch's t-test"),xlab=("Observed p-value"))
dev.off()

#FIGURE 2B: now plot the histogram for p-value distributions
png("Figure2B.png",width=2000,height=2000, res = 300)
par(mfrow=c(2,1))
hist(n1_50_n2_50_SDR_2[,1], main=paste("Figure 2b: n1=n2=50   SDR=2","\n","\n","Student's t-test"), xlab=("Observed p-value"))
hist(n1_50_n2_50_SDR_2[,2], main=paste("Welch's t-test"),xlab=("Observed p-value"))
dev.off()

#FIGURE 2C:now plot the histogram for p-value distributions
png("Figure2C.png",width=2000,height=2000, res = 300)
par(mfrow=c(2,1))
hist(n1_40_n2_60_SDR_2[,1], main=paste("Figure 2c: n1=40   n2=60   SDR=2","\n","\n","Student's t-test"), xlab=("Observed p-value"))
hist(n1_40_n2_60_SDR_2[,2], main=paste("Welch's t-test"),xlab=("Observed p-value"))
dev.off()

#FIGURE 2D: now plot the histogram for p-value distributions
png("Figure2D.png",width=2000,height=2000, res = 300)
par(mfrow=c(2,1))
hist(n1_40_n2_60_SDR_0.5[,1], main=paste("Figure 2d: n1=40   n2=60   SDR=0.5","\n","\n","Student's t-test"), xlab=("Observed p-value"))
hist(n1_40_n2_60_SDR_0.5[,2], main=paste("Welch's t-test"),xlab=("Observed p-value"))
dev.off()

##### Type 1 error rate for each scenario?

#Scenario A:
alpha_student_A <- round(sum(n1_60_n2_40_SDR_1[,1]<.05)/length(n1_60_n2_40_SDR_1[,1]),3)
alpha_welch_A <- round(sum(n1_60_n2_40_SDR_1[,2]<.05)/length(n1_60_n2_40_SDR_1[,2]),3)

#Scenario B:
alpha_student_B <- round(sum(n1_50_n2_50_SDR_2[,1]<.05)/length(n1_50_n2_50_SDR_2[,1]),3)
alpha_welch_B <- round(sum(n1_50_n2_50_SDR_2[,2]<.05)/length(n1_50_n2_50_SDR_2[,2]),3)

#Scenario C:
alpha_student_C <- round(sum(n1_40_n2_60_SDR_2[,1]<.05)/length(n1_40_n2_60_SDR_2[,1]),3)
alpha_welch_C <- round(sum(n1_40_n2_60_SDR_2[,2]<.05)/length(n1_40_n2_60_SDR_2[,2]),3)

#Scenario D:
alpha_student_D <- round(sum(n1_40_n2_60_SDR_0.5[,1]<.05)/length(n1_40_n2_60_SDR_0.5[,1]),3)
alpha_welch_D <- round(sum(n1_40_n2_60_SDR_0.5[,2]<.05)/length(n1_40_n2_60_SDR_0.5[,2]),3)


alpha_student_A
alpha_welch_A

alpha_student_B
alpha_welch_B

alpha_student_C
alpha_welch_C

alpha_student_D
alpha_welch_D

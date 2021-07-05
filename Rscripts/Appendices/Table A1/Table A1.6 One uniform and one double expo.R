##################################################################################################################################
#                                         S1 = UNIF(0,2) ; S2 = DOUBLEX(0,2*SDR)                                                 #
##################################################################################################################################

########################################################## ALPHA RISK ############################################################

for (package in c("PairedData","fGarch","smoothmest")) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

setwd(dir="C:/Users/mdela/Dropbox/Welch vs. Students_final Scripts/Final after Review/R Scripts/All the scripts/Table A1.1 to A1.9/Stored files")

SDR <- c(0.01,0.1,10,100)   # ratio = sd2/sd1

nSims=1000000
generation=c(1:nSims)   # to generate pairs of independent samples

###### n=10

	alpha_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=0, SDR*2)#simulate participants condition B, as a default option, me


			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(10,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
            alpha_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,alpha_welch,alpha_student))

	}

	alpha_rate=t(sapply(SDR,alpha_bis))
	write.table(alpha_rate,paste("Alpha extr SDR_balanced_Unif and double expo_n1=n2=",alpha_rate[1],".txt"),sep=";",dec=",")

###### n=20

	alpha_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=0, SDR*2)#simulate participants condition B, as a default option, me

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(20,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
            alpha_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,alpha_welch,alpha_student))

	}

	alpha_rate=t(sapply(SDR,alpha_bis))
	write.table(alpha_rate,paste("Alpha extr SDR_balanced_Unif and double expo_n1=n2=",alpha_rate[1],".txt"),sep=";",dec=",")

  ###### n=30

	alpha_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=0, SDR*2)#simulate participants condition B, as a default option, me

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(30,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
            alpha_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,alpha_welch,alpha_student))

	}

	alpha_rate=t(sapply(SDR,alpha_bis))
	write.table(alpha_rate,paste("Alpha extr SDR_balanced_Unif and double expo_n1=n2=",alpha_rate[1],".txt"),sep=";",dec=",")

###### n=40
	alpha_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=0, SDR*2)#simulate participants condition B, as a default option, me


			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(40,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
            alpha_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,alpha_welch,alpha_student))

	}

	alpha_rate=t(sapply(SDR,alpha_bis))
	write.table(alpha_rate,paste("Alpha extr SDR_balanced_Unif and double expo_n1=n2=",alpha_rate[1],".txt"),sep=";",dec=",")

###### n=50

	alpha_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=0, SDR*2)#simulate participants condition B, as a default option, me

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(50,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
            alpha_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,alpha_welch,alpha_student))

	}

	alpha_rate=t(sapply(SDR,alpha_bis))
	write.table(alpha_rate,paste("Alpha extr SDR_balanced_Unif and double expo_n1=n2=",alpha_rate[1],".txt"),sep=";",dec=",")

################################################################## POWER ############################################################

###### n=10

	power_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=1, SDR*2)#simulate participants condition B, mean = 1


			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(10,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#power

		power_welch <- sum(p_values[,3]<.05)/nSims
            power_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,power_welch,power_student))

	}

	power_rate=t(sapply(SDR,power_bis))
	write.table(power_rate,paste("Power extr SDR_balanced_Unif and doublex_n1=n2=",power_rate[1],".txt"),sep=";",dec=",")

###### n=20

	power_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=1, SDR*2)#simulate participants condition B, mean = 1

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(20,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#power

		power_welch <- sum(p_values[,3]<.05)/nSims
            power_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,power_welch,power_student))

	}

	power_rate=t(sapply(SDR,power_bis))
	write.table(power_rate,paste("Power extr SDR_balanced_Unif and doublex_n1=n2=",power_rate[1],".txt"),sep=";",dec=",")

###### n=30

	power_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=1, SDR*2)#simulate participants condition B, mean = 1

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(30,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#power

		power_welch <- sum(p_values[,3]<.05)/nSims
            power_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,power_welch,power_student))

	}

	power_rate=t(sapply(SDR,power_bis))
	write.table(power_rate,paste("Power extr SDR_balanced_Unif and doublex_n1=n2=",power_rate[1],".txt"),sep=";",dec=",")

###### n=40

	power_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=1, SDR*2)#simulate participants condition B, mean = 1

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(40,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#power

		power_welch <- sum(p_values[,3]<.05)/nSims
            power_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,power_welch,power_student))

	}

	power_rate=t(sapply(SDR,power_bis))
	write.table(power_rate,paste("Power extr SDR_balanced_Unif and doublex_n1=n2=",power_rate[1],".txt"),sep=";",dec=",")

###### n=50

	power_bis=function(SDR){ 

		generate=function(i){
			simulation <- function(n,SDR){
				# i = the ith generation
				# n = sample size of each sample
				# ratio = sd2/sd1

			sample1 <- runif(n,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- rdoublex(n, mu=1, SDR*2)#simulate participants condition B, mean = 1

			# perform Welch and Student
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
			return(c(n,SDR,p_welch, p_student))
			}
			simulation(50,SDR)
		}

		p_values=t(sapply(generation,generate)) # perform the function "generate" for each element of "generation" 
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n= p_values[1]

		#View(results)
		#power

		power_welch <- sum(p_values[,3]<.05)/nSims
            power_student <- sum(p_values[,4]<.05)/nSims
		return(c(n,SDR,power_welch,power_student))

	}

	power_rate=t(sapply(SDR,power_bis))
	write.table(power_rate,paste("Power extr SDR_balanced_Unif and doublex_n1=n2=",power_rate[1],".txt"),sep=";",dec=",")


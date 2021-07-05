#################################################################################
#  PART 1: the specificity of the classical 2-sample t-test and Welch's t-test  #
# to the assumption of equal variance when the normality assumption holds true  #
#################################################################################

library(fGarch)
library(coin)
library(smoothmest)

setwd(dir="C:/Users/mdela/Dropbox/Welch vs. Students_final Scripts/Table A3 power for distributions/With extreme SDR/Uniform")

ratio <- c(0.01,0.1,10,100)   # ratio = sd2/sd1
r <- c(1,2,3,4,5)    # r = n2/n1

nSims=1000000
generation=c(1:nSims)   # to generate pairs of independent samples

###### n=10 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))
			}
			simulation(10,1,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=10 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))
			}
			simulation(10,2,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")


###### n=10 and r=3

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))
			}
			simulation(10,3,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=10 and r=4

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))
			}
			simulation(10,4,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")


###### n=10 and r=5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(10,5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(20,1,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(20,2,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=3

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(20,3,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=4

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(20,4,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

		}
			simulation(20,5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(30,1,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

		}
			simulation(30,2,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=3

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

		}
			simulation(30,3,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=4

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

		}
			simulation(30,4,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- runif(n1,-3.465,3.465) #simulate participants condition A, as a default option, mean = 0
			sample2 <- runif(r*n1,ratio*-3.465,ratio*3.465) #simulate participants condition B, as a default option, mean = 0

			# perform Welch
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			return(c(n1,r,p_welch))

			}
			simulation(30,5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Extr and very extr SDR_Two uniform_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")


#################################################################################
#   PART 1: ALPHA RISK : S1 = CHI(2) ; S2 = Nskewed(2,2*ratio, asymetry=-1.99     #
#################################################################################

for (package in c("PairedData","fGarch","smoothmest")) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

setwd(dir="C:/Users/mdela/Dropbox/Welch vs. Students_final Scripts/Table A3 for different non normal distributions/Middle to big SS/Chi squared and normal skewed")

ratio <- c(0.5,1,1.5,2)   # ratio = sd2/sd1
r <- c(0.5,1,1.5,2)    # r = n2/n1

nSims=1000000
generation=c(1:nSims)   # to generate pairs of independent samples

###### n=10 and r=0.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) # mean = 2 (the same than in the chi square) because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(10,0.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=10 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
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
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")


###### n=10 and r=1.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(10,1.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=10 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
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
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")


###### n=20 and r=0.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate


			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(20,0.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
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
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=1.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(20,1.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=20 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
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
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=0.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(30,0.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
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
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=1.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(30,1.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=30 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
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
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=40 and r=0.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(40,0.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=40 and r=1

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(40,1,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=40 and r=1.5

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(40,1.5,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

###### n=40 and r=2

	alpha_bis=function(ratio){

		generate=function(i){
			simulation <- function(n1,r,ratio){
				# i = the ith generation
				# n1 = sample size of the first sample
				# r = n2/n1
				# ratio = sd2/sd1

			sample1 <- rchisq(n1,df=2,ncp=0) # mean = df = 2; sd = 2
			sample2 <- rsnorm(n1, mean=2, sd=ratio*2,xi=-1.99) #mean = 2 (the same than in the chi square)because we want to test the type 1 error rate

			# perform Student's t-test, Welch's t-test and Yuen's t-test, extracting p-value
			p_welch <- t.test(sample1,sample2,alternative="two.sided",var.equal=FALSE)$p.value
			p_student <- t.test(sample1,sample2,alternative="two.sided",var.equal=TRUE)$p.value
	            p_yuen <- yuen.t.test(sample1,sample2, alternative = "two.sided")$p.value #perform the t-test and store p-value
			return(c(n1,r,p_welch,p_student,p_yuen))
			}
			simulation(40,2,ratio)
		}

		p_values=t(sapply(generation,generate)) # perform the function "simu" for each element of "generation" 
     		      	            # (so do the generation 100 000 times)
						# in R, "sapply" is more efficient than the loop (it's running faster)

		n1= p_values[1]
		r = p_values[1,2]

		#View(results)
		#alpha risk

		alpha_welch <- sum(p_values[,3]<.05)/nSims
		alpha_student <- sum(p_values[,4]<.05)/nSims 
		alpha_yuen <- sum(p_values[,5]<.05)/nSims
		return(c(ratio,n1,r,alpha_welch,alpha_student,alpha_yuen))

	}

	alpha_rate=t(sapply(ratio,alpha_bis))
	write.table(alpha_rate,paste("Chi square and normal skewed_n1=",alpha_rate[1,2],"and r=",alpha_rate[1,3],".txt"),sep=";",dec=",")

  
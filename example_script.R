#Example code demonstrating how to use the parameteric_fucntional_redundancy
#function. This example script analyzes a 10 taxa community and the associated
#trait levels

#define filepath to parametric_functional_redundancy function
source("R/parametric_functional_redundancy.R")


#create a vector of relative abundances using a lognormal community structure (equation 8)
#equation 1
S0<-1
rank<-c(0:(10-1))
a<-0.2
abundance<-S0*exp(-a^2*rank^2)
abundance<-abundance/sum(abundance)


#define fixed trait levels; traits must have values that are non-negative reals
#equation 2
trait<-c(1,3,10,2,0,0,1,7,2,0)

#equation 5-7
alpha<-c(0,0.5,1,2) #choose a vector of alphas
parametric_functional_redundancy(abundance,trait,alpha)




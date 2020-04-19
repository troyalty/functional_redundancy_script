TCE_functional_redundancy<-function(abundance,trait,alpha.range=seq(0,2,by=0.1)){
  
  #Equation 3 and 4
  trait.contribution<-abundance*trait/sum(abundance*trait)
  
  #Equation 6
  even.community<-rep(sum(trait.contribution)/length(trait.contribution),length(trait.contribution))
  n.alpha<-length(alpha.range)
  indx<-1
  
  TCE_fr<-data.frame(TCE.fr=rep(NaN,n.alpha),
                            parametric.diversity=rep(NaN,n.alpha),
                            alpha=rep(NaN,n.alpha))
  
  if (sum(even.community)>0){
    for (alpha in alpha.range){
      if (alpha==1){
        #equation 5
        parametric.diversity<-sum(-1*trait.contribution[trait.contribution>0]*log(trait.contribution[trait.contribution>0]))
        parametric.diversity.even<-sum(-1*even.community*log(even.community))
        #equation 7
        evenness<-parametric.diversity/parametric.diversity.even
      } else {
        #equation 5
        parametric.diversity<-(1-sum(trait.contribution[trait.contribution>0]^alpha))/(alpha-1)
        parametric.diversity.even<-(1-sum(even.community^alpha))/(alpha-1)
        #equation 7
        evenness<-parametric.diversity/parametric.diversity.even
      }
      parametric_fr$alpha[indx]<-alpha
      parametric_fr$parametric.diversity[indx]<-parametric.diversity
      parametric_fr$TCE.fr[indx]<-evenness
      indx<-indx+1
    }
  } else {
    parametric_fr$alpha[indx]<-alpha
    parametric_fr$parametric.diversity[indx]<-NaN
    parametric_fr$TCE.fr[indx]<-NaN
  }
  
  return(TCE_fr)
}

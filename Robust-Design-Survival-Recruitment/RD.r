library(R2jags)

### SIMULATE A JOLLY-SEBER
nprim<- 10
n_occ<-1
phi<- rep(0.8,nprim-1)
p<- rep(0.7,nprim)
f<- 0.3

n<- 100 # initial population size
R<- c(n)
for(i in 2:nprim)
	{
	R<- c(R,rpois(1,n[i-1]*f))
	n<- c(n, rbinom(1,n[i-1],phi)+R[i])	
	}
super_n<- sum(R) # total population
# WHEN DO CRITTERS RECRUIT TO THE POPULATION?
ent<-rep(c(1:length(R)),R)


ch<- Z<- matrix(0,super_n,nprim)
for(i in 1:super_n)
	{
	Z[i,ent[i]]<-1
	indx<- ent[i]+1
	if(indx<=nprim)
		{
		for(j in indx:nprim)
			{
			Z[i,j]<- rbinom(1,1,phi[j-1]*Z[i,j-1])
			}
		}		
	}
# SIMULATE CAPTURE HISTORY
for(i in 1:super_n)
	{
	for(j in 1:nprim)
		{
		ch[i,j]<- rbinom(1,1,p[j]*Z[i,j])
		}
	}
indx_nocap<- which(apply(ch,1,sum)!=0)
ch<- ch[indx_nocap,]


# from BPA
mod<- function()
	{
	# Priors 
	mean.p ~ dunif(0, 1)              
	mean.phi ~ dunif(0, 1)            
	for(t in 1:nprim)
		{
		omega[t] ~ dunif(0, 1)
		} #t

	# SET UP PARAMETER MATRICES
	for (i in 1:nind)
		{
		# SURVIVAL MATRIX [INVIDUALS, OCCASIONS]
		for(t_ in 1:(nprim-1))
			{
			logit(phi[i,t_]) <- mean.phi
			} #t_
		for (t in 1:nprim)
			{
			logit(p[i,t]) <- mean.p
			} #t
		} #i
 
	   
	# SIMULATE DEMOGRAPHIC PROCESSES
	for (i in 1:nind)
		{
		# First occasion
		Z[i,1] ~ dbern(omega[1]) # inclusion probability
		 
		# Subsequent occasions
		for (t in 2:nprim)
			{
			openslots[i,t]<-1-max(Z[i,1:(t-1)])
			mu2[i,t] <- phi[i,t-1] *Z[i,t-1] + 	# SURVIVAL IF PREVIOUSLY ALIVE ELSE 0
				omega[t] * openslots[i,t]  	# PROBABILITY OF RECRUIT IF NOT PREVIOUSLY ALIVE, 0 IF PREVIOSLY ALIVE
			Z[i,t] ~ dbern(mu2[i,t])				# [0,1] GIVEN SURVIVAL OR RECRUIT
			} #t
		} #i
		
	# Likelihood	
	for (i in 1:nind)
		{
		for(j in 1:nprim)
			{			
			mu1[i,j] <- Z[i,j] * p[i,j] 
			y[i,j] ~ dbern(mu1[i,j])		
			} #j
		}
	}		



daug <- 300
ch_aug <- as.matrix(rbind(ch, 
	matrix(0, ncol = dim(ch)[2],nrow = daug)))
Z_ini<- ch
for(i in 1:nrow(Z_ini))
	{
	if(sum(Z_ini[i,])>0)
		{
		indx<- range(which(Z_ini[i,]==1))
		}
	Z_ini[i,indx[1]:indx[2]]<-1 
	}
Z_ini<-	as.matrix(rbind(Z_ini, 
	matrix(0, ncol = dim(Z_ini)[2],nrow = daug)))
inits<- function(t)
	{
	list(mean.p=0,mean.phi=0,omega=runif(ncol(ch_aug)),Z=Z_ini)
	}	
	
dat<- list(nprim=ncol(ch_aug), nind=nrow(ch_aug),y=ch_aug)
params<-c("mean.p","mean.phi","omega")		
	
	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
out	
	
	
	
	
## AS ROBUST DESIGN
library(R2jags)

### SIMULATE A JOLLY-SEBER
nprim<- 10
nsec<- c(3,4,5,3,4,5,4,5,6,3)
phi<- rep(0.8,nprim-1)
p<- rep(0.3,nprim)
f<- 0.3
n<- 100 # initial population size
R<- c(n)
for(i in 2:nprim)
	{
	R<- c(R,rpois(1,n[i-1]*f))
	n<- c(n, rbinom(1,n[i-1],phi)+R[i])	
	}
super_n<- sum(R) # total population
# WHEN DO CRITTERS RECRUIT TO THE POPULATION?
ent<-rep(c(1:length(R)),R)

Z<- matrix(0,super_n,nprim)
for(i in 1:super_n)
	{
	Z[i,ent[i]]<-1
	indx<- ent[i]+1
	if(indx<=nprim)
		{
		for(j in indx:nprim)
			{
			Z[i,j]<- rbinom(1,1,phi[j-1]*Z[i,j-1])
			}
		}		
	}
# SIMULATE CAPTURE HISTORY
## MORE THAN 1 CAPTURE OCCASIONS
ch<- matrix(0,nrow(Z),sum(nsec))

## LINK SECONDARY OCCASION TO PRIMARY OCCASION
primsec<- matrix(cbind(rep(1:nprim,nsec),
	c(1:sum(nsec))), ncol=2)
for(i in 1:super_n)
	{
	for(j in 1:sum(nsec))
		{
		ch[i,j]<- rbinom(1,1,
			p[primsec[j,1]]*Z[i,primsec[j,1]])
		}
	}
## SUBSET OUT FISH THAT ARE NEVER CAPTURED
ch<- ch[which(apply(ch,1,sum)!=0),]



mod<- function()
	{
	# Priors 
	mean.p ~ dnorm(0, 0.37)              
	mean.phi~ dnorm(0, 0.37)            
	for(t in 1:nprim)
		{
		omega[t] ~ dunif(0, 1)
		} #t

	# SET UP PARAMETER MATRICES
	for (i in 1:nind)
		{
		# SURVIVAL MATRIX [INVIDUALS, OCCASIONS]
		for(t_ in 1:(nprim-1))
			{
			logit(phi[i,t_]) <- mean.phi
			} #t_
		for (t in 1:nprim)
			{
			logit(p[i,t]) <- mean.p
			} #t
		} #i
 
	   
	# SIMULATE DEMOGRAPHIC PROCESSES
	for (i in 1:nind)
		{
		# First occasion
		Z[i,1] ~ dbern(omega[1]) # inclusion probability
		 
		# Subsequent occasions
		for (t in 2:nprim)
			{
			openslots[i,t]<-1-max(Z[i,1:(t-1)])
			mu2[i,t] <- phi[i,t-1] *Z[i,t-1] + 	# SURVIVAL IF PREVIOUSLY ALIVE ELSE 0
				omega[t] * openslots[i,t]  	# PROBABILITY OF RECRUIT IF NOT PREVIOUSLY ALIVE, 0 IF PREVIOSLY ALIVE
			Z[i,t] ~ dbern(mu2[i,t])				# [0,1] GIVEN SURVIVAL OR RECRUIT
			} #t
		} #i
		
	# Likelihood	
	for (i in 1:nind)
		{
		for(j in 1:total_sec)
			{			
			mu1[i,j] <- Z[i,primsec[j,1]] * p[i,primsec[j,1]] 
			y[i,j] ~ dbern(mu1[i,j])		
			} #j
		}#i
	}		


daug <- 300
ch_aug <- as.matrix(rbind(ch, 
	matrix(0, ncol = dim(ch)[2],nrow = daug)))
Z_ini<- matrix(0,nrow(ch),nprim)
tmp<-t(sapply(1:nrow(ch),function(x)
	{
	range(which(ch[x,]==1))
	}))
for(i in 1:nrow(ch))
	{
	Z_ini[i,primsec[tmp[i,1],1]:primsec[tmp[i,2],1]]<-1
	}
Z_ini<-	as.matrix(rbind(Z_ini, 
	matrix(0, ncol = dim(Z_ini)[2],nrow = daug)))
inits<- function(t)
	{
	list(mean.p=0,mean.phi=0,omega=runif(nprim),Z=Z_ini)
	}	
	
dat<- list(nprim=nprim, 
	nind=nrow(ch_aug),
	y=ch_aug,
	total_sec= sum(nsec),
	primsec=primsec)
params<-c("mean.p","mean.phi","omega")		
	
	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 150000,	
	n.burnin = 60000, 
	n.thin=2,
	working.directory=getwd())
plogis(unlist(out$BUGSoutput$mean	)[-1])
save(out, file="out-rd.Rdata")

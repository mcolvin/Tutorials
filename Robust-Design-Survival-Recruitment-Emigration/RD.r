library(R2jags)
setwd("C:/Users/mcolvin/Google Drive/Tutorials/Robust-Design-Survival-Recruitment-Emigration")
# ALLOW FOR PERMANT EMIGRATION FROM THE SYSTEM


nprim<- 10
nsec<- c(3,4,5,3,4,5,4,5,6,3)
phi	<- rep(0.8,nprim-1)
p	<- rep(0.3,nprim)
f	<- 0.3
gam	<- 0.3 # EMIGRATION RATE
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
# PERMANANT EMIGRATION
Z_em<- Z
Z_em[]<-0	
p_em<-Z_em	
for(i in 1:nrow(Z_em))
	{
	for(j in 2:(nprim))
		{
		p_em[i,j]<- (1-Z_em[i,j-1])*Z[i,j-1]*gam + Z_em[i,j-1]*Z[i,j-1]
		Z_em[i,j]<- rbinom(1,1,	p_em[i,j]*Z[i,j])
		}
	}
	
# SIMULATE CAPTURE HISTORY
## MORE THAN 1 CAPTURE OCCASIONS
ch<- matrix(0,nrow(Z),sum(nsec))
avail2Capture<-ch
## LINK SECONDARY OCCASION TO PRIMARY OCCASION
primsec<- matrix(cbind(rep(1:nprim,nsec),
	c(1:sum(nsec))), ncol=2)
## CAPTURE HISTORY CONDITIONAL ON BEING PRESENT (I.E., NOT EMIGRATED)
for(i in 1:super_n)
	{
	for(j in 1:sum(nsec))
		{
		primocc<- primsec[j,1]
		avail2Capture[i,j]<- (1-Z_em[i,primsec[j,1]]) * Z[i,primsec[j,1]]
		ch[i,j]<- rbinom(1,1,p[primocc]*avail2Capture[i,j])
		}
	}
	
## SUBSET OUT FISH THAT ARE NEVER CAPTURED
ch<- ch[which(apply(ch,1,sum)!=0),]



mod<- function()
	{
	# Priors 
	mean.p ~ dnorm(0, 0.37)              
	mean.phi~ dnorm(0, 0.37)            
	mean.gam~ dnorm(0, 0.37)            
	for(t in 1:nprim)
		{
		logit(omega[t])<-omega1[t]
		omega1[t] ~ dnorm(0, 0.37)  
		} #t

	# SET UP PARAMETER MATRICES
	for (i in 1:nind)
		{
		# SURVIVAL MATRIX [INVIDUALS, OCCASIONS]
		for(t_ in 1:(nprim-1))
			{
			logit(phi[i,t_]) <- mean.phi
			logit(gam[i,t_]) <- mean.gam
			} #t_
		for (t in 1:nprim)
			{ # SET UP TO HANDLE INVIDUAL AND TEMPORAL EFFECTS
			logit(p[i,t]) <- mean.p
			} #t
		} #i
 
	   
	# SIMULATE DEMOGRAPHIC PROCESSES
	for (i in 1:nind)
		{
		# First occasion
		Z[i,1] ~ dbern(omega[1]) # inclusion probability
		Z_em[i,1]<-0 
		# Subsequent occasions
		for (t in 2:nprim)
			{			
			# SURVIVAL AND RECRUITMENT IN THE SUPER POPULATION
			openslots[i,t]<-1-max(Z[i,1:(t-1)])
			mu2[i,t] <- phi[i,t-1] *Z[i,t-1] + 		# SURVIVAL IF PREVIOUSLY ALIVE ELSE 0
				omega[t] * openslots[i,t]  			# PROBABILITY OF RECRUIT IF NOT PREVIOUSLY ALIVE, 0 IF PREVIOSLY ALIVE
			Z[i,t] ~ dbern(mu2[i,t])				# [0,1] GIVEN SURVIVAL OR RECRUIT

			# EMIGRATION
			mu_em[i,t]<- gam[i,t-1]*(1-Z_em[i,t-1])*Z[i,t-1] + # gam if in system and alive, 0 otherwise
				Z_em[i,t-1]*Z[i,t]#Z[i,t-1] # 1 if alive and already emigrated, 0 otherwise
			
			Z_em[i,t]~dbern(mu_em[i,t])
			
			} #t
		} #i
		
	# Likelihood	
	for (i in 1:nind)
		{
		for(j in 1:total_sec)
			{	
			# capture probability conditional on availability
			mu1[i,j] <-(1-Z_em[j,primsec[j,1]]) * 	# 0 IF UNAVAILIBLE?
				Z[i,primsec[j,1]] * 				# 1 IF ALIVE
				p[i,primsec[j,1]] 					# CAPTURE PROBABILITY
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
	
# INITIAL EMIGRATION MATRIX
Z_em_ini<-Z_ini
Z_em_ini[]<-0		# THIS IS SUCKFUL
Z_em_ini[,1]<-NA 	# EFFIN JAGS CAN'T ASSIGN IN MOD UNLESS NA
inits<- function(t)
	{
	list(mean.p=0,mean.phi=0,omega1=runif(nprim),Z=Z_ini,Z_em=Z_em_ini)
	}	
	
dat<- list(nprim=nprim, 
	nind=nrow(ch_aug),
	y=ch_aug,
	total_sec= sum(nsec),
	primsec=primsec)
params<-c("mean.p","mean.phi","omega","mean.gam")		
	
	
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
	
	
	
plogis(unlist(out$BUGSoutput$mean	)[-1])
save(out, file="out-rd.Rdata")      
load(file="out-rd.Rdata")

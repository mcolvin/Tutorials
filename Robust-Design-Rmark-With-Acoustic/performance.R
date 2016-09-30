

setwd("C:/Users/mcolvin/Google Drive/Tutorials/Robust-Design-Rmark-With-Acoustic")
library(RMark)
source("./sim-rd-function.R")
options(scipen=15)
ppp<-list()
ppp$vals<-data.frame()
ppp$abundance<-data.frame()


nprim<-5
start<- max(ppp$vals$rep)
for(ii in start:150)
	{
	out<- sim_rd(nprim=nprim,
		nsec=rep(8,nprim),
		phi=rep(0.8,nprim-1),
		n_acoustic_tags=50,
		p_cap=rep(0.3,nprim),# CAPTURE PROBABILITY
		gamma_prime=rep(0.2, nprim-1) ,# pr(unobservable @ t-1  --> unobservable @ t )
		gamma_dblprime=rep(0.1, nprim-1) ,# pr(observable @ t-1 --> unobservable  @ t   )
		n=175,# initial population size
		n_unobservable=25)

	rd_acoustic<- process.data(data=out$acoustic$ch, 
		groups="acoustic",model="Robust", 
		time.intervals=out$acoustic$occs) # CLUTCH...
	rd_acoustic_ddl<-make.design.data(rd_acoustic)

	rd_pit<- process.data(data=out$pit$ch, 
		groups="acoustic",model="Robust", 
		time.intervals=out$pit$occs) # CLUTCH...
	rd_pit_ddl<-make.design.data(rd_pit)


	S=list(formula=~1)# SURVIVAL
	## SHARE = TRUE TO SET C = P
	p_fixed1_indx<- c()
	for(i in 1:max(as.numeric(as.character(rd_acoustic_ddl$p$group))))
		{
		p_fixed1_indx<- c(p_fixed1_indx,
			which(rd_acoustic_ddl$p$group==i &  
				#rd_acoustic_ddl$p$session %in% c((i+1):nprim) &
				rd_acoustic_ddl$p$session %in% c((i+1):nprim) &
				rd_acoustic_ddl$p$time==1))
		}

	p_fixed0_indx<- c(which(rd_acoustic_ddl$p$group==0 & 
			rd_acoustic_ddl$p$time==1))
	for(i in 2:max(as.numeric(as.character(rd_acoustic_ddl$p$group))))
		{
		p_fixed0_indx<- c(p_fixed0_indx,
			which(rd_acoustic_ddl$p$group==i &  
				rd_acoustic_ddl$p$session %in% c(1:(i-1)) &
				rd_acoustic_ddl$p$time==1))

		}

	rd_acoustic_ddl$p$verify<-NA
	rd_acoustic_ddl$p$verify[p_fixed1_indx]<-1
	rd_acoustic_ddl$p$verify[p_fixed0_indx]<-0

	p=list(formula=~1,share=TRUE,
		fixed= list(
			index = c(p_fixed1_indx, p_fixed0_indx), 
			value=  c(rep(1,length(p_fixed1_indx)),
				rep(0, length(p_fixed0_indx))
			))) # FIX P TO 1 FOR ACOUSTIC FISH AND 0 FOR PIT TAGS)# CAPTURE PROBABILITY

	f0<- list(formula=~acoustic+session)
	GammaDoublePrime=list(formula=~1)
	GammaPrime=list(formula=~1)
	fit_acoustic<-mark(data = rd_acoustic, 
		model = "Robust", 
		time.intervals=time.intervals,
		model.parameters=list(
			S=S,
			GammaDoublePrime=GammaDoublePrime,
			GammaPrime=GammaPrime,
			p=p,
			f0=f0),
		threads=2,
		brief=FALSE,output=FALSE,invisible=TRUE)
	derived_acoustic<- fit_acoustic$results$derived$`N Population Size`


	p=list(formula=~1,share=TRUE)
	fit_pit<-mark(data = rd_pit, 
		model = "Robust", 
		time.intervals=time.intervals,
		model.parameters=list(
			S=S,
			GammaDoublePrime=GammaDoublePrime,
			GammaPrime=GammaPrime,
			p=p),
		threads=2,
		brief=FALSE,output=FALSE,invisible=TRUE)
	derived_pit<- fit_pit$results$derived$`N Population Size`




	estimates<- matrix(derived_acoustic$estimate,
		nrow=nprim,
		ncol=length(as.numeric(unique(as.character(rd_acoustic_ddl$p$acoustic)))),byrow=FALSE)
	lcl<- matrix(derived_acoustic$lcl,
		nrow=nprim,
		ncol=length(as.numeric(unique(as.character(rd_acoustic_ddl$p$acoustic)))),byrow=FALSE)
		
	ucl<- matrix(derived_acoustic$ucl,
		nrow=nprim,
		ncol=length(as.numeric(unique(as.character(rd_acoustic_ddl$p$acoustic)))),byrow=FALSE)

	lcl<- lcl[,1]+rowSums(estimates[,2:ncol(estimates)])
	ucl<- ucl[,1]+rowSums(estimates[,2:ncol(estimates)])
	estimates<- rowSums(estimates)

	# SAVE OUTPUT
	ppp$vals<-rbind(ppp$vals,data.frame(
		rep=ii,
		parm=c("phi","gamma_dblprime","gamma_prime","p"),
		acoustic=plogis(summary(fit_acoustic)$beta$estimate[1:4]),
		pit=plogis(summary(fit_pit)$beta$estimate[1:4]),
		true=c(out$parameters$phi[1],out$parameters$gamma_dblprime[1],
			out$parameters$gamma_prime[1],out$parameters$p_cap[1]),
			 bias_acoustic=NA, bias_pit=NA))
	ppp$abundance<- rbind(ppp$abundance,
		data.frame(rep=ii,
		occasion=c(1:length(estimates)),
		"pit"=derived_pit$estimate,
		"acoustic"=estimates,
		"true"=out$parameters$true_abundance,
		"pit_se"=derived_pit$se,
		"acoustic_se"=derived_acoustic$se[1:out$parameters$nprim],
		acoustic_cv=NA,pit_cv=NA))
	}

saveRDS(ppp,"sim-output2.RDS")
cleanup(ask=FALSE)









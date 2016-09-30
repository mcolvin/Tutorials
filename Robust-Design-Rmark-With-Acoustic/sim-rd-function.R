sim_rd<- function(
	nprim=5,
	nsec=rep(8,nprim),
	phi=rep(0.8,nprim-1),
	n_acoustic_tags=50,
	p_cap=rep(0.3,nprim),# CAPTURE PROBABILITY
	gamma_prime=rep(0.2, nprim-1) ,# pr(unobservable @ t-1  --> unobservable @ t )
	gamma_dblprime=rep(0.1, nprim-1) ,# pr(observable @ t-1 --> unobservable  @ t   )
	n=175,# initial population size
	n_unobservable=25)# UNOBSERVABLE
	{
	super_n<- n+n_unobservable # SUPER POPULATION (ONSITE + OFFSITE)
	Z<- matrix(0,super_n,nprim)
	Z_observable<- matrix(0,super_n,nprim)
	Z_unobservable<- matrix(0,super_n,nprim)
	Z[1:super_n,1]<-1
	Z_observable[1:n,1]<-1
	Z_unobservable[(n+1):(n+n_unobservable),1]<-1

	# SURVIVAL,, EMIGRATION, AND MIGRATION
	for(i in 1:super_n) # FOR EACH INDIVIDUAL IN THE SUPER POPULATION
		{
		for(j in 2:nprim) # FOR PRIMARY OCCASIONS 2:NPRIM
			{
			# DOES A FISH SURVIVE, IF IT WAS PREVIOUSLY ALIVE?
			Z[i,j]<- rbinom(1,1,phi[j-1])*Z[i,j-1]

			prs<-c(
				# OBSERVABLE[t-1] --> UNOBSERVABLE[t]
				(1-Z_unobservable[i,j-1])*(gamma_dblprime[j-1]),
				
				# OBSERVABLE[t-1] --> OBSERVABLE[t]
				(1-Z_unobservable[i,j-1])*(1-gamma_dblprime[j-1]),
				
				# UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
				Z_unobservable[i,j-1]*gamma_prime[j-1],
				
				# UNOBSERVABLE[t-1]--> 	OBSERVABLE[t]
				Z_unobservable[i,j-1]*(1-gamma_prime[j-1]))

			state<-sample(1:4,1,prs,replace=FALSE)
			if(state==1 & Z[i,j]==1){Z_unobservable[i,j]<- 1}
			if(state==2 & Z[i,j]==1){Z_observable[i,j]<-1}
			if(state==3 & Z[i,j]==1){Z_unobservable[i,j]<-1}
			if(state==4 & Z[i,j]==1){Z_observable[i,j]<-1}
			}
		}
		
	# SIMULATE CAPTURE HISTORY
	## MORE THAN 1 CAPTURE OCCASIONS
	ch<- matrix(0,nrow(Z),sum(nsec))
	avail2Capture<-ch
	## LINK SECONDARY OCCASION TO PRIMARY OCCASION
	primsec<- matrix(cbind(
		rep(1:nprim,nsec),
		c(1:sum(nsec)),
		c(sapply(nsec,function(x) c(1:x))),
		0), ncol=4)
	acoustic_slots<-which(primsec[,3]==1)
	
	## CAPTURE HISTORY CONDITIONAL ON BEING PRESENT (I.E., NOT EMIGRATED)
	for(i in 1:super_n)
		{
		for(j in 1:sum(nsec))
			{
			primocc<- primsec[j,1]
			avail2Capture[i,j]<-Z_observable[i,primsec[j,1]]
			ch[i,j]<- rbinom(1,1,p_cap[primocc]*avail2Capture[i,j])
			}
		}
	ch[,acoustic_slots]<-0 # KEEP THESE SLOTS OPEN...

	## ACOUSTIC TAGS
	## FISH MARKED THAT DATA ARE NOT INCLUDED IN ACOUSTIC CAPTURE HISTORY	
	tmp<-lapply(1:nrow(ch),function(x) 
		{
		xx<-tapply(unlist(ch[x,]),primsec[,1],sum)
		})
	tmp<-do.call("rbind",tmp)
	tmp[tmp>0]<-1

	first_capture<- unlist(sapply(1:nrow(tmp), function(x)
		{# first capture to install acoustic tag
		if(sum(tmp[x,])==0){y<-0}
		if(sum(tmp[x,])>0){y<-min(which(tmp[x,]==1))}# TAGGED ON PREVIOUS PRIMARY OCCASION
		return(y)
		}))

	indx<- c(1:nrow(ch))[which(apply(ch,1,sum)>0)]
	indx<- sample(indx,n_acoustic_tags,replace=FALSE)
	ch_acoustic<- matrix(0,nrow(Z),nprim)
	fish_data<- data.frame(acoustic=rep(0,nrow(Z)))

	## ASSIGN WHEN A FISH WAS TAGGED ACOUSTICALLY
	## THEN FIX P AFTERWARDS TO BE 1
	fish_data$acoustic[indx]<-first_capture[indx]

	## ASSIGN 1 FOR PRIMARY PERIOD AFTER ACOUSTIC TAGGING AS 1
	for(i in indx)
		{
		#ch_acoustic[i,(first_capture[i]+1):nprim]<-1 # available for detection in the next capture occasion
		ch_acoustic[i,(first_capture[i]):nprim]<-1 # available for detection in the next capture occasion
		}

	## ZERO OUT DETECTION IF UNOBSERVABLE
	ch_acoustic<-ch_acoustic*Z_observable

	# COMBINE ACOUSTIC AND PIT TAGS
	for(i in 1:nprim)
		{
		ch[,acoustic_slots[i]]<- ch_acoustic[,i]
		}
	## SUBSET OUT FISH THAT ARE NEVER CAPTURED
	indx<- which(apply(ch,1,sum)!=0)
	ch<- ch[indx,]
	fish_data<-as.data.frame(fish_data[indx,])
	## NEED THIS FOR RMARK
	out<-list()
	out$acoustic$ch_raw<-ch
	out$acoustic$ch<- data.frame(ch=apply(ch,1,paste0,collapse=""),
		freq=1,
		acoustic=as.factor(fish_data[,1]),
		stringsAsFactors=FALSE)# prep data for processing
	ends<-cumsum(nsec) # last sampling occasion
	occs<- rep(0,sum(nsec))
	occs[ends]<-1# last occasion in primary
	out$acoustic$occs<- occs[-length(occs)]# drop last 1 for processing

	## CAPTURE HISTORIES WITH NO ACOUSTIC INFORMATION
	ch<- ch[,-acoustic_slots]
	out$pit$ch_raw<-ch
	out$pit$ch<- data.frame(ch=apply(ch,1,paste0,collapse=""),
		freq=1,
		acoustic=as.factor(0),
		stringsAsFactors=FALSE)
	ends<-cumsum(nsec-1) # remove acoustic sampling last sampling occasion
	occs<- rep(0,sum(nsec-1))
	occs[ends]<-1# last occasion in primary
	out$pit$occs<- occs[-length(occs)]# drop last 1 for processing

	out$parameters<-list(
		nprim=nprim,
		nsec=nsec,
		phi=phi,
		n_acoustic_tags=n_acoustic_tags,
		p_cap=p_cap,
		gamma_prime=gamma_prime ,
		gamma_dblprime=gamma_dblprime ,
		n=n ,
		n_unobservable=n_unobservable,
		true_abundance=colSums(Z_observable))
	return(out)
	}
	

	
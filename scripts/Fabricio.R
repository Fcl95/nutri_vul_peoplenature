### Leitão Rarity


##1)	INPUT	MATRICES

#Trait	matrix	(species	(rows)	X	traits	(columns))


traits <- marine_traits

RI <- marine #for what I want to remove species first, as a rarity index

RI<-RI["MaxLengthTL"]



########################################
########################################
##2)	FUNCTIONAL	SPACE

library(cluster)
library(FD)
#Gower	distance	between	species	(not	all	traits	continous)

#distance<-gowdis(traits)

distance<-daisy(marine[,2:8],	metric="euclidean")

#Euclidean	distance	between	species	(all	traits	continous;	e.g.	Trees)


#PCoA	based	on	Gower/Euclidean	distance	between	species
library(ape)

PCOA<-pcoa(distance,	correction="none")

#Number	of	dimensions	of	the	functional	space
NoD<-4

#OBS:	Adjust	"NoD"	acording	to	the	number	of	dimensions	(PCoA	axes)	for	each taxonimic	group	

Axes<-PCOA$vectors[,1:NoD]



########################################
########################################

##3)	SIMULATIONS	OF	REGIONAL	SPECIES	LOSS

##3.1)	FUNCTIONAL	RICHNESS	(FRic)
library(geometry)

#Input:	PCoA	axes	+	RI

ERS<-cbind(Axes, RI)
NoS<-1280
#OBS:	Adjust	"NoS"	acording	to	the	maximum	possible	number	of	species	in	each community minus 4 or 5 
#OBS:	This	maximum	possible	number	of	species	is	due	to	the	impossibility	to	compute	
#FRic	with	few	species	in	null	models
#Scenario	losing	rarest	species	first
##############Run	for################
Simulat_rare<-ERS[order(ERS[,"MaxLengthTL"]),]
tr_rare<-Simulat_rare[,1:NoD]
FRic_Simulat_rare=matrix(NA,nrow=NoS,ncol=1)
for	(i	in	1:NoS)
{
  tr_rare=tr_rare[-1,]
  FRic_Simulat_rare[i]<-convhulln(tr_rare,"FA")$vol
}
##############End	of	for#############
#Scenario	losing	commonest	species	first
##############Run	for################
Simulat_comm<-ERS[order(ERS[,"MaxLengthTL"],decreasing=T),]
tr_comm<-Simulat_comm[,1:NoD]
FRic_Simulat_comm=matrix(NA,nrow=NoS,ncol=1)
for	(i	in	1:NoS)
{
  tr_comm=tr_comm[-1,]
  FRic_Simulat_comm[i]<-convhulln(tr_comm,"FA")$vol
}
##############End	of	for#############
#Scenario	losing	species	randomly	(null	model)
##############Run	for################
dataperm=ERS
randFRic=matrix(NA,nrow=NoS,ncol=1000)#ncol=	#randomizations
for	(j	in	1:1000)
{
  dataperm[,"MaxLengthTL"]=sample(dataperm[,"MaxLengthTL"])
  Null_Simulat<-dataperm[order(dataperm[,"MaxLengthTL"]),]
  Null_tr<-Null_Simulat[,1:NoD]
  for	(i	in	1:NoS)
  {
    Null_tr=Null_tr[-1,]
    randFRic[i,j]<-convhulln(Null_tr,"FA")$vol
  }			
  
}
##Calculating	median	and	quantiles	from	the	null	models
Null_FRic=	matrix(NA,nrow=NoS,ncol=3)
for	(i	in	1:NoS)
{
  Null_FRic[i,1]=median(randFRic[i,])		
  Null_FRic[i,2]=quantile(randFRic[i,],probs=0.025)
  Null_FRic[i,3]=quantile(randFRic[i,],probs=0.975)		
}
##############End	of	for#############
#Plot	regional	species	loss	scenarios	- FRic
Spp_erosion<-c(1:NoS)
plot(Spp_erosion,Null_FRic[,1]/max(Null_FRic[,1]),type="l",xlim=c(0,NoS),xlab="Regional	Species	loss (%)",ylab="FRic",font.lab=1,cex.lab=1 ,cex.axis=1,col="gray",pch=16,cex=1.5,xaxt="n
")
axis(side=1,	at=c(0,NoS/4,NoS/2,NoS/1.333,NoS),	
     labels=c(0,25,50,75,100),line=F,tick=-0.3,cex.axis=1,mgp=c(3,1,0))
polygon(c(1:NoS,rev(1:NoS)),c(Null_FRic[,2]/max(Null_FRic[,2]),rev(Null_FRic[,3]/max
                                                                   (Null_FRic[,3]))),col="grey88",border=F)
points(Null_FRic[,1]/max(Null_FRic[,1]),col="gray",cex=1.6,type="l",lty=1,lwd=4)
points(FRic_Simulat_rare/max(FRic_Simulat_rare),col="black",cex=1.6,type="l",lwd=4)
points(FRic_Simulat_comm/max(FRic_Simulat_comm),col="black",cex=1.6,type="l",lwd
       =4,lty=3)
########################################




##3.2)	FUNCTIONAL	SPECIALIZATION	(FSpe)
#Computing	functional	specialization	for	each	species	(speS)
O<-apply(Axes,	2,	mean)
speS<-apply(Axes,	1,	function(x)	{	(sum((x-O)^2)	)^0.5}	)
speS<-speS/max(speS)
ERS<-cbind(RI,speS)
#Scenario	losing	rarest	species	first
##############Run	for################
Simulat_rare<-ERS[order(ERS[,"MaxLengthTL"]),]
FSpe_Simulat_rare=matrix(NA,nrow=NoS,ncol=1)
for	(i	in	1:NoS)
{
  Simulat_rare=Simulat_rare[-1,]
  FSpe_Simulat_rare[i]<-mean(Simulat_rare[,"speS"])
}
##############End	of	for#############
#Scenario	losing	commonest	species	first
##############Run	for################
Simulat_comm<-ERS[order(ERS[,"MaxLengthTL"],decreasing=T),]
FSpe_Simulat_comm=matrix(NA,nrow=NoS,ncol=1)

for	(i	in	1:NoS)
{
  Simulat_comm=Simulat_comm[-1,]
  FSpe_Simulat_comm[i]<-mean(Simulat_comm[,"speS"])
}
##############End	of	for#############
#Scenario	losing	species	randomly	(null	model)
##############Run	for################
dataperm=ERS
randSPE=matrix(NA,nrow=NoS,ncol=1000)#ncol=	#randomizations
for	(j	in	1:1000)
{
  dataperm[,1]=sample(dataperm[,1])	
  Null_Simulat_rare<-dataperm[order(dataperm[,"MaxLengthTL"]),]		
  for	(i	in	1:NoS)
  {
    Null_Simulat_rare=Null_Simulat_rare[-1,]
    randSPE[i,j]<-mean(Null_Simulat_rare[,"speS"])
  }
}
##Calculating	median	and	quantiles	from	the	null	models
Null_FSpe=	matrix(NA,nrow=NoS,ncol=3)
for	(i	in	1:NoS)
{
  Null_FSpe[i,1]=median(randSPE[i,])		
  Null_FSpe[i,2]=quantile(randSPE[i,],probs=0.025)
  Null_FSpe[i,3]=quantile(randSPE[i,],probs=0.975)		
}
##############End	of	for#############
#Plot	regional	species	loss	scenarios	- FSpe
Spp_erosion<-c(1:NoS)
plot(Spp_erosion,Null_FSpe[,1],type="l",xlim=c(0,NoS),xlab="Regional	Species	loss	
(%)",ylab="FSpe",font.lab=2,cex.lab=1.4,cex.axis=1.2,col="gray",pch=16,cex=1.5,xaxt="n
",ylim=c(min(Null_FSpe),max(Null_FSpe)))
axis(side=1,	at=c(0,NoS/4,NoS/2,NoS/1.333,NoS),	
     labels=c(0,25,50,75,100),line=F,tick=-0.3,cex.axis=1.2,mgp=c(3,1,0))
polygon(c(1:NoS,rev(1:NoS)),c(Null_FSpe[,2],rev(Null_FSpe[,3])),col="grey88",border=
          F)
points(Null_FSpe[,1],col="gray",cex=1.6,type="l",lty=1,lwd=4)
points(FSpe_Simulat_rare,col="black",type="l",lty=1,lwd=4)
points(FSpe_Simulat_comm,col="black",type="l",lty=3,lwd=4)
########################################



##3.3)	FUNCTIONAL	ORIGINALITY	(FOri)
#Computing	functional	originality	for	each	species	(oriS)
dist_F<-as.matrix(dist(Axes,method="euclidean"))	;	dist_F[which(dist_F==0)]<-NA
oriS<-apply(dist_F,	1,	min,	na.rm=T	)
oriS<-oriS/max(oriS)

ERS<-cbind(RI,oriS)
#Scenario	losing	rarest	species	first
##############Run	for################
Simulat_rare<-ERS[order(ERS[,"MaxLengthTL"]),]
FOri_Simulat_rare=matrix(NA,nrow=NoS,ncol=1)
for	(i	in	1:NoS)
{
  Simulat_rare=Simulat_rare[-1,]
  FOri_Simulat_rare[i]<-mean(Simulat_rare[,"oriS"])
}
##############End	of	for#############
#Scenario	losing	commonest	species	first
##############Run	for################
Simulat_comm<-ERS[order(ERS[,"MaxLengthTL"],decreasing=T),]
FOri_Simulat_comm=matrix(NA,nrow=NoS,ncol=1)
for	(i	in	1:NoS)
{
  Simulat_comm=Simulat_comm[-1,]
  FOri_Simulat_comm[i]<-mean(Simulat_comm[,"oriS"])
}
##############End	of	for#############
#Scenario	losing	species	randomly	(null	model)
##############Run	for################
dataperm=ERS
randORI=matrix(NA,nrow=NoS,ncol=1000)#ncol=	#randomizations
for	(j	in	1:1000)
{
  dataperm[,1]=sample(dataperm[,1])	
  Null_Simulat_rare<-dataperm[order(dataperm[,"MaxLengthTL"]),]		
  for	(i	in	1:NoS)
  {
    Null_Simulat_rare=Null_Simulat_rare[-1,]
    randORI[i,j]<-mean(Null_Simulat_rare[,"oriS"])
  }
}
##Calculating	median	and	quantiles	from	the	null	models
Null_FOri=	matrix(NA,nrow=NoS,ncol=3)
for	(i	in	1:NoS)
{
  Null_FOri[i,1]=median(randORI[i,])		
  Null_FOri[i,2]=quantile(randORI[i,],probs=0.025)
  Null_FOri[i,3]=quantile(randORI[i,],probs=0.975)		
}
##############End	of	for#############
#Plot	regional	species	loss	scenarios	- FOri
Spp_erosion<-c(1:NoS)

plot(Spp_erosion,Null_FOri[,1],type="l",xlim=c(0,NoS),xlab="Regional	Species	loss	
(%)",ylab="FOri",font.lab=2,cex.lab=1.4,cex.axis=1.2,col="gray",pch=16,cex=1.5,xaxt="n
",ylim=c(min(Null_FOri),max(Null_FOri)))
axis(side=1,	at=c(0,NoS/4,NoS/2,NoS/1.333,NoS),	
     labels=c(0,25,50,75,100),line=F,tick=-0.3,cex.axis=1.2,mgp=c(3,1,0))
polygon(c(1:NoS,rev(1:NoS)),c(Null_FOri[,2],rev(Null_FOri[,3])),col	=	"grey88",	border	
        =	F)
points(Null_FOri[,1],col="gray",cex=1.6,type="l",lty=1,lwd=4)
points(FOri_Simulat_rare,col="black",type="l",lty=1,lwd=4)
points(FOri_Simulat_comm,col="black",type="l",lty=3,lwd=4)
########################################
########################################

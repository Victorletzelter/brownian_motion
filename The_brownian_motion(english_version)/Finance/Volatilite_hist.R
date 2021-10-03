data<-read.csv(file='/Users/victorletzelter/Downloads/AMZN-2.csv',header=TRUE)

evolution_volatilite<-function(data,T=20,D=60){ 
#data : contient les données 
#T : nombre de jours de la fenêtre (cf fonction vol)
#D : le nombre de dates que l'on souhaite faire apparaître sur le graphique

TS<-ts(data)
TSD<-as.data.frame(TS)

vol<-function(t,T=20){
  
  #t : date à laquelle la volatilité est calculée
  #T : nombre de jours de la fenêtre
  
  plot(TSD$Date,TSD$Close,'l',main="Cours de l'action AMZN",xlab="Dates en jour",ylab="Prix de cloture (en USD)")
  Val<-TSD$Close
  #Val<-rev(Val) : On doit inverser l'ordre de la série temporelles si les données les plus récentes sont données en premier
  Val<-Val[(length(Val)-T-D+t):(length(Val)-D+t)]
  dln<-diff(log(Val))
  
  #hist(dln,prob=T,nclass=10,main="Histogramme dlnSt AMZN",col="#99CCFF",xlab="dlnSt",ylab="Fréquence") 
  #curve(dnorm(x, mean=mean(dln),sd=sqrt(var(dln))),add=T)
  
  #Estimation des paramètres
  
  dt=1/252 #On en prend pas dt=1 mais dt=1/252, il s'agit d'une convention : on exprime le temps en année (le trading est ouvert 252 jours par an)
  variance<-var(dln)
  moy<-mean(dln)
  sigma=sqrt(variance/dt)
  mu<-(moy/dt)+(sigma^2)/2
  return(sigma)
}

T<-seq(1,D,1)
y<-rep(0,D)
for (i in 1:D){
  y[i]=vol(i)
}

plot(T,y,'l',main='Volatilité historique (fenêtre : T jours) - AMZN',xlab='Jours',ylab='Volatilité')
}

evolution_volatilite(data)


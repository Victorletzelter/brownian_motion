install.packages("xlsx")
install.packages("readxl")
library(xlsx)
library(readxl)


file ="/Users/victorletzelter/Desktop/coord-tracker.xlsx"

data<-read_xlsx(file,col_names=TRUE)
data<-data*0.2181*10^(-3) #Convertion des coordonées en mètres
P1X<-data$P1X
P1Y<-data$P1Y
P2X<-data$P2X
P2Y<-data$P2Y
P3X<-data$P3X
P3Y<-data$P3Y
P4X<-data$P4X
P4Y<-data$P4Y
P5X<-data$P5X
P5Y<-data$P5Y
P6X<-data$P6X
P6Y<-data$P6Y
P7X<-data$P7X
P7Y<-data$P7Y
P8X<-data$P8X
P8Y<-data$P8Y
P9X<-data$P9X
P9Y<-data$P9Y
P10X<-data$P10X
P10Y<-data$P10Y
P11X<-data$P11X
P11Y<-data$P11Y
P12X<-data$P12Y
P12Y<-data$P12Y
P13X<-data$P13X
P13Y<-data$P13Y
P14X<-data$P14X
P14Y<-data$P14Y
P15X<-data$P15X
P15Y<-data$P15Y
P16X<-data$P16X
P16Y<-data$P16Y
P17X<-data$P17X
P17Y<-data$P17Y
P18X<-data$P18X
P18Y<-data$P18Y
P19X<-data$P19X
P19Y<-data$P19Y
P20X<-data$P20X
P20Y<-data$P20Y
P21X<-data$P21X
P21Y<-data$P21Y
P22X<-data$P22X
P22Y<-data$P22Y
P23X<-data$P23X
P23Y<-data$P23Y
P24X<-data$P24X
P24Y<-data$P24Y
P25X<-data$P25X
P25Y<-data$P25Y

l<-cbind(P1X,P1Y,P2X,P2Y,P3X,P3Y,P4X,P4Y,P5X,P5Y,P6X,P6Y,P7X,P7Y,P8X,P8Y,P9X,P9Y,P10X,P10Y,P11X,P11Y,P12X,P12Y,P13X,P13Y,P14X,P14Y,P15X,P15Y,P16X,P16Y,P17X,P17Y,P18X,P18Y,P19X,P19Y,P20X,P20Y,P21X,P21Y,P22X,P22Y,P23X,P23Y,P24X,P24Y,P25X,P25Y)
#On introduit l pour faciliter les manipulations de données
n=length(P1X) #n=1256 imges ici.
#n=375 #On diminue n pour se placer dans la première plage de temps 
#l<-l[1:n,]

for (j in 1:50){ #On standardise le mouvement brownien
  for (i in 2:n){ #Commencer à 2 pour ne par perturber le premier élément
  l[i,j]<-l[i,j]-l[1,j]
  }
  l[1,j]<-0 
}


t<-matrix(0,n,1) #Contient les instants
for (j in 0:n-1){
  t[j+1]<-(1/25)*j #fréquence du pointage : 25 images par seconde
}

#tbis<-t[0:15*25,1]


Moy_simple_x<-matrix(0,n,1) #Contiendra <x>_{t} à chaque temps t
Moy_simple_y<-matrix(0,n,1) #Contiendra <y>_{t} à chaque temps t

V_x<-matrix(0,n,25)
for (j in 1:25){
  V_x[,j]<-l[,2*j-1]
}

V_y<-matrix(0,n,25)
for (j in 1:25){
  V_y[,j]<-l[,2*j]
}

for (i in 1:n){
  Moy_simple_x[i,1]=mean(V_x[i,])
  Moy_simple_y[i,1]=mean(V_y[i,])
}

centrer<-function(l){ 
  n<-dim(l)[1]
  p<-dim(l)[2]
 for (i in 1:n){
    for (j in 1:(p/2)){
    l[i,2*j-1]<-l[i,2*j-1]-Moy_simple_x[i,1]
    l[i,2*j]<-l[i,2*j]-Moy_simple_y[i,1]
    }
 }
  return(l)
} #Toutes les données de l seront tq qqs t <x>_{t}=0 et <y>_{t}=0

l<-centrer(l)
 #Prise en compte du Drift : re centrage des données.


mp<-function(l1,l2){
  n=length(l1) #On suppose que length(l1)=n=length(l2)
  res=matrix(0,n,1) #Contiendra les valeurs moyennes à chaque instant
  for (i in 1:n){
    res[i,1]<-l1[i]^(2)+l2[i]^(2) #r2=x2+y2
  }
  return(res)
} #Calcul de la liste r^2(1),...r^2(n) à partir de (x(1),...,x(n)) et (y(1),.,y(n))

R<-matrix(0,n,25) #R[,1]<-mp(R1X,R1Y) : On obtient (r^2(1),...r^(t)) pour chaque particule 
for (j in 1:25){
  R[,j]<-mp(l[,2*j-1],l[,2*j])
}

Moy<-matrix(0,n,1) #Contiendra <r^2> à chaque temps t
for (i in 1:n){
  Moy[i]=mean(R[i,])
}

#t<-t[1:30*25,]
#Moy<-Moy[1:30*25]

#reglin
#Regression linéaire
reg<-lm(Moy ~ t)
a1<-reg$coefficients[1] #Ord à l'origine 
b1<-reg$coefficients[2] #Pente
plot(t,Moy,'l',ylab='<r^2>')
abline(a1,b1,col='red')
d<-2
D<-b1/2*d #d : nombre de dimensions
#Possibilité de réaliser une regression linéaire

D<-matrix(0,n,1)
c=0 #Donne la taille de D (sans compter les NA)
S=0
d=2 #Nombre de dimensions considérées ici
for (i in 2:n){
  D[i,1]=Moy[i,1]/(2*d*t[i])
  if (is.na(Moy[i,1])==FALSE){
    c<-c+1
    S=S+D[i,1]
  }
}

Dmean<-S/c #Valeur moyenne de D pour être plus précis

#D=(pos_moy_t-pos_moy_0)/(2*d*(t-t0))
#D=Moy[5,1]/(2*d*tp)


#calcul du nombre d'Avogadro
Tp<-294.75 #en K
a<-0.1*10**(-6) #en mètres
vis<-1.10**(-3) #visosité dynamique du fluide à 21.6 degrés celcius
Rgp<-8.314
Na<-(Rgp*Tp)/(6*pi*vis*a*Dmean)


#Vérification des propriétés du mouvement Brownien :

#1/ qqs t,s s<t, sigma(B_t-B_s) - N(0,sigma^2*(t-s))
#Par exemple pour t-s=1/25 s : 
plot(hist(diff(l[,1]))) #équivalent à l'étude réalisée sur plusieurs particules
qqnorm(diff(l[,1])) #idem

#2/ ssq N entier naturel, qqs t_0,...t,_N, les B_(t_i)-B_(t_i-1) sont indep
#Montrons les (B_(t)-B_(t-1)) sont indep
L<-as.ts(l[,1])
D<-diff(L)
plot(acf(D),main='Autocorrelation pour la série différentiée')
#On en déduit bien que les incréments sont indépendants     

#3/ Pour w dans Omega, t -> B_(t)(w) est p.s Continue
#Cette propriété peut se vérifier visuellement par la commande
plot(l[,1],l[,2],'l')

###----------------------------------------- Simulation d’un mvt Brownien en 1D -----------------####
Bo=7.723619e-05 #on choisit la valeur du MB à l'instant t=0 pour faire correspondre à la position initiale d'une particule
t=seq(0,1255)
### Simulation des accroissements suivant une loi normale multiplié par le coefficient de diffusion observé pour nos valeurs 
B.acc = 1.115545e-07*rnorm(1256)
### Simulation d’une trajectoire en sommant les accroissements
B.sim = Bo+cumsum(B.acc)

plot(B.sim,type="l",xlab="Temps")
D<-data.frame(B.sim,P1X)


plot(t,B.sim,ylim = range(c(B.sim, P1X)),type="l",col="red")

lines(t,P1X, type="l")
##----------------------------------------------simulation d'un MB en 2D--------------------------------------------#
Bo=-3.415556e-05#valeur initiale

### Simulation des accroissements
B.acc = 1.115545e-07*rnorm(1256)
### Simulation d’une trajectoire en sommant les accroissements
B.sim2 = Bo+cumsum(B.acc)


## L’option type="l" relie les points entre eux.
ggplot()+ geom_point(data = D)
plot(D)


plot(B.sim,B.sim2,ylim = range(c(B.sim2, P1Y)),xlim = range(c(B.sim, P1X)),type="l",col="red")
lines(P1X,P1Y, type="l")




### Simulation d’un mvt Brownien ### discretisation du temps

Bo=0
t=seq(0,10000)
### Simulation des accroissements
B.acc = rnorm(10001)
### Simulation d’une trajectoire > on a un MB B.sim 
B.sim = Bo+cumsum(B.acc)


#on veut un MB géométrique maintenant :
sigma = 0.001
mu=0.0001

#MB simulé, on peut en faire plusieurs comme celui ci 
S=exp((mu-sigma^2/2)*t+sigma*B.sim)


#esperance - prédiction :
pred=exp(mu*t)

plot(S,type="l",xlab="En rouge la prédiction en noir une trajectoire",ylab="Axe des ordonnées")
lines(pred,col="red")
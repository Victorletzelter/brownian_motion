import numpy as np
import scipy.stats as si
import sympy as sy
from sympy.stats import Normal, cdf
from sympy import init_printing
init_printing()
from matplotlib import pyplot as plt


##calcul du prix d'une option call par Black-Scholes

def euro_vanilla_call(S, K, T, r, sigma):

    #S: prix comptant
    #K: prix d'exercice
    #T: Temps restant avant l’expiration de l’option, en pourcentage d’une année
    #r: taux d'intérêt
    #sigma: volatilité implicite

    d1 = (np.log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
    d2 = (np.log(S / K) + (r - 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))

    call = (S * si.norm.cdf(d1, 0.0, 1.0) - K * np.exp(-r * T) * si.norm.cdf(d2, 0.0, 1.0))

    return call


##la fonction précision calcul la volatilité implicite avec une précision eps

def precision(eps,C,S,T,K,r):
    #mêmes paramètres sauf que ici eps correspond à la précision et C est le prix réel de l'option
    sigma_min = 0
    sigma_max=1
    sigma = 0.5
    C_BS = euro_vanilla_call(S, K, T, r, sigma)
    n=0
    while abs(C_BS-C)>eps and n<1000:
        sigma = (sigma_min+sigma_max)/2
        C_BS = euro_vanilla_call(S, K, T, r, sigma)
        if C_BS<C:
            sigma_min=sigma
        if C_BS>C:
            sigma_max=sigma
        n+=1
    return (sigma)

##application :
""""
On va calculer le prix d'une option Call et sa volatilité implicite.
On a choisi une option sur l'entreprise Amazon.

On a utilisé les données sur le site web : https://www.barchart.com/stocks/quotes/AMZN/options
St =3138.38
T= 2/252 attention il ne s'agit pas le temps exact, d'ou l'erreur
sigma_impl =36.38%
K=3090
C=64.15
r=0.01
"""
#on recalcule le prix :
prix = euro_vanilla_call(3138.38, 3090, 0.007, 0.01, 0.3460)
#on recalcule la volatilité implicite avec ces parametres ;
vol_impl = precision(0.0001,62.73,3138,0.007,3090,0.01) #on extrait avec petite erreur
#On retrouve les bons résultats pour le call de l'action Amazon.
print(vol_impl)
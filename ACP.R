# install.packages("PCAmixdata")
require(PCAmixdata)
#help(PCAmix)
#----------------------------------------
# Mise en oeuvre de l’ACP
#----------------------------------------
# On enlève la variable à expliquer PctBf

res <- PCAmix(donneesProjet[,2:14], graph = FALSE)
res
round(res$eig,digit=2) # permet d’afficher les valeurs propres et les pourcentages
# de variances expliquees par chaque axe
# On applique le critère de Kaiser
# eigen value > 1
# Mesure globale de qualité
# On retiendra dim1, dim2, dim3 totalité de la part de variance expliqué 
# 61.93 + 11.20 + 6.84 = 79.97 %

# Graphique de l’ebouli des valeurs propres
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)

#permet d’afficher la fenetre d’aide de la commande "plot.PCA"
plot(res,axes=c(1,2),choice="ind", label = TRUE) # on retrouve ici le graphique des individus (plan 1-2)
plot(res,axes=c(1,2),choice="cor") # on retrouve ici le cercle des corrélations
# des variables (plan 1-2)
plot(res,axes=c(1,3),choice="ind", label = TRUE)
plot(res,axes=c(1,3),choice="cor", label = TRUE)
plot(res,axes=c(1,2),choice="sqload") # on retrouve ici le graphique des "square loadings" (plan 1-2)

#--------------------------------------------------------------------
# Sorties numeriques pour les individus et es variables
#--------------------------------------------------------------------
res$ind # permet d’afficher l’ensemble des sorties numeriques associees aux individus :
# coordonnees, contributions, cosinus carres
round(res$ind$cos2,digit=3) # uniquement les cosinus carres
res$quanti # permet d’afficher l’ensemble des sorties numeriques associees aux variables :
# coordonnees, contributions, cosinus carres
round(res$quanti$cos2,digit=3) # uniquement les cosinus carres
# Confirme que les variables sont corrélées
# Variable âge intéressante ??
# Y peut-être reconstruit ??
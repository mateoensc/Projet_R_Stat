
require(PCAmixdata)
# Chargement du package corrplot
library(corrplot)


# Matrice des corrélations
Matricecor <- cor(donneesProjet[,2:14])
# Graphique de la matrice des corrélations
corrplot(Matricecor,type="upper", order="hclust",tl.col = "black",tl.srt = 45)
#----------------------------------------
# Mise en oeuvre de l’ACP
#----------------------------------------
# On enlève la variable à expliquer PctBf
acp <- PCAmix(donneesProjet[,2:14], graph = FALSE, ndim = 13)
acp
round(acp$eig,digit=2) # permet d’afficher les valeurs propres et les pourcentages
# de variances expliquees par chaque axe
# On applique le critère de Kaiser
# eigen value > 1
# Mesure globale de qualité
# On retiendra dim1, dim2, dim3 totalité de la part de variance expliqué 
# 61.93 + 11.20 + 6.84 = 79.97 %

# Graphique de l’ebouli des valeurs propacp
barplot(acp$eig[,1],main="Eigenvalues",names.arg=1:nrow(acp$eig))
abline(h=1,col=2,lwd=2)

#permet d’afficher la fenetre d’aide de la commande "plot.PCA"
plot(acp,axes=c(1,2),choice="ind", label = TRUE) # on retrouve ici le graphique des individus (plan 1-2)
plot(acp,axes=c(1,2),choice="cor") # on retrouve ici le cercle des corrélations
# des variables (plan 1-2)
plot(acp,axes=c(2,3),choice="ind", label = TRUE)
plot(acp,axes=c(2,3),choice="cor", label = TRUE)
plot(acp,axes=c(1,2),choice="sqload") # on retrouve ici le graphique des "square loadings" (plan 1-2)

#--------------------------------------------------------------------
# Sorties numeriques pour les individus et les variables
#--------------------------------------------------------------------
acp$ind # permet d’afficher l’ensemble des sorties numeriques associees aux individus :
# coordonnees, contributions, cosinus caracp
round(acp$ind$cos2,digit=3) # uniquement les cosinus caracp
acp$quanti # permet d’afficher l’ensemble des sorties numeriques associees aux variables :
# coordonnees, contributions, cosinus caracp
round(acp$quanti$cos2,digit=3) # uniquement les cosinus caracp
# Confirme que les variables sont corrélées
# Variable âge intéacpsante ??
# Y peut-être reconstruit ??

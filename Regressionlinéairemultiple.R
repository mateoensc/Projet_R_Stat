# Test de la régression avec deux variables fortement corrélées
y = donneesProjet$Pct.BF # Pct Bf

test = lm(y~donneesProjet$Weight)
summary(test)
# 0.9326905 valeur de corrélation entre Hip et Weight
test2 = lm(y~donneesProjet$Weight+donneesProjet$Hip)
summary(test2)

# Test de la regression linéaire sur les trois premières composantes
composantes = res$ind$coord
res3 <- lm(y~composantes[,1]) # Par rapport à la première composante
summary(res3)
res4 <- lm(y~composantes[,1]+composantes[,2]+composantes[,3])
summary(res4)
res5 <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,5])
summary(res5)
# On observe que rajouter la 4eme composante permet d'augmenter le R ajusté
# Et donc de gagner en précision du modèle

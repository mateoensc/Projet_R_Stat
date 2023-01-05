# Test de la régression avec deux variables fortement corrélées
y = donneesProjet$Pct.BF # Pct Bf

test = lm(y~donneesProjet$Weight)
summary(test)
# 0.9326905 valeur de corrélation entre Hip et Weight
test2 = lm(y~donneesProjet$Weight+donneesProjet$Hip)
summary(test2)

# Test de la regression linéaire sur les trois premières composantes
composantes = res$ind$coord # Ici charger l'ACP
res3 <- lm(y~composantes[,1]) # Par rapport à la première composante
summary(res3)
res4 <- lm(y~composantes[,1]+composantes[,2]+composantes[,3])
summary(res4)
res5 <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,5])
summary(res5)
# On observe que rajouter la 4eme composante permet d'augmenter le R ajusté
# Et donc de gagner en précision du modèle
resF <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4])
summary(resF)
# On observe que rajouter la 4eme composante permet d'augmenter le R ajusté
# Et donc de gagner en précision du modèle

# Etude des résidus
par(mfrow=c(1,2))
plot(resF$fitted,resF$residuals)
abline(h=0,col=2)
plot(resF$fitted,y)
abline(0,1,col=2)



shapiro.test(resF$residuals)


# Affinage du modèle avec les composantes principales
matYX<-data.frame(y,composantes[,1],composantes[,2],composantes[,3],composantes[,4],composantes[,5])
res <- lm(y~1,data=matYX) 
step(res,~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,5])  
step(res,~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4],trace=F)
# Selectionne bien les 4 composantes principales
res <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,5],data=matYX) # modele initial complet
step(res)  
# Seule la 5eme est enlevée
# Selection des variables sans les composantes principales
matYX2<-data.frame(y,donneesProjet[,2],donneesProjet[,3],donneesProjet[,4],donneesProjet[,5],donneesProjet[,6],donneesProjet[,7],donneesProjet[,8],donneesProjet[,9],donneesProjet[,10],donneesProjet[,11],donneesProjet[,12],donneesProjet[,13],donneesProjet[,14])
res <- lm(y~1,data=matYX2) 
step(res,~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14])  
step(res,~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14],trace=F)
# Ajout des variables avec le modèle sans variable explicative
# Weight, Abdomen, Wrist, Bicep, Age, Thigh
res_selec <- lm(y~donneesProjet$Weight+donneesProjet$Abdomen+donneesProjet$Wrist+donneesProjet$Bicep+donneesProjet$Age+donneesProjet$Thigh)
summary(res_selec)
res <- lm(y~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14],data=matYX) # modele initial complet
step(res)  
# Age, Height, Neck, Hip, Thigh, Wrist, Forearm
summary(res)

# on enlève âge qui paraît moins corrélé avec les autres
res<-lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4],data=matYX)
drop1(res)



bartlett.test(donneesProjet[,1]~donneesProjet[,2])

# Variable à expliquer
y = donneesProjet$Pct.BF # Pct Bf
composantes = acp$ind$coord # Ici charger l'ACP
# Test de la régression avec deux variables fortement corrélées
test = lm(y~donneesProjet$Weight)
summary(test)
# 0.9326905 valeur de corrélation entre Hip et Weight
test2 = lm(y~donneesProjet$Weight+donneesProjet$Hip)
summary(test2)

# Test de la regression linéaire sur les trois premières composantes

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

# Etude des résidus
par(mfrow=c(1,2))
plot(resF$fitted,resF$residuals)
abline(h=0,col=2)
plot(resF$fitted,y)
abline(0,1,col=2)



shapiro.test(resF$residuals) # On ne rejette pas H0


# Affinage du modèle avec les composantes principales
matYX<-data.frame(y,composantes[,1],composantes[,2],composantes[,3],composantes[,4],composantes[,5],composantes[,6]+composantes[,7]+composantes[,8]+composantes[,9]+composantes[,10]+composantes[,11]+composantes[,12]+composantes[,13]) # modele initial complet
res <- lm(y~1,data=matYX) 
step(res,~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,5]+composantes[,6]+composantes[,7]+composantes[,8]+composantes[,9]+composantes[,10]+composantes[,11]+composantes[,12]+composantes[,13])  

# Selectionne 1,2,3,4,12,7,6,10,8,11
res <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,5]+composantes[,6]+composantes[,7]+composantes[,8]+composantes[,9]+composantes[,10]+composantes[,11]+composantes[,12]+composantes[,13],data=matYX) # modele initial complet
step(res)  
# Selectionne 1,2,3,4,6,7,8,10,11,12
# Selection des variables sans les composantes principales
matYX2<-data.frame(y,donneesProjet[,2],donneesProjet[,3],donneesProjet[,4],donneesProjet[,5],donneesProjet[,6],donneesProjet[,7],donneesProjet[,8],donneesProjet[,9],donneesProjet[,10],donneesProjet[,11],donneesProjet[,12],donneesProjet[,13],donneesProjet[,14])
res <- lm(y~1,data=matYX2) 
step(res,~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14])  
step(res,~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14],trace=F)
# Ajout des variables avec le modèle sans variable explicative
# Weight, Abdomen, Wrist, Bicep, Age, Thigh

res <- lm(y~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14],data=matYX) # modele initial complet
step(res)  
# Age, Height, Neck, Hip, Thigh, Wrist, Forearm
summary(res)

# Comparaison entre deux modèles celui avec composantes principales et celui avec variables explicatives
# Avec ACP 
res_acp <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4]+composantes[,6]+composantes[,7]+composantes[,8]+composantes[,10]+composantes[,11]+composantes[,12])
summary(res_acp)
# Variables explicatives
res_selec <- lm(y~donneesProjet$Weight+donneesProjet$Abdomen+donneesProjet$Wrist+donneesProjet$Bicep+donneesProjet$Age+donneesProjet$Thigh)
summary(res_selec)


bartlett.test(donneesProjet[,1]~donneesProjet[,2])

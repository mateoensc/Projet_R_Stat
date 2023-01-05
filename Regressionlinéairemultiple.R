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

par(mfrow=c(1,1))
plot(rstudent(resF),ylab="res. student.",ylim=c(-3.5,3.5)) 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

shapiro.test(resF$residuals)


# Affinage du modèle
matYX<-data.frame(y,composantes[,1],composantes[,2],composantes[,3],composantes[,4])
res <- lm(y~1,data=matYX) 
step(res,~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4])  
step(res,~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4],trace=F)

res <- lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4],data=matYX) # modele initial complet
step(res)  

matYX2<-data.frame(y,donneesProjet[,2],donneesProjet[,3],donneesProjet[,4],donneesProjet[,5],donneesProjet[,6],donneesProjet[,7],donneesProjet[,8],donneesProjet[,9],donneesProjet[,10],donneesProjet[,11],donneesProjet[,12],donneesProjet[,13],donneesProjet[,14])
res <- lm(y~1,data=matYX2) 
step(res,~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14])  
step(res,~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14],trace=F)

res <- lm(y~donneesProjet[,2]+donneesProjet[,3]+donneesProjet[,4]+donneesProjet[,5]+donneesProjet[,6]+donneesProjet[,7]+donneesProjet[,8]+donneesProjet[,9]+donneesProjet[,10]+donneesProjet[,11]+donneesProjet[,12]+donneesProjet[,13]+donneesProjet[,14],data=matYX) # modele initial complet
step(res)  
summary(res)

# on enlève âge qui paraît moins corrélé avec les autres
res<-lm(y~composantes[,1]+composantes[,2]+composantes[,3]+composantes[,4],data=matYX)
drop1(res)



bartlett.test(donneesProjet[,1]~donneesProjet[,2])

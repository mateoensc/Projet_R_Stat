data <- load("donneesProjet2A.RData")
summary(donneesProjet)
# install.packages("PCAmixdata")
require(PCAmixdata)
#help(PCAmix)
##
# Une première visualisation des variables quantitatives
# Tableau statistique des variables 

summary(donneesProjet$Age)
summary(donneesProjet$Weight)
summary(donneesProjet$Height)
summary(donneesProjet$Neck)
summary(donneesProjet$Chest)
summary(donneesProjet$Abdomen)
summary(donneesProjet$Hip)
summary(donneesProjet$Thigh)
summary(donneesProjet$Knee)
summary(donneesProjet$Ankle)
summary(donneesProjet$Bicep)
summary(donneesProjet$Forearm)
summary(donneesProjet$Wrist)
dev.off()
layout(matrix(1:2,1,2))
boxplot(donneesProjet$Pct.BF,xlab="Pct.BF");
boxplot(donneesProjet$Age,xlab="Age");
layout(matrix(1:2,1,2))

boxplot(donneesProjet$Weight,xlab="Weight en livres");
boxplot(donneesProjet$Height,xlab="Height en pouces");
layout(matrix(1:2,1,2))

boxplot(donneesProjet$Neck,xlab="Neck");
boxplot(donneesProjet$Chest,xlab="Chest")
layout(matrix(1:2,1,2))

boxplot(donneesProjet$Abdomen,xlab="Abdomen");
boxplot(donneesProjet$Hip,xlab="Hip");
layout(matrix(1:2,1,2))

boxplot(donneesProjet$Thigh,xlab="Thigh");
boxplot(donneesProjet$Knee,xlab="Knee");
layout(matrix(1:2,1,2))

boxplot(donneesProjet$Ankle,xlab="Ankle");
boxplot(donneesProjet$Bicep,xlab="Bicep");
layout(matrix(1:2,1,2))

boxplot(donneesProjet$Forearm,xlab="Forearm");
boxplot(donneesProjet$Wrist,xlab="Wrist")


#####
plot(donneesProjet[,1:14])
plot(donneesProjet[,1:2])
hist(donneesProjet$Age)
hist(donneesProjet$Weight)

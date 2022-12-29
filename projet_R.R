data <- load("donneesProjet2A.RData")
summary(donneesProjet)

##
# Une première visualisation des variables quantitatives
# Tableau statistique des variables 

summarydb <- data.frame();
for (var in 1:14){
  name <- colnames(donneesProjet[var])
  summary <-  array(summary(donneesProjet[[var]]))
  var_sd <- data.frame(Coeffvariation = (sd(donneesProjet[[var]])/mean(donneesProjet[[var]]))*100,ecart_type = sd(donneesProjet[[var]]))
  summary <- data.frame(Min = summary[1],FirstQuarter = summary[2],Median = summary[3],Mean = summary[4], ThirdQuarter = summary[5],Max = summary[6])
  ans <- data.frame(name,summary,var_sd)
  summarydb <- rbind(summarydb,ans)
}
dev.off()
# On se permet ici de réaliser les boxplots car il y'a un grand nombre de modalités par variables
# EN utilisant le package dplyr on obtient le nombre de modalités par variable
occurences <- data.frame();
for (var in 1:14){
  name <- colnames(donneesProjet[var])
  numofoccurence <- dplyr::n_distinct(donneesProjet[var])
  ans <- data.frame(name,numofoccurence)
  occurences <- rbind(occurences,ans)
}

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


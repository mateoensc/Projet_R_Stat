data <- load("donneesProjet2A.RData")
summary(donneesProjet)
# install.packages("PCAmixdata")
require(PCAmixdata)
#help(PCAmix)
##
# Une première visualisation des variables quantitatives
# 
plot(donneesProjet[,1:14])
plot(donneesProjet[,1:2])
hist(donneesProjet$Age)
hist(donneesProjet$Weight)
